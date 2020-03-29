(ns lab4-7.core
  (:gen-class)
  (:use [clojure.data.csv])
  (:require [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn readTSV [name]
  (map #(str/split % #"\t")
       (str/split (slurp name) #"\r\n")))

(defn readCSV [name]
  (with-open [reader (io/reader name)]
    (doall
      (csv/read-csv reader))))

(defn data->maps [head & lines]
  (map #(zipmap (map keyword head) %1) lines))

(defn makeTableTSV [name]
  (apply data->maps (readTSV name)))

(defn makeTableCSV [name]
  (apply data->maps (readCSV name)))

(defn getKeysForPrint [numTable table]
  (keys (first (nth table numTable))))

(defn getValsForPrint [numTable numRow table]
  (vals (nth (nth table numTable) numRow)))

(defn getCountOfAllKeys [table & countOfKeys]
  (cond
    (empty? table)
      (first countOfKeys)
    (empty? countOfKeys)
      (getCountOfAllKeys (next table) (count (keys (first (first table)))))
    :else (getCountOfAllKeys (next table)
                             (+ (count (keys (first (first table)))) (first countOfKeys)))))

(defn printTable [table]
  (loop [l 0]
    (when (< l (count table))
      (loop [k 0]
        (when (< k (count (first (nth table l))))
          (print (format "%40s| " (name (nth (getKeysForPrint l table) k))))
          (recur (+ k 1))))
      (recur (+ l 1))))
  (println (str "\n" (str/join "" (take (* 42 (getCountOfAllKeys table)) (repeat "-")))))
  (loop [i 0]
    (when (< i (count (first table)))
      (loop [j 0]
        (when (< j (count table))
          (loop [z 0]
            (when (< z (count (nth (nth table j) i)))
              (print (format "%40s| " (nth (getValsForPrint j i table) z)))
              (recur (+ z 1))))
          (recur (+ j 1))))
      (println)
      (recur (+ i 1)))))

(defn checkFormat [name]
  (let [formatFile (str/split name #"\.")]
    (if (= (str (nth formatFile 1)) "csv") true false)))

(defn getFile [name]
  (if (checkFormat name) (makeTableCSV name) (makeTableTSV name)))

(defn compareForStringNumber [a b]
  (let [alterA (str a)
        alterB (str b)]
    (cond
      (every? #(Character/isDigit %) alterA)
      (compare (read-string alterA) (read-string alterB))
      :else (compare alterA alterB))))

(defn containsForMap [m1 m2]
  (and (every? (set (keys m1)) (keys m2))
       (every? #(= (m1 %) (m2 %)) (keys m2))))

(defn getLastIndex [coll word]
  (if (not= -1 (.indexOf (reverse coll) word))
    (- (count coll) (.indexOf (reverse coll) word) 1) -1))

(defn getColumn [columnName file]
  (map #(select-keys % [(keyword columnName)]) file))

(defn mapCompare [a b & keysOfMap]
  (cond
    (and (empty? keysOfMap)
         (= 0 (compareForStringNumber (get a (first (keys a))) (get b (first (keys b)))))
         (= 1 (count (keys a))))
    0
    (and (empty? keysOfMap)
         (not= 0 (compareForStringNumber (get a (first (keys a))) (get b (first (keys b)))))
         (= 1 (count (keys a))))
    (compareForStringNumber (get a (first (first keysOfMap))) (get b (first (first keysOfMap))))
    (and (empty? keysOfMap)
         (= 0 (compareForStringNumber (get a (first (keys a))) (get b (first (keys b))))))
    (mapCompare a b (next (keys a)))
    (and (= 1 (count (first keysOfMap)))
         (= 0 (compareForStringNumber (get a (first (first keysOfMap))) (get b (first (first keysOfMap))))))
    0
    (and (> (count (first keysOfMap)) 1)
         (= 0 (compareForStringNumber (get a (first (first keysOfMap))) (get b (first (first keysOfMap))))))
    (mapCompare a b (next (first keysOfMap)))
    (empty? keysOfMap)
    (compareForStringNumber (get a (first (keys a))) (get b (first (keys b))))
    :else
    (compareForStringNumber (get a (first (first keysOfMap))) (get b (first (first keysOfMap))))))

(defn mergeStep [l r ord columns]
  (cond (empty? l) r
        (empty? r) l
        (and (empty? columns) (ord (mapCompare (first l) (first r))))
        (cons (first l) (mergeStep (next l) r ord columns))
        (and (empty? columns) (not (ord (mapCompare (first l) (first r)))))
        (cons (first r) (mergeStep l (next r) ord columns))
        (ord (mapCompare (first l) (first r) columns))
        (cons (first l) (mergeStep (next l) r ord columns))
        :else
        (cons (first r) (mergeStep l (next r) ord columns))))


(defn mergeSort
  ([data] (mergeSort data neg?))
  ([data ord] (mergeSort data ord []))
  ([data ord columns]
   (if (< (count data) 2)
     data
     (mergeStep
       (mergeSort (first (split-at (/ (count data) 2) data)) ord columns)
       (mergeSort (second (split-at (/ (count data) 2) data)) ord columns)
       ord columns))))

(defn checkOrder [words commands]
  (cond
    (not= -1 (.indexOf (map #(str/upper-case %) words) (nth commands 10))) "pos?"
    :else "neg?"))

(defn getColumnsOrderBy [words indexOfBy]
  (map #(keyword %) (str/split (nth words (inc indexOfBy)) #",")))

(defn orderBy [table words commands]
  (let [indexOfOrder (.indexOf (map #(str/upper-case %) words) (nth commands 4))]
    (if (= -1 indexOfOrder)
      table (mergeSort table (resolve (symbol (checkOrder words commands)))
                       (getColumnsOrderBy words (inc indexOfOrder))))))

(defn countFunc [table]
    (list (hash-map :count (count (filter #(not (every? (fn [value] (= "" value)) (vals %))) table)))))

(defn sumForReduceMap [& body]
  (let [firstValueOfMap (get (first body) (first (keys (first body))))
        secondValueOfMap (get (second body) (first (keys (second body))))]
    (cond
      (= "" (first (vals (second body))))
      (first body)
      (and (number? firstValueOfMap) (number? secondValueOfMap))
      (merge-with + (first body) (second body))
      (and (string? firstValueOfMap) (string? secondValueOfMap))
      (update (first body) (first (keys (first body)))
              #(+ (read-string %) (read-string secondValueOfMap)))
      :else
      (update (first body) (first (keys (first body)))
              #(+ % (read-string secondValueOfMap))))))

(defn avgFunc [column]
  (let [sumOfMaps (if (= 1 (count column))
                    (if (number? (first (vals (first column))))
                      (first column) (hash-map (first (keys (first column)))
                                               (read-string (first (vals (first column))))))
                    (reduce sumForReduceMap column))
        numberOfElementsMap (countFunc column)
        numberOfElements (get (first numberOfElementsMap) (first (keys (first numberOfElementsMap))))
        avgValueMap (update sumOfMaps (first (keys sumOfMaps)) / (double numberOfElements))]
    (list (set/rename-keys avgValueMap {(first (keys avgValueMap)) :avg}))))

(defn minForReduceMap [& body]
  (let [firstValueOfMap (get (first body) (first (keys (first body))))
        secondValueOfMap (get (second body) (first (keys (second body))))]
    (if (or (= "" (first (vals (second body))))
            (and (map? (first body))
                 (<= (compareForStringNumber firstValueOfMap secondValueOfMap) 0)))
      (first body) (second body))))

(defn minFunc [column]
  (let [minValueMap (if (= 1 (count column)) (first column) (reduce minForReduceMap column))]
    (list (set/rename-keys minValueMap {(first (keys minValueMap)) :min}))))

(defn parseConditions [words commands]
  (let [indexOfWhere (.indexOf (map #(str/upper-case %) words) (nth commands 2))
        lastIndexOfAnd (if (not= -1 (.indexOf (map #(str/upper-case %) (reverse words)) (nth commands 6)))
                         (- (count words) (.indexOf (map #(str/upper-case %) (reverse words)) (nth commands 6)) 1) -1)
        lastIndexOfOr (if (not= -1 (.indexOf (map #(str/upper-case %) (reverse words)) (nth commands 7)))
                        (- (count words) (.indexOf (map #(str/upper-case %) (reverse words)) (nth commands 7)) 1) -1)
        lastIndexOfNot (if (not= -1 (.indexOf (map #(str/upper-case %) (reverse words)) (nth commands 8)))
                         (- (count words) (.indexOf (map #(str/upper-case %) (reverse words)) (nth commands 8)) 1) -1)]
    (if (= -1 (max lastIndexOfAnd lastIndexOfOr lastIndexOfNot))
      (subvec (vec words) (+ 1 indexOfWhere) (+ 4 indexOfWhere))
      (subvec (vec words) (+ 1 indexOfWhere) (+ 4 (max lastIndexOfAnd lastIndexOfOr lastIndexOfNot))))))

(defn checkTruth [row condition]
  (let [indexOfValue (if (= -1 (.indexOf (keys row) (keyword (nth condition 0)))) 0 2)
        indexOfColumn (if (= 0 indexOfValue) 2 0)]
    (cond
      (and (= 0 (compare (nth condition 1) "="))
           (every? #(Character/isDigit %) (nth condition indexOfValue)))
      (if (= (read-string (nth condition indexOfValue))
             (read-string (get row (keyword (nth condition indexOfColumn))))) true false)
      (and (= 0 (compare (nth condition 1) ">"))
           (every? #(Character/isDigit %) (nth condition indexOfValue)))
      (if (= 0 indexOfValue)
        (if (> (compareForStringNumber (read-string (nth condition indexOfValue))
                                         (read-string (get row (keyword (nth condition indexOfColumn))))) 0)
          true false)
        (if (> (compareForStringNumber (read-string (get row (keyword (nth condition indexOfColumn))))
                                         (read-string (nth condition indexOfValue))) 0) true false))
      (and (= 0 (compare (nth condition 1) "=")))
      (if (= 0 (compare (get row (keyword (nth condition indexOfColumn)))
                        (subs (nth condition indexOfValue) (+ (str/index-of (nth condition indexOfValue) "\"") 1)
                              (str/last-index-of (nth condition indexOfValue) "\"")))) true false)
      (and (= 0 (compare (nth condition 1) ">")))
      (if (= 0 indexOfValue)
        (if (> (compare (subs (nth condition indexOfValue) (+ (str/index-of (nth condition indexOfValue) "\"") 1)
                              (str/last-index-of (nth condition indexOfValue) "\""))
                 (get row (keyword (nth condition indexOfColumn)))) 0) true false)
        (if (> (compare (get row (keyword (nth condition indexOfColumn)))
                          (subs (nth condition indexOfValue) (+ (str/index-of (nth condition indexOfValue) "\"") 1)
                                (str/last-index-of (nth condition indexOfValue) "\""))) 0) true false)))))

(defn andOrNotCondition [row condition commands logicOp]
  (let [indexOfNot (.indexOf (map #(str/upper-case %) condition) (nth commands 8))]
    (if (= (str/upper-case logicOp) (nth commands 6))
      (cond
        (= 9 (count condition))
        (and (not (checkTruth row (subvec (vec condition) 1 4)))
             (not (checkTruth row (subvec (vec condition) 6 9))))
        (and (= 8 (count condition)))
        (if (= 0 indexOfNot)
          (and (not (checkTruth row (subvec (vec condition) 1 4)))
               (checkTruth row (subvec (vec condition) 5 8)))
          (and (checkTruth row (subvec (vec condition) 0 3))
               (not (checkTruth row (subvec (vec condition) 5 8)))))
        (= 7 (count condition))
        (and (checkTruth row (subvec (vec condition) 0 3))
             (checkTruth row (subvec (vec condition) 4 7)))
        (= 6 (count condition))
        (and (not (checkTruth row (subvec (vec condition) 1 4))) (nth condition 5))
        (= 5 (count condition))
        (and (checkTruth row (subvec (vec condition) 0 3)) (nth condition 4)))
      (cond
        (= 9 (count condition))
        (or (not (checkTruth row (subvec (vec condition) 1 4)))
            (not (checkTruth row (subvec (vec condition) 6 9))))
        (and (= 8 (count condition)))
        (if (= 0 indexOfNot)
          (or (not (checkTruth row (subvec (vec condition) 1 4)))
              (checkTruth row (subvec (vec condition) 5 8)))
          (or (checkTruth row (subvec (vec condition) 0 3))
              (not (checkTruth row (subvec (vec condition) 5 8)))))
        (= 7 (count condition))
        (or (checkTruth row (subvec (vec condition) 0 3))
            (checkTruth row (subvec (vec condition) 4 7)))
        (= 6 (count condition))
        (or (not (checkTruth row (subvec (vec condition) 1 4))) (nth condition 5))
        (= 5 (count condition))
        (or (checkTruth row (subvec (vec condition) 0 3)) (nth condition 4))))))

(defn numberOfRepeated [coll x & counter]
  (cond
    (empty? counter)
    (if (= 0 (compare (first coll) x))
      (numberOfRepeated (next coll) x 1) (numberOfRepeated (next coll) x 0))
    (empty? coll)
    (first counter)
    (= 0 (compare (first coll) x))
    (numberOfRepeated (next coll) x (inc (first counter)))
    :else (numberOfRepeated (next coll) x (first counter))))

(defn indexBeforeLastBoolOp [conditions lastIndex commands]
  (let [lastIndexOfAnd (getLastIndex (map #(str/upper-case %)
                                          (subvec (vec conditions) 0 lastIndex)) (nth commands 6))
        lastIndexOfOr (getLastIndex (map #(str/upper-case %)
                                         (subvec (vec conditions) 0 lastIndex)) (nth commands 7))]
    (if (> lastIndexOfAnd lastIndexOfOr) lastIndexOfAnd lastIndexOfOr)))


(defn checkAllConditions [row conditions commands]
  (let [lastIndexOfAnd (getLastIndex (map #(str/upper-case %) conditions) (nth commands 6))
        lastIndexOfOr (getLastIndex (map #(str/upper-case %) conditions) (nth commands 7))
        numberOfAnd (numberOfRepeated (map #(str/upper-case %) conditions) (nth commands 6))
        numberOfOr (numberOfRepeated (map #(str/upper-case %) conditions) (nth commands 7))]
    (cond
      (= 3 (count conditions)) (checkTruth row conditions)
      (= 4 (count conditions))
      (if (not= -1 (.indexOf (map #(str/upper-case %) conditions) (nth commands 8)))
        (not (checkTruth row (subvec (vec conditions) 1))) false)
      (= 1 (+ numberOfAnd numberOfOr))
      (if (= 1 numberOfAnd)
        (andOrNotCondition row conditions commands (nth commands 6))
        (andOrNotCondition row conditions commands (nth commands 7)))
      (> lastIndexOfAnd lastIndexOfOr)
      (checkAllConditions
        row
        (conj (subvec (vec conditions)
                      0 (+ 1 (indexBeforeLastBoolOp conditions lastIndexOfAnd commands)))
              (andOrNotCondition
                row
                (subvec (vec conditions)
                        (+ 1 (indexBeforeLastBoolOp conditions lastIndexOfAnd commands)) (count conditions))
                commands (nth commands 6)))
        commands)
      :else
      (checkAllConditions
        row
          (conj (subvec (vec conditions)
                      0 (+ 1 (indexBeforeLastBoolOp conditions lastIndexOfOr commands)))
              (andOrNotCondition
                row
                (subvec (vec conditions)
                        (+ 1 (indexBeforeLastBoolOp conditions lastIndexOfOr commands)) (count conditions))
                commands (nth commands 7)))
        commands))))

(defn whereClause [table words commands]
    (if (not= -1 (.indexOf (map #(str/upper-case %) words) (nth commands 2)))
      (filter #(checkAllConditions % (parseConditions words commands) commands) table) table))

(defn mergeColStep [indexRow indexColumn table & map]
  (if (= 1 (- (count table) indexColumn))
    (merge (first map) (nth (nth table indexColumn) indexRow))
    (mergeColStep indexRow (+ 1 indexColumn) table (merge (first map) (nth (nth table indexColumn) indexRow)))))

(defn mergeColumns [table]
  (for [i (range (count (first table)))]
    (mergeColStep i 0 table)))

(defn getTableNameJoinClause [str]
  (if (and (= 0 (.indexOf str "[")) (not= -1 (.indexOf str "]")))
    (subs str 1 (.indexOf str "]"))
    (nth (str/split str #"\.") 0)))

(defn pasteEmptyAndMakeMap [map]
  (zipmap (keys map) (take (count map) (repeat ""))))

(defn getAcceptableElements [currentTable forJoiningTable
                             currentTableKeyword forJoiningTableKeyword indexOfIter]
  (filter #(= (get (nth currentTable indexOfIter) (keyword currentTableKeyword))
              (get % (keyword forJoiningTableKeyword))) forJoiningTable))

(defn leftJoin [resultTable currentTable forJoiningTable
                currentTableKeyword forJoiningTableKeyword & indexOfIter]
  (let [acceptableElements (if (empty? indexOfIter)
                             (getAcceptableElements currentTable forJoiningTable currentTableKeyword
                                                    forJoiningTableKeyword 0)
                             (getAcceptableElements currentTable forJoiningTable currentTableKeyword
                                                    forJoiningTableKeyword (first indexOfIter)))
        emptyMapForJoiningKeys (pasteEmptyAndMakeMap (first forJoiningTable))]
    (cond
      (empty? indexOfIter)
      (leftJoin
        (if (empty? acceptableElements)
          (list (merge (nth currentTable 0) emptyMapForJoiningKeys))
          (map #(merge (nth currentTable 0) %) acceptableElements))
        currentTable forJoiningTable currentTableKeyword forJoiningTableKeyword 1)
      (= (first indexOfIter) (- (count currentTable) 1))
      (concat
        resultTable
        (if (empty? acceptableElements)
          (list (merge (nth currentTable (first indexOfIter)) emptyMapForJoiningKeys))
          (map #(merge (nth currentTable (first indexOfIter)) %) acceptableElements)))
      :else
      (leftJoin
        (concat
          resultTable
          (if (empty? acceptableElements)
            (list (merge (nth currentTable (first indexOfIter)) emptyMapForJoiningKeys))
            (map #(merge (nth currentTable (first indexOfIter)) %) acceptableElements)))
        currentTable forJoiningTable currentTableKeyword
        forJoiningTableKeyword (+ 1 (first indexOfIter))))))

(defn fullOuterJoin [resultTable currentTable forJoiningTable
                     currentTableKeyword forJoiningTableKeyword & indexOfIterListOfUsed]
  (let [acceptableElements (if (empty? indexOfIterListOfUsed)
                             (getAcceptableElements currentTable forJoiningTable currentTableKeyword
                                                    forJoiningTableKeyword 0)
                             (getAcceptableElements currentTable forJoiningTable currentTableKeyword
                                                    forJoiningTableKeyword (first indexOfIterListOfUsed)))
        emptyMapForJoiningKeys (pasteEmptyAndMakeMap (first forJoiningTable))
        emptyMapCurrentKeys (pasteEmptyAndMakeMap (first currentTable))]
    (cond
      (empty? indexOfIterListOfUsed)
      (fullOuterJoin
        (if (empty? acceptableElements)
          (list (merge (nth currentTable 0) emptyMapForJoiningKeys))
          (map #(merge (nth currentTable 0) %) acceptableElements))
        currentTable forJoiningTable currentTableKeyword forJoiningTableKeyword 1 acceptableElements)
      (= (first indexOfIterListOfUsed) (- (count currentTable) 1))
      (concat
        resultTable
        (if (empty? acceptableElements)
          (list (merge (nth currentTable (first indexOfIterListOfUsed)) emptyMapForJoiningKeys))
          (map #(merge (nth currentTable (first indexOfIterListOfUsed)) %) acceptableElements))
        (map #(merge emptyMapCurrentKeys %)
             (into '() (set/difference (set forJoiningTable) (set (second indexOfIterListOfUsed))))))
      :else
      (fullOuterJoin
        (concat
          resultTable
          (if (empty? acceptableElements)
            (list (merge (nth currentTable (first indexOfIterListOfUsed)) emptyMapForJoiningKeys))
            (map #(merge (nth currentTable (first indexOfIterListOfUsed)) %) acceptableElements)))
        currentTable forJoiningTable currentTableKeyword
        forJoiningTableKeyword (inc (first indexOfIterListOfUsed))
        (concat (second indexOfIterListOfUsed) acceptableElements)))))

(defn innerJoin [resultTable currentTable forJoiningTable
                 currentTableKeyword forJoiningTableKeyword & indexOfIter]
  (let [acceptableElements (if (empty? indexOfIter)
                             (getAcceptableElements currentTable forJoiningTable currentTableKeyword
                                                    forJoiningTableKeyword 0)
                             (getAcceptableElements currentTable forJoiningTable currentTableKeyword
                                                    forJoiningTableKeyword (first indexOfIter)))]
    (cond
      (empty? indexOfIter)
      (innerJoin
        (map #(merge (nth currentTable 0) %) acceptableElements)
        currentTable forJoiningTable currentTableKeyword forJoiningTableKeyword 1)
      (= (first indexOfIter) (- (count currentTable) 1))
      (concat
        resultTable
        (map #(merge (nth currentTable (first indexOfIter)) %) acceptableElements))
      :else
      (innerJoin
        (concat
          resultTable
          (map #(merge (nth currentTable (first indexOfIter)) %) acceptableElements))
        currentTable forJoiningTable currentTableKeyword
        forJoiningTableKeyword (+ 1 (first indexOfIter))))))

(defn changeJoin [currentTable forJoiningTable condition forJoiningTableName typeJoin]
  (let [textInParenthesesFirst (if (and (not= -1 (.indexOf (first condition) "("))
                                        (not= -1 (.indexOf (first condition) ")"))
                                        (> (.indexOf (first condition) ")")
                                           (.indexOf (first condition) "(")))
                                 (subs (first condition)
                                       (inc (str/last-index-of
                                              (subs (first condition)
                                                    0 (.indexOf (first condition) "(")) "."))
                                       (inc (.indexOf (first condition) ")"))) [])
        textInParenthesesLast (if (and (not= -1 (.indexOf (last condition) "("))
                                       (not= -1 (.indexOf (last condition) ")"))
                                       (> (.indexOf (last condition) ")")
                                          (.indexOf (last condition) "(")))
                                (subs (last condition)
                                      (inc (str/last-index-of
                                             (subs (last condition)
                                                   0 (.indexOf (last condition) "(")) "."))
                                      (inc (.indexOf (last condition) ")"))) [])]
    (cond
      (= "innerJoin" typeJoin)
        (if (= (getTableNameJoinClause (first condition)) forJoiningTableName)
          (innerJoin '() currentTable forJoiningTable
                    (if (empty? textInParenthesesLast)
                      (last (str/split (last condition) #"\.")) textInParenthesesLast)
                    (if (empty? textInParenthesesFirst)
                      (last (str/split (first condition) #"\.")) textInParenthesesFirst))
          (innerJoin '() currentTable forJoiningTable
                    (if (empty? textInParenthesesFirst)
                      (last (str/split (first condition) #"\.")) textInParenthesesFirst)
                    (if (empty? textInParenthesesLast)
                      (last (str/split (last condition) #"\.")) textInParenthesesLast)))
      (= "fullOuterJoin" typeJoin)
        (if (= (getTableNameJoinClause (first condition)) forJoiningTableName)
          (fullOuterJoin '() currentTable forJoiningTable
                    (if (empty? textInParenthesesLast)
                      (last (str/split (last condition) #"\.")) textInParenthesesLast)
                    (if (empty? textInParenthesesFirst)
                      (last (str/split (first condition) #"\.")) textInParenthesesFirst))
          (fullOuterJoin '() currentTable forJoiningTable
                    (if (empty? textInParenthesesFirst)
                      (last (str/split (first condition) #"\.")) textInParenthesesFirst)
                    (if (empty? textInParenthesesLast)
                      (last (str/split (last condition) #"\.")) textInParenthesesLast)))
      (= "leftJoin" typeJoin)
        (if (= (getTableNameJoinClause (first condition)) forJoiningTableName)
          (leftJoin '() currentTable forJoiningTable
                    (if (empty? textInParenthesesLast)
                      (last (str/split (last condition) #"\.")) textInParenthesesLast)
                    (if (empty? textInParenthesesFirst)
                      (last (str/split (first condition) #"\.")) textInParenthesesFirst))
          (leftJoin '() currentTable forJoiningTable
                    (if (empty? textInParenthesesFirst)
                      (last (str/split (first condition) #"\.")) textInParenthesesFirst)
                    (if (empty? textInParenthesesLast)
                      (last (str/split (last condition) #"\.")) textInParenthesesLast))))))

(defn renameMapKeys [table sameKeys tableName]
  (map #(set/rename-keys
          % (zipmap sameKeys
                    (map (fn [currKey]
                           (keyword (subs (str currKey "(" tableName ")") 1))) sameKeys))) table))

(defn joinTables [table words commands]
  (let [firstJoinIndex (.indexOf (map #(str/upper-case %) words) (nth commands 18))]
    (cond
      (= -1 firstJoinIndex)
      table
      (and (= 0 (compare (str/upper-case (nth words (- firstJoinIndex 1))) (nth commands 14)))
           (= 0 (compare (str/upper-case (nth words (+ firstJoinIndex 2))) (nth commands 19))))
      (let [forJoiningTable (getFile (nth words (+ 1 firstJoinIndex)))
            tableJoiningIndex (if (= (nth words (+ 1 firstJoinIndex))
                                     (subs (nth words (+ 3 firstJoinIndex))
                                           1 (str/last-index-of (nth words (+ 3 firstJoinIndex)) "]")))
                                           (+ 3 firstJoinIndex) (+ 5 firstJoinIndex))
            nameOfColumnInCondition (subs (nth words tableJoiningIndex)
                                          (+ 2 (str/last-index-of (nth words tableJoiningIndex) "]"))
                                          (count (nth words tableJoiningIndex)))
            sameKeys (vec (set/intersection (set (keys (first table))) (set (keys (first forJoiningTable)))))
            sameConditionKeys (not= -1 (.indexOf sameKeys (keyword nameOfColumnInCondition)))
            correctKeysJoiningTable (renameMapKeys forJoiningTable sameKeys (nth words (+ 1 firstJoinIndex)))]
        (joinTables (changeJoin
                      table correctKeysJoiningTable
                      (if sameConditionKeys
                        (subvec (vec (map #(if (or (= (nth words (+ 3 firstJoinIndex)) %)
                                                   (= (nth words (+ 5 firstJoinIndex)) %))
                                             (if (= (getTableNameJoinClause %)
                                                    (nth words (+ 1 firstJoinIndex)))
                                               (str % "(" (nth words (+ 1 firstJoinIndex)) ")") %) %) words))
                                (+ 3 firstJoinIndex)
                                (+ 6 firstJoinIndex))
                        (subvec (vec words) (+ 3 firstJoinIndex) (+ 6 firstJoinIndex)))
                      (nth words (+ 1 firstJoinIndex)) "innerJoin")
                    (subvec (vec words) (+ 6 firstJoinIndex) (count words)) commands))
      (and (= 0 (compare (str/upper-case (nth words (- firstJoinIndex 2))) (nth commands 15)))
           (= 0 (compare (str/upper-case (nth words (- firstJoinIndex 1))) (nth commands 16)))
           (= 0 (compare (str/upper-case (nth words (+ firstJoinIndex 2))) (nth commands 19))))
      (let [forJoiningTable (getFile (nth words (+ 1 firstJoinIndex)))
            tableJoiningIndex (if (= (nth words (+ 1 firstJoinIndex))
                                     (subs (nth words (+ 3 firstJoinIndex))
                                           1 (str/last-index-of (nth words (+ 3 firstJoinIndex)) "]")))
                                (+ 3 firstJoinIndex) (+ 5 firstJoinIndex))
            nameOfColumnInCondition (subs (nth words tableJoiningIndex)
                                          (+ 2 (str/last-index-of (nth words tableJoiningIndex) "]"))
                                          (count (nth words tableJoiningIndex)))
            sameKeys (vec (set/intersection (set (keys (first table))) (set (keys (first forJoiningTable)))))
            sameConditionKeys (not= -1 (.indexOf sameKeys (keyword nameOfColumnInCondition)))
            correctKeysJoiningTable (renameMapKeys forJoiningTable sameKeys (nth words (+ 1 firstJoinIndex)))]
        (joinTables (changeJoin
                      table correctKeysJoiningTable
                      (if sameConditionKeys
                        (subvec (vec (map #(if (or (= (nth words (+ 3 firstJoinIndex)) %)
                                                   (= (nth words (+ 5 firstJoinIndex)) %))
                                             (if (= (getTableNameJoinClause %)
                                                    (nth words (+ 1 firstJoinIndex)))
                                               (str % "(" (nth words (+ 1 firstJoinIndex)) ")") %) %) words))
                                (+ 3 firstJoinIndex)
                                (+ 6 firstJoinIndex))
                        (subvec (vec words) (+ 3 firstJoinIndex) (+ 6 firstJoinIndex)))
                      (nth words (+ 1 firstJoinIndex)) "fullOuterJoin")
                    (subvec (vec words) (+ 6 firstJoinIndex) (count words)) commands))
      (and (= 0 (compare (str/upper-case (nth words (- firstJoinIndex 1))) (nth commands 17)))
           (= 0 (compare (str/upper-case (nth words (+ firstJoinIndex 2))) (nth commands 19))))
      (let [forJoiningTable (getFile (nth words (+ 1 firstJoinIndex)))
            tableJoiningIndex (if (= (nth words (+ 1 firstJoinIndex))
                                     (subs (nth words (+ 3 firstJoinIndex))
                                           1 (str/last-index-of (nth words (+ 3 firstJoinIndex)) "]")))
                                (+ 3 firstJoinIndex) (+ 5 firstJoinIndex))
            nameOfColumnInCondition (subs (nth words tableJoiningIndex)
                                          (+ 2 (str/last-index-of (nth words tableJoiningIndex) "]"))
                                          (count (nth words tableJoiningIndex)))
            sameKeys (vec (set/intersection (set (keys (first table))) (set (keys (first forJoiningTable)))))
            sameConditionKeys (not= -1 (.indexOf sameKeys (keyword nameOfColumnInCondition)))
            correctKeysJoiningTable (renameMapKeys forJoiningTable sameKeys (nth words (+ 1 firstJoinIndex)))]
        (joinTables (changeJoin
                      table correctKeysJoiningTable
                      (if sameConditionKeys
                        (subvec (vec (map #(if (or (= (nth words (+ 3 firstJoinIndex)) %)
                                                   (= (nth words (+ 5 firstJoinIndex)) %))
                                             (if (= (getTableNameJoinClause %)
                                                    (nth words (+ 1 firstJoinIndex)))
                                               (str % "(" (nth words (+ 1 firstJoinIndex)) ")") %) %) words))
                                (+ 3 firstJoinIndex)
                                (+ 6 firstJoinIndex))
                        (subvec (vec words) (+ 3 firstJoinIndex) (+ 6 firstJoinIndex)))
                      (nth words (+ 1 firstJoinIndex)) "leftJoin")
                    (subvec (vec words) (+ 6 firstJoinIndex) (count words)) commands)))))

(defn checkForJoin [file words commands]
  (if (= -1 (.indexOf (map #(str/upper-case %) words) (nth commands 18)))
    file (joinTables file words commands)))

(defn conditionHaving [havingIndex words]
  (if (= -1 havingIndex) [] (if (> (+ havingIndex 4) (count words))
                              [] (subvec (vec words) (+ 1 havingIndex) (+ 4 havingIndex)))))

(defn checkConditionForCurrRow [tableContainsRow condition commands]
  (let [aggregateFunc (if (or (= (str/upper-case (first (str/split (first condition) #"\("))) (nth commands 11))
                              (= (str/upper-case (first (str/split (first condition) #"\("))) (nth commands 12))
                              (= (str/upper-case (first (str/split (first condition) #"\("))) (nth commands 13)))
                        (str/upper-case (first (str/split (first condition) #"\(")))
                        (str/upper-case (first (str/split (last condition) #"\("))))
        indexOfAggFunc (if (str/starts-with? (str/upper-case (first condition)) aggregateFunc) 0 2)
        rowToAggregate (subs (nth condition indexOfAggFunc)
                             (inc (.indexOf (nth condition indexOfAggFunc) "("))
                             (- (count (nth condition indexOfAggFunc)) 1))]
    (if (= "=" (nth condition 1))
      (cond
        (= aggregateFunc (nth commands 11))
        (if (=
              (if (= indexOfAggFunc 0)
                (read-string (nth condition 2)) (read-string (nth condition 0)))
              (get (first (countFunc (getColumn rowToAggregate tableContainsRow))) :count))
          true false)
        (= aggregateFunc (nth commands 12))
        (if (=
              (if (= indexOfAggFunc 0)
                (read-string (nth condition 2)) (read-string (nth condition 0)))
              (get (first (avgFunc (getColumn rowToAggregate tableContainsRow))) :avg))
          true false)
        (= aggregateFunc (nth commands 13))
        (if (=
              (if (= indexOfAggFunc 0)
                (read-string (nth condition 2)) (read-string (nth condition 0)))
              (get (first (minFunc (getColumn rowToAggregate tableContainsRow))) :min))
          true false))
      (cond
        (and (= aggregateFunc (nth commands 11)) (= 0 indexOfAggFunc))
        (if (> (get (first (countFunc (getColumn rowToAggregate tableContainsRow))) :count)
               (read-string (nth condition 2))) true false)
        (and (= aggregateFunc (nth commands 12)) (= 0 indexOfAggFunc))
        (if (> (get (first (avgFunc (getColumn rowToAggregate tableContainsRow))) :avg)
               (read-string (nth condition 2))) true false)
        (and (= aggregateFunc (nth commands 13)) (= 0 indexOfAggFunc))
        (if (> (get (first (minFunc (getColumn rowToAggregate tableContainsRow))) :min)
               (read-string (nth condition 2))) true false)
        (and (= aggregateFunc (nth commands 11)) (= 2 indexOfAggFunc))
        (if (> (read-string (nth condition 2))
               (get (first (countFunc (getColumn rowToAggregate tableContainsRow))) :count))
          true false)
        (and (= aggregateFunc (nth commands 12)) (= 2 indexOfAggFunc))
        (if (> (read-string (nth condition 2))
               (get (first (avgFunc (getColumn rowToAggregate tableContainsRow))) :avg))
          true false)
        (and (= aggregateFunc (nth commands 13)) (= 2 indexOfAggFunc))
        (if (> (read-string (nth condition 2))
               (get (first (minFunc (getColumn rowToAggregate tableContainsRow))) :min))
          true false)))))

(defn getBoolTableOfResults [distinctTable mainTable condition commands]
  (map #(checkConditionForCurrRow
          (filter (fn [currentRow] (containsForMap currentRow %)) mainTable) condition commands) distinctTable))

(defn having [distinctTable mainTable condition commands]
  (if (empty? condition)
    distinctTable
    (filter #(nth (getBoolTableOfResults distinctTable mainTable condition commands)
                  (.indexOf distinctTable %)) distinctTable)))

(defn deleteElementsMap [map keysForDelete]
  (if (empty? keysForDelete) map
                             (deleteElementsMap (dissoc map (keyword (first keysForDelete))) (next keysForDelete))))

(defn getTableGroupBy [file columns]
  (let [elementsForDelete (vec (set/difference (set (map #(subs (str %) 1) (keys (first file)))) (set columns)))]
    (distinct (map #(deleteElementsMap % elementsForDelete) file))))

(defn groupByForAggregate [file resultTable column aggFunc]
  (cond
    (= "countFunc" aggFunc)
      (map #(first (countFunc
                     (getColumn (subs column (+ (.indexOf column "(") 1) (- (count column) 1))
                                (filter (fn [currentRowFile] (containsForMap currentRowFile %))
                                        file)))) resultTable)
    (= "avgFunc" aggFunc)
      (map #(first (avgFunc
                     (getColumn (subs column (+ (.indexOf column "(") 1) (- (count column) 1))
                                (filter (fn [currentRowFile] (containsForMap currentRowFile %))
                                        file)))) resultTable)
    (= "minFunc" aggFunc)
      (map #(first (minFunc
                     (getColumn (subs column (+ (.indexOf column "(") 1) (- (count column) 1))
                                (filter (fn [currentRowFile] (containsForMap currentRowFile %))
                                        file)))) resultTable)))

(defn searchCorrCondCase [conditions currRow]
  (let [indexOfValue (if (= -1 (.indexOf (keys currRow) (keyword (nth conditions 0)))) 0 2)]
    (cond
      (= 1 (count conditions))
      (first conditions)
      (= 4 (count conditions))
      (if (= "=" (nth conditions 1))
        (if (= 0 (compareForStringNumber
                   (if (= \" (first (nth conditions indexOfValue))
                          (last (nth conditions indexOfValue)))
                     (subs (nth conditions indexOfValue)
                           1 (- (count (nth conditions indexOfValue)) 1)) (nth conditions indexOfValue))
                   (get currRow (keyword (nth conditions (if (= 0 indexOfValue) 2 0))))))
          (nth conditions 3) "")
        (if (= 0 indexOfValue)
          (if (> (compareForStringNumber
                   (if (= \" (first (nth conditions 0))
                          (last (nth conditions 0)))
                     (subs (nth conditions 0)
                           1 (- (count (nth conditions 0)) 1)) (nth conditions 0))
                   (get currRow (keyword (nth conditions 2)))) 0)
            (nth conditions 3) "")
          (if (> (compareForStringNumber
                   (get currRow (keyword (nth conditions 0)))
                   (if (= \" (first (nth conditions 2))
                          (last (nth conditions 2)))
                     (subs (nth conditions 2)
                           1 (- (count (nth conditions 2)) 1)) (nth conditions 2))) 0)
            (nth conditions 3) "")))
      :else
      (if (= "=" (nth conditions 1))
        (if (= 0 (compareForStringNumber
                   (if (= \" (first (nth conditions indexOfValue))
                          (last (nth conditions indexOfValue)))
                     (subs (nth conditions indexOfValue)
                           1 (- (count (nth conditions indexOfValue)) 1))
                     (nth conditions indexOfValue))
                   (get currRow (keyword (nth conditions (if (= 0 indexOfValue) 2 0))))))
          (nth conditions 3) (searchCorrCondCase (subvec (vec conditions) 4 (count conditions)) currRow))
        (if (= 0 indexOfValue)
          (if (> (compareForStringNumber
                   (if (= \" (first (nth conditions 0))
                          (last (nth conditions 0)))
                     (subs (nth conditions 0)
                           1 (- (count (nth conditions 0)) 1)) (nth conditions 0))
                   (get currRow (keyword (nth conditions 2)))) 0)
            (nth conditions 3) (searchCorrCondCase (subvec (vec conditions) 4 (count conditions)) currRow))
          (if (> (compareForStringNumber
                   (get currRow (keyword (nth conditions 0)))
                   (if (= \" (first (nth conditions 2))
                          (last (nth conditions 2)))
                     (subs (nth conditions 2)
                           1 (- (count (nth conditions 2)) 1)) (nth conditions 2))) 0)
            (nth conditions 3) (searchCorrCondCase (subvec (vec conditions) 4 (count conditions)) currRow)))))))

(defn divideConditions [conditions commands & resultConditions]
  (let [indexOfWhere (.indexOf (map #(str/upper-case %) conditions) (nth commands 23))
        indexOfThen (.indexOf (map #(str/upper-case %) conditions) (nth commands 24))
        indexOfElse (.indexOf (map #(str/upper-case %) conditions) (nth commands 25))]
    (cond
      (and (= 0 indexOfWhere) (= 4 indexOfThen) (= 6 (count conditions)))
      (concat (if (empty? resultConditions) resultConditions (first resultConditions))
              (subvec (vec conditions) 1 4) (conj [] (nth conditions 5)))
      (and (= 0 indexOfWhere) (= 4 indexOfThen) (<= 8 (count conditions)))
      (divideConditions (subvec (vec conditions) 6 (count conditions)) commands
                        (concat (if (empty? resultConditions) resultConditions (first resultConditions))
                                (subvec (vec conditions) 1 4) (conj [] (nth conditions 5))))
      (= 0 indexOfElse)
      (concat (if (empty? resultConditions) resultConditions (first resultConditions)) (conj [] (nth conditions 1)))
      :else resultConditions)))

(defn checkCaseCondition [file conditions nameOfMap commands]
  (let [splitConditions (re-seq #"\"[^\"]+\"|[\S]+" conditions)
        filteredConditions
        (divideConditions (subvec (vec splitConditions)
                                  (inc (.indexOf (map #(str/upper-case %) splitConditions) (nth commands 22)))
                                  (.indexOf (map #(str/upper-case %) splitConditions) (nth commands 26))) commands)]
    (map #(hash-map (keyword nameOfMap) (searchCorrCondCase filteredConditions %)) file)))

(defn groupBy [file columns words commands]
  (let [distinctTable (getTableGroupBy
                        file (str/split (nth words (+ 2 (.indexOf
                                                          (map #(str/upper-case %) words)
                                                          (nth commands 20)))) #","))
        havingCondition (conditionHaving (.indexOf (map #(str/upper-case %) words) (nth commands 21)) words)
        resultTable (having distinctTable file havingCondition commands)]
    (map #(let [splitCol (str/split % #" ")
                indexOfNameCase (.indexOf (keys (first file)) (keyword (last splitCol)))]
            (cond
              (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                   (str/starts-with? (str/upper-case %) (nth commands 11)))
              (groupByForAggregate file resultTable % "countFunc")
              (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                   (str/starts-with? (str/upper-case %) (nth commands 12)))
              (groupByForAggregate file resultTable % "avgFunc")
              (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                   (str/starts-with? (str/upper-case %) (nth commands 13)))
              (groupByForAggregate file resultTable % "minFunc")
              (and (> (count splitCol) 1) (= (str/upper-case (first splitCol)) (nth commands 22)))
              (if (or (not= -1 indexOfNameCase) (not= -1 (.indexOf (keys (first resultTable)) "case")))
                (getColumn (if (= -1 indexOfNameCase) "case" (last splitCol)) resultTable)
                (if (= (str/upper-case
                         (nth splitCol (- (count splitCol) 2))) (nth commands 27))
                  (checkCaseCondition resultTable % (nth splitCol (- (count splitCol) 1)) commands)
                  (checkCaseCondition resultTable % "case" commands)))
              :else (getColumn % resultTable)))
         columns)))

(defn checkForGroupBy [words commands]
  (if (= 1 (- (.indexOf (map #(str/upper-case %) words) (nth commands 5))
              (.indexOf (map #(str/upper-case %) words) (nth commands 20))))
    true false))

(defn getAllColumns [file columns commands]
  (map #(let [splitCol (re-seq #"\"[^\"]+\"|[\S]+" %)
              indexOfNameCase (.indexOf (keys (first file)) (keyword (last splitCol)))]
          (cond
            (= 0 (compare % "*"))
            file
            (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                 (= 2 (- (.indexOf % ")") (.indexOf % "(")))
                 (= 0 (compare "*" (subs % (+ 1 (.indexOf % "(")) (- (count %) 1))))
                 (str/starts-with? (str/upper-case %) (nth commands 11)))
              (if (not= -1 (.indexOf (keys (first file)) :count))
                (getColumn "count" file) (countFunc file))
            (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                 (str/starts-with? (str/upper-case %) (nth commands 11)))
            (if (not= -1 (.indexOf (keys (first file)) :count))
              (getColumn "count" file)
              (countFunc (getColumn (subs % (+ 1 (.indexOf % "(")) (- (count %) 1)) file)))
            (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                 (str/starts-with? (str/upper-case %) (nth commands 12)))
            (if (not= -1 (.indexOf (keys (first file)) :avg))
              (getColumn "avg" file)
              (avgFunc (getColumn (subs % (+ 1 (.indexOf % "(")) (- (count %) 1)) file)))
            (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                 (str/starts-with? (str/upper-case %) (nth commands 13)))
            (if (not= -1 (.indexOf (keys (first file)) :min))
              (getColumn "min" file)
              (minFunc (getColumn (subs % (+ 1 (.indexOf % "(")) (- (count %) 1)) file)))
            (and (> (count splitCol) 1) (= (str/upper-case (first splitCol)) (nth commands 22)))
              (if (or (not= -1 indexOfNameCase) (not= -1 (.indexOf (keys (first file)) :case)))
                (getColumn (if (= -1 indexOfNameCase) "case" (last splitCol)) file)
                (if (= (str/upper-case
                        (nth splitCol (- (count splitCol) 2))) (nth commands 27))
                 (checkCaseCondition file % (nth splitCol (- (count splitCol) 1)) commands)
                 (checkCaseCondition file % "case" commands)))
            :else (getColumn % file)))
       columns))

(defn getMainTable [columns words commands]
  (let [fileWithAllColumns (getFile (nth words (+ 1 (.indexOf (map #(str/upper-case %) words)
                                                              (nth commands 1)))))
        filteredFile (orderBy (whereClause (checkForJoin fileWithAllColumns words commands)
                                           words commands) words commands)]
    (if (checkForGroupBy words commands)
      (groupBy filteredFile columns words commands)
      (if (= -1 (.indexOf (map #(str/upper-case %) words) (nth commands 3)))
        (getAllColumns filteredFile columns commands)
        (if (= -1 (.indexOf columns "*"))
          (getAllColumns (distinct (mergeColumns (getAllColumns filteredFile columns commands))) columns commands)
          (getAllColumns (distinct filteredFile) columns commands))))))

(defn getCorrectTable [words commands]
  (if (not= 0 (compare (str/upper-case (nth words 1)) (nth commands 3)))
    (if (not= 0 (compare (str/upper-case (nth words 2)) (nth commands 1)))
      [] (getMainTable (map #(str/trim %) (str/split (nth words 1) #",")) words commands))
    (if (not= 0 (compare (str/upper-case (nth words 3)) (nth commands 1)))
      [] (getMainTable (map #(str/trim %) (str/split (nth words 2) #",")) words commands))))

(defn deeperParseForWords [words commands]
  (let [indexOfFrom (.indexOf (map #(str/upper-case %) words) (nth commands 1))
        indexOfDistinct (.indexOf (map #(str/upper-case %) words) (nth commands 3))]
    (if (= -1 indexOfDistinct)
      (concat (subvec (vec words) 0 1) (conj [] (str/join " " (subvec (vec words) 1 indexOfFrom)))
              (subvec (vec words) indexOfFrom (count words)))
      (concat (subvec (vec words) 0 2) (conj [] (str/join " " (subvec (vec words) 2 indexOfFrom)))
              (subvec (vec words) indexOfFrom (count words))))))

(defn getResult [expr commands]
  (let [words (deeperParseForWords (re-seq #"\"[^\"]+\"|[\S]+" expr) commands)]
    (if (not= 0 (compare (str/upper-case (first words)) (first commands)))
      [] (getCorrectTable words commands))))

(defn view []
  (println "Write your query below:")
  (let [input (read-line)
        commands ["SELECT" "FROM" "WHERE" "DISTINCT"
                  "ORDER" "BY" "AND" "OR" "NOT" "ASC" "DESC" "COUNT" "AVG" "MIN"
                  "INNER" "FULL" "OUTER" "LEFT" "JOIN" "ON" "GROUP"
                  "HAVING" "CASE" "WHEN" "THEN" "ELSE" "END" "AS"]]
    (if (= "QUIT" (str/upper-case input)) true (do (printTable (getResult input commands)) (view)))))

(defn createHelp [] (println (slurp "Help.txt")) true)

(defn -main [& args]
  (do (createHelp) (view)))