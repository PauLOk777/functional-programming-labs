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

(defn printTable [table]
  (loop [l 0]
    (when (< l (count table))
      (loop [k 0]
        (when (< k (count (first (nth table l))))
          (print (format "%40s| " (name (nth (getKeysForPrint l table) k))))
          (recur (+ k 1))))
      (recur (+ l 1))))
  (println)
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
      (recur (+ i 1))))
  true)

(defn checkFormat [name]
  (let [formatFile (str/split name #"\.")]
    (if (= (str (nth formatFile 1)) "csv") true false)))

(defn getFile [name]
  (if (checkFormat name)
    (makeTableCSV name)
    (makeTableTSV name)))

(defn compareForStringNumber [a b]
  (cond
    (number? a) (compare a b)
    (and (every? #(Character/isDigit %) a) (string? a)) (compare (read-string a) (read-string b))
    :else (compare a b)))

(defn getLastIndex [coll word]
  (if (not= -1 (.indexOf (reverse coll) word))
    (- (count coll) (.indexOf (reverse coll) word) 1) -1))

; compare for maps
(defn map-sort [a b & keysOfMap]
  (cond
    ; if map contains only 1 key and values are equal return 0
    (and (empty? keysOfMap)
         (= 0 (compareForStringNumber (get a (first (keys a))) (get b (first (keys b)))))
         (= 1 (count (keys a))))
    0
    ; if map contains only 1 key and values have difference return compare
    (and (empty? keysOfMap)
         (not= 0 (compareForStringNumber (get a (first (keys a))) (get b (first (keys b)))))
         (= 1 (count (keys a))))
    (compareForStringNumber (get a (first (first keysOfMap))) (get b (first (first keysOfMap))))
    ; if map contains more then 1 key and first values of first key are equal return next keys
    (and (empty? keysOfMap)
         (= 0 (compareForStringNumber (get a (first (keys a))) (get b (first (keys b))))))
    (map-sort a b (next (keys a)))
    ; if last values are equal return 0
    (and (= 1 (count (first keysOfMap)))
         (= 0 (compareForStringNumber (get a (first (first keysOfMap))) (get b (first (first keysOfMap))))))
    0
    ; if values are equals return next keys
    (and (> (count (first keysOfMap)) 1)
         (= 0 (compareForStringNumber (get a (first (first keysOfMap))) (get b (first (first keysOfMap))))))
    (map-sort a b (next (first keysOfMap)))
    ; if values have difference return compare
    (empty? keysOfMap)
    (compareForStringNumber (get a (first (keys a))) (get b (first (keys b))))
    :else
    (compareForStringNumber (get a (first (first keysOfMap))) (get b (first (first keysOfMap))))))

(defn mergeStep [l r ord columns]
  (cond (empty? l) r
        (empty? r) l
        (and (empty? columns) (ord (map-sort (first l) (first r))))
        (cons (first l) (mergeStep (next l) r ord columns))
        (and (empty? columns) (not (ord (map-sort (first l) (first r)))))
        (cons (first r) (mergeStep l (next r) ord columns))
        (ord (map-sort (first l) (first r) columns))
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

(defn orderBy [table ord columns]
  (map #(mergeSort % ord columns) table))

(defn countFunc [table]
  (if (not= 1 (count (keys (first table))))
    (list (hash-map :count (count table)))
    (list (hash-map :count (count (filter #(not= "" (get % (first (keys %)))) table))))))

(defn sumForReduceMap [& body]
  (let [firstValueOfMap (get (first body) (first (keys (first body))))
        secondValueOfMap (get (second body) (first (keys (second body))))]
    (cond
      (and (number? firstValueOfMap) (number? secondValueOfMap))
      (merge-with + (first body) (second body))
      (and (string? firstValueOfMap) (string? secondValueOfMap))
      (update (first body) (first (keys (first body)))
              #(+ (read-string %) (read-string secondValueOfMap)))
      :else
      (update (first body) (first (keys (first body)))
              #(+ % (read-string secondValueOfMap))))))

(defn avgFunc [column]
  (let [sumOfMaps (reduce sumForReduceMap column)
        numberOfElementsMap (countFunc column)
        numberOfElements (get (first numberOfElementsMap) (first (keys (first numberOfElementsMap))))
        avgValueMap (update sumOfMaps (first (keys sumOfMaps)) / (double numberOfElements))]
    (list (set/rename-keys avgValueMap {(first (keys avgValueMap)) :avg}))))

(defn minForReduceMap [& body]
  (let [firstValueOfMap (get (first body) (first (keys (first body))))
        secondValueOfMap (get (second body) (first (keys (second body))))]
    (if (and (map? (first body))
             (>= 0 (compareForStringNumber firstValueOfMap secondValueOfMap)))
      (first body) (second body))))

(defn minFunc [column]
  (let [minValueMap (reduce minForReduceMap column)]
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
  (cond
    (and (= 0 (compare (nth condition 1) "="))
         (every? #(Character/isDigit %) (nth condition 2)))
    (if (= (read-string (nth condition 2))
           (read-string (get row (keyword (nth condition 0))))) true false)
    (and (= 0 (compare (nth condition 1) ">"))
         (every? #(Character/isDigit %) (nth condition 2)))
    (if (< 0 (compareForStringNumber (read-string (get row (keyword (nth condition 0))))
                                     (read-string (nth condition 2))))
      true false)
    (and (= 0 (compare (nth condition 1) "=")))
    (if (= 0 (compare (get row (keyword (nth condition 0)))
                      (subs (nth condition 2) (+ (str/index-of (nth condition 2) "\"") 1)
                            (str/last-index-of (nth condition 2) "\"")))) true false)
    (and (= 0 (compare (nth condition 1) ">")))
    (if (< 0 (compare (get row (keyword (nth condition 0)))
                      (subs (nth condition 2) (+ (str/index-of (nth condition 2) "\"") 1)
                            (str/last-index-of (nth condition 2) "\"")))) true false)))

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
        commands)
      )))

(defn whereClause [table words commands]
  (let [conditions (parseConditions words commands)]
    (if (not= -1 (.indexOf (map #(str/upper-case %) words) (nth commands 2)))
    (filter #(checkAllConditions % conditions commands) table) table)))

(defn mergeColStep [indexRow indexColumn table & map]
  (if (= 1 (- (count table) indexColumn))
    (merge (first map) (nth (nth table indexColumn) indexRow))
    (mergeColStep indexRow (+ 1 indexColumn) table (merge (first map) (nth (nth table indexColumn) indexRow)))))

(defn mergeColumns [table]
  (for [i (range (count (first table)))]
    (mergeColStep i 0 table)))

(defn getColumn [columnName file]
  (map #(select-keys % [(keyword columnName)]) file))

(defn getMainTable [columns words fileName commands]
  (let [file (getFile fileName)
        filteredFile (whereClause file words commands)]
    (map #(cond
            (= 0 (compare % "*"))
              filteredFile
            (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                 (= 2 (- (.indexOf % "(") (.indexOf % ")")))
                 (= 0 (compare "*" (subs % (+ 1 (.indexOf % "(")) (.indexOf % ")"))))
                 (str/starts-with? (str/upper-case %) (nth commands 11)))
              (countFunc filteredFile)
            (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                 (str/starts-with? (str/upper-case %) (nth commands 11)))
              (countFunc (getColumn (subs % (+ 1 (.indexOf % "(")) (.indexOf % ")")) filteredFile))
            (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                 (str/starts-with? (str/upper-case %) (nth commands 12)))
              (avgFunc (getColumn (subs % (+ 1 (.indexOf % "(")) (.indexOf % ")")) filteredFile))
            (and (not= -1 (.indexOf % "(")) (not= -1 (.indexOf % ")"))
                 (str/starts-with? (str/upper-case %) (nth commands 13)))
              (minFunc (getColumn (subs % (+ 1 (.indexOf % "(")) (.indexOf % ")")) filteredFile))
            :else (getColumn % filteredFile))
         columns)))

(defn checkOrder [words commands]
  (cond
    (not= -1 (.indexOf (map #(str/upper-case %) words) (nth commands 10))) "pos?"
    :else "neg?"))

(defn getColumnsOrderBy [words commands]
  (let [orderWords (nth words
                        (+ 1
                           (.indexOf (map #(str/upper-case %) words) (nth commands 5))))]
    (map #(keyword %) (str/split orderWords #","))))

(defn myDistinct [table columns words commands]
  (if (not= -1 (.indexOf columns "*"))
    (let [file (distinct (mergeColumns table))]
      (map #(if (not= 0 (compare % "*"))
              (getColumn % (whereClause file words commands))
              (whereClause file words commands))
           columns))
    (list (distinct (mergeColumns table)))))

(defn withDistinct [words commands]
  (cond
    ; Bad query
    (not= 0 (compare (str/upper-case (nth words 3)) (nth commands 1))) []
    ; ORDER BY
    (= 1 (- (.indexOf (map #(str/upper-case %) words) (nth commands 5))
            (.indexOf (map #(str/upper-case %) words) (nth commands 4))))
    (orderBy (myDistinct (getMainTable
                           (str/split (nth words 2) #",") words (nth words 4) commands)
                         (str/split (nth words 2) #",")
                         words commands) (resolve (symbol (checkOrder words commands)))
             (getColumnsOrderBy words commands))
    :else
    ; default query with DISTINCT and maybe WHERE clauses
    (myDistinct (getMainTable
                  (str/split (nth words 2) #",") words (nth words 4) commands)
                (str/split (nth words 2) #",")
                words commands)))

(defn noDistinct [words commands]
  (cond
    ; Bad query
    (not= 0 (compare (str/upper-case (nth words 2)) (nth commands 1))) []
    ; ORDER BY
    (= 1 (- (.indexOf (map #(str/upper-case %) words) (nth commands 5))
            (.indexOf (map #(str/upper-case %) words) (nth commands 4))))
    (orderBy
      (getMainTable (str/split (nth words 1) #",") words (nth words 3) commands)
      (resolve (symbol (checkOrder words commands))) (getColumnsOrderBy words commands))
    :else
    ; default query maybe with WHERE clause
    (getMainTable (str/split (nth words 1) #",") words (nth words 3) commands)))

(defn processColumns [words commands]
  (if (not= 0 (compare (str/upper-case (nth words 1)) (nth commands 3)))
    ; check for DISTINCT clause
    (noDistinct words commands)
    (withDistinct words commands)))

(defn getResult [expr commands]
  (let [words (re-seq #"\"\D+\"|[\S]+" expr)]
    ; Expression must start with SELECT or select (noSens)
    (if (not= 0 (compare (str/upper-case (first words)) (first commands)))
      [] (processColumns words commands))))

(defn -main
  [& args]
  (println "Write your query below this: ")
  (let [input (read-line)
        commands ["SELECT" "FROM" "WHERE" "DISTINCT"
                  "ORDER" "BY" "AND" "OR" "NOT" "ASC" "DESC" "COUNT" "AVG" "MIN"]
        result (getResult input commands)]
    (printTable result)))
