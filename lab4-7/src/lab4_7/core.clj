(ns lab4-7.core
  (:gen-class)
  (:use [clojure.data.csv])
  (:require [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

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

(defn countFunc [column counter]
  (cond
    (and (= "null" (get (first column) (first (keys (first column))))) (= 1 (count column)))
      (list (hash-map :count counter))
    (and (not= "null" (get (first column) (first (keys (first column))))) (= 1 (count column)))
      (list (hash-map :count (+ 1 counter)))
    (= "null" (get (first column) (first (keys (first column)))))
      (countFunc (next column) counter)
    :else
      (countFunc (next column) (+ 1 counter))))

(defn avgFunc [column sum n]
  (cond
    (and (= 1 (count column)) (number? (get (first column) (first (keys (first column))))))
      (list (hash-map :avg (/ (+ sum (get (first column) (first (keys (first column))))) n)))
    (number? (get (first column) (first (keys (first column)))))
      (avgFunc (next column) (+ sum (get (first column) (first (keys (first column))))) n)
    (= 1 (count column))
     (list (hash-map :avg (/ (+ sum (read-string (get (first column) (first (keys (first column)))))) n)))
    :else
      (avgFunc (next column) (+ sum (read-string (get (first column) (first (keys (first column)))))) n)))

(defn minFunc [column minValue]
  (cond
    (and (= 1 (count column))
         (> 0 (compareForStringNumber (get (first column) (first (keys (first column)))) minValue)))
      (list (hash-map :min (get (first column) (first (keys (first column))))))
    (and (= 1 (count column))
         (<= 0 (compareForStringNumber (get (first column) (first (keys (first column)))) minValue)))
      (list (hash-map :min minValue))
    (> 0 (compareForStringNumber (get (first column) (first (keys (first column)))) minValue))
      (minFunc (next column) (get (first column) (first (keys (first column)))))
    :else (minFunc (next column) minValue)))

(defn whereClause [table words commands]
  (let [whereIndex (.indexOf (map #(str/upper-case %) words) (nth commands 2))]
    (cond
      (= -1 whereIndex) table

      (and
        (= 0 (compare (nth words (+ whereIndex 2)) "="))
        (every? #(Character/isDigit %) (nth words (+ whereIndex 3))))
      (filter
        #(= 0 (compare (read-string (get % (keyword (nth words (+ whereIndex 1)))))
                       (read-string (nth words (+ whereIndex 3))))) table)

      (and
        (= 0 (compare (nth words (+ whereIndex 2)) ">"))
        (every? #(Character/isDigit %) (nth words (+ whereIndex 3))))
      (filter
        #(< 0 (compare (read-string (get % (keyword (nth words (+ whereIndex 1)))))
                       (read-string (nth words (+ whereIndex 3))))) table)

      (= 0 (compare (nth words (+ whereIndex 2)) "="))
      (filter
        #(= 0 (compare (get % (keyword (nth words (+ whereIndex 1))))
                       (subs (nth words (+ whereIndex 3))
                             (+ (str/index-of (nth words (+ whereIndex 3)) "\"") 1)
                             (str/last-index-of (nth words (+ whereIndex 3)) "\"")))) table)

      (= 0 (compare (nth words (+ whereIndex 2)) ">"))
      (filter
        #(< 0 (compare (get % (keyword (nth words (+ whereIndex 1))))
                       (subs (nth words (+ whereIndex 3))
                             (+ (str/index-of (nth words (+ whereIndex 3)) "\"") 1)
                             (str/last-index-of (nth words (+ whereIndex 3)) "\"")))) table)
      :else [])))

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
  (let [file (getFile fileName)]
    (map #(if (not= 0 (compare % "*"))
            (getColumn % (whereClause file words commands))
              (whereClause file words commands))
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
    (if (not= 0 (compare (str/upper-case (first words)) (first commands))) []
      (processColumns words commands))))

(defn -main
  [& args]
  (println "Write your query below this: ")
  (let [input (read-line)
        commands ["SELECT" "FROM" "WHERE" "DISTINCT" "ORDER" "BY" "AND" "OR" "NOT" "ASC" "DESC"]
        result (getResult input commands)]
  (printTable result)))

;(orderBy [[{:foo "1" :bar "11" :loh "2"} {:foo "1" :bar "12" :loh "4"} {:foo "2" :bar "11" :loh "3"} {:foo "1" :bar "31" :loh "3"}]]
;           (resolve (symbol (checkOrder ["desc"]
;                ["SELECT" "FROM" "WHERE" "DISTINCT" "ORDER" "BY" "AND" "OR" "NOT" "ASC" "DESC"])))
;           (getColumnsOrderBy ["by" "foo,loh,bar"]
;                              ["SELECT" "FROM" "WHERE" "DISTINCT" "ORDER" "BY" "AND" "OR" "NOT" "ASC" "DESC"])
;           )