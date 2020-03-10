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
      (recur (+ i 1)))))

(defn checkFormat [name]
  (def formatFile (str/split name #"\."))
  (if (= (str (nth formatFile 1)) "csv") true false))

(defn getFile [name]
  (if (checkFormat name)
    (makeTableCSV name)
    (makeTableTSV name)))

(defn whereClause [table words commands]
  (def whereIndex (.indexOf (map #(str/upper-case %) words) (nth commands 2)))
  (println (nth words (+ whereIndex 3)))
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
    :else []))

(defn getColumn [columnName file]
  (map #(select-keys % [(keyword columnName)]) file))

(defn getMainTable [columns words fileName commands]
  (def file (getFile fileName))
  (map #(if (not= 0 (compare % "*"))
          (getColumn % (whereClause file words commands))
          (whereClause file words commands)) columns))

(defn withDistinct [expr commands]
  [])

(defn noDistinct [words commands]
  (cond
    (not= 0 (compare (str/upper-case (nth words 2)) (nth commands 1))) []
    :else
    (getMainTable (str/split (nth words 1) #",") words (nth words 3) commands)))

(defn processColumns [words commands]
  (if (not= 0 (compare (str/upper-case (nth words 1)) (nth commands 3)))
    (noDistinct words commands)
    (withDistinct words commands)))

(defn getResult [expr commands]
  (def words (re-seq #"\"\D+\"|[\S]+" expr))
  (if (not= 0 (compare (str/upper-case (first words)) (first commands))) []
    (processColumns words commands)))

(defn -main
  [& args]
  (println "Write your query below this: ")
  (def input (read-line))
  (def commands ["SELECT" "FROM" "WHERE" "DISTINCT"])
  (printTable (getResult input commands)))

;(getResult "SELECT * FROM mp-posts_full.csv WHERE full_name > \"Ясько Єлизавета Олексіївна\"" ["SELECT" "FROM" "WHERE" "DISTINCT"])
;(re-seq #"\"\D+\"|[\S]+" "SELECT ALL FROM DAUN WHERE JKF = \"dAUN EBANIY оа\"")