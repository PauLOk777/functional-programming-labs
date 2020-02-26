(ns lab3.core
  (:gen-class)
  (:use [clojure.data.csv])
  (:require [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))


(defn getFile [name]
  (def string1 (slurp name))
  string1)

(defn makeTableTSV [name]
  (map #(clojure.string/split % "\t")
       (clojure.string/split (getFile name) #"\n")))

(defn makeTableCSV [name]
  (with-open [reader (io/reader name)]
    (doall
      (csv/read-csv reader))))

(defn printTable [table]
  (loop [i 0]
    (when (< i (count table))
      (loop [j 0]
        (when (< j (count (nth table i)))
          (print (format "%40s| " (nth (nth table i) j)))
          (recur (+ j 1))))
      (println "")
      (recur (+ i 1)))))

(defn checkFormat [name]
  (def formatFile (str/split name #"\."))
  (if (= (str (nth formatFile 1)) "csv") true false))

(defn -main
  [& args]
  (println "Input name of file: ")
  (def input (read-line))
  ;(def input "mp-assistants.csv")
  (if (checkFormat input)
    (printTable (makeTableCSV input))
    (printTable (makeTableTSV input))))

;(-main)
;(getRegexp 3)
;
;(loop [x 0]
;  (when (< x 5)
;    (println (format "%s|%s" "123" "4566"))
;    (recur (+ x 1))))
;(format "%s" (nth [1 "dd"] 1))
;
;(.indexOf ["t" "v" "d"] "d")
;
;(with-open [reader (clojure.java.io/reader "mp-posts_full.csv")]
;  (doall
;    (csv/read-csv reader)))






















