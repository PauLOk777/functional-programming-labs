(ns lab3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn getFile [name]
  (def string1 (slurp name))
  string1)

(defn makeTableCSV [name]
  (map #(clojure.string/split % #",")
       (clojure.string/split (getFile name) #"\n")))

(defn checkFormat [name]
  (def formatFile (clojure.string/split name #"\."))
  (if (= (str (nth formatFile 1)) "csv") true false))

(defn -main
  [& args]
  (def input (read-line))
  (if (checkFormat input)
    (println (nth (nth (makeTableCSV input) 1) 1))
    (println "Hello")))

(-main)