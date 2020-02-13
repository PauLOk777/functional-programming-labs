(ns mycloj-pr.core
  (:gen-class))

;(let [x (let [u 4] u)
;      y (* x 3)
;      z (* x y)]
;  (* x y z))
;
;(if false (+ 1 2) (* 3 4))
;
;(defn factorial [x]
;  (if (or (= x 1) (= x 0))
;    1
;    (* x (factorial (- x 1))))
;  )
;(factorial (+ 5 1))
;
;(let [list1 '(2 5 1)]
;  (conj list1 3))

;(#(take 10 (next (range 0 1000 7))))
;(take 10 (rest (filter #(= 0 (mod % 7)) (range))))
;(reduce #(if (> %1 %2) %1 %2) [1 2 100 5 6 11])

;1. Описати неіменовану функцію для об'єднання голів трьох
;списків в один список, вихідні дані взяти з таблиці 1.
; 1 Variant

(def firstList '(DF GH JK))
(def secondList '(1 2 5 6 (4 5) 4))
(def thirdList '(ER RT TY 5 6 6 5))

; println
((fn [list1 list2 list3]
   (println (conj '() (first list1) (first list2) (first list3))))
 '(DF GH JK) '(1 2 5 6 (4 5) 4) '(ER RT TY 5 6 6 5))
; without
((fn [list1 list2 list3]
   (conj '() (first list1) (first list2) (first list3)))
 '(DF GH JK) '(1 2 5 6 (4 5) 4) '(ER RT TY 5 6 6 5))
; 2 Variant
(#(conj '() (first %1) (first %2) (first %3))
  firstList secondList thirdList)

; 2. Описати іменовану функцію для створення нового списку з
;елементів декількох вихідних списків. В якості вихідних списків
;використовувати списки таблиці 1. Номери елементів списків взяти в
;таблиці 2.

(defn secondFn [list1 list2 list3]
  (conj '() (nth list1 2) (nth list2 5) (nth list3 5)))

(secondFn firstList secondList thirdList)

; 3. Описати іменовану функцію для обчислення результату
;відповідно до варіанта індивідуального завдання зі списку. Набори для
;множин взяти в таблиці 1.

(def A (set '(1 2)))
(def B (set '(2 3)))
(def AB (clojure.set/union A B))

(defn expr [a b ab]
  (clojure.set/intersection
    (clojure.set/union a b)
    (clojure.set/union a (clojure.set/difference ab b))
    (clojure.set/union (clojure.set/difference ab a) b)
    ))

(expr A B AB)