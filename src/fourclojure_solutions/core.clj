(ns fourclojure-solutions.core)

;; 16. Hello World
;; Write a function which returns a personalized greeting.
#(str "Hello, " % "!")
#(format "Hello, %s!" %)

;; 19. Last Element
;; Write a function which returns the last element in a sequence.
;; Special Restrictions: last
(fn[coll] (nth coll (dec (count coll))))
(comp first reverse)

;; 20. Penultimate Element
;; Write a function which returns the second to last element from a sequence.
#(first (take-last 2 %))
(comp last butlast)
(comp second reverse)

;; 21. Nth Element
;; Write a function which returns the Nth element from a sequence.
;; Special Restrictions: nth
#((vec %) %2)
(fn [coll n] (first (drop n coll)))
(fn [seq n] (if (zero? n) (first seq) (recur (rest seq) (dec n))))

;; 22. Count a Sequence
;; Write a function which returns the total number of elements in a sequence.
;; Special Restrictions: count
#(reduce (fn [x _] (inc x)) 0 %)
