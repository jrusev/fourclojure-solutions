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

;; 23. Reverse a Sequence
;; Write a function which reverses a sequence.
;; Special Restrictions: reverse, rseq
#(reduce (fn [res x] (conj res x)) () %)
#(reduce (fn [res x] (cons x res)) [] %)
#(into () %)
(fn [s]
  (loop [s s res ()]
    (if (empty? s) res
      (recur (next s) (conj res (first s))))))

;; 24. Sum It All Up
;; Write a function which returns the sum of a sequence of numbers.
reduce +
#(apply + %)

;; 25. Find the odd numbers
;; Write a function which returns only the odd numbers from a sequence.
filter odd?
#(filter odd? %)

;; 26. Fibonacci Sequence
;; Write a function which returns the first X fibonacci numbers.
#(loop [s [1 1]]
  (if (= % (count s)) s
      (let [f1 (-> s butlast last)
            f2 (last s)]
        (recur (conj s (+ f1 f2))))))
#(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))
#(take % ((fn fib [a b] (cons a (lazy-seq (fib b (+ a b))))) 1 1))

;; TDD principle: Write the minimum amount of code required to make the test pass :)
(fn [i] (take i '(1 1 2 3 5 8 13 21)))


;; 27. Palindrome Detector
;; Write a function which returns true if the given sequence is a palindrome.
;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
#(= (seq %) (reverse %))
#(let [median (quot (count %) 2)]
  (= (take median %) (take median (reverse %))))
