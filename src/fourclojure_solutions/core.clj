(ns fourclojure-solutions.core)

;; 16. Hello World
;; Write a function which returns a personalized greeting.
;; (= (__ "Dave") "Hello, Dave!")
#(str "Hello, " % "!")
#(format "Hello, %s!" %)

;; 19. Last Element
;; Write a function which returns the last element in a sequence.
;; Special Restrictions: last
;; (= (__ [1 2 3 4 5]) 5)
(fn[coll] (nth coll (dec (count coll))))
(comp first reverse)

;; 20. Penultimate Element
;; Write a function which returns the second to last element from a sequence.
;; (= (__ (list 1 2 3 4 5)) 4)
#(first (take-last 2 %))
(comp last butlast)
(comp second reverse)

;; 21. Nth Element
;; Write a function which returns the Nth element from a sequence.
;; Special Restrictions: nth
;; (= (__ '(4 5 6 7) 2) 6)
#((vec %) %2)
(fn [coll n] (first (drop n coll)))
(fn [seq n] (if (zero? n) (first seq) (recur (rest seq) (dec n))))

;; 22. Count a Sequence
;; Write a function which returns the total number of elements in a sequence.
;; Special Restrictions: count
;; (= (__ '(1 2 3 3 1)) 5)
#(reduce (fn [x _] (inc x)) 0 %)

;; 23. Reverse a Sequence
;; Write a function which reverses a sequence.
;; Special Restrictions: reverse, rseq
;; (= (__ [1 2 3 4 5]) [5 4 3 2 1])
#(reduce (fn [res x] (conj res x)) () %)
#(reduce (fn [res x] (cons x res)) [] %)
#(into () %)
(fn [s]
  (loop [s s res ()]
    (if (empty? s) res
      (recur (next s) (conj res (first s))))))

;; 24. Sum It All Up
;; Write a function which returns the sum of a sequence of numbers.
;; (= (__ [1 2 3]) 6)
reduce +
#(apply + %)

;; 25. Find the odd numbers
;; Write a function which returns only the odd numbers from a sequence.
;; (= (__ #{1 2 3 4 5}) '(1 3 5))
filter odd?
#(filter odd? %)

;; 26. Fibonacci Sequence
;; Write a function which returns the first X fibonacci numbers.
;; (= (__ 8) '(1 1 2 3 5 8 13 21))
#(loop [s [1 1]]
  (if (= % (count s)) s
      (let [f1 (-> s butlast last)
            f2 (last s)]
        (recur (conj s (+ f1 f2))))))
#(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))
#(take % ((fn fib [a b] (cons a (lazy-seq (fib b (+ a b))))) 1 1))

;; TDD principle: Write the minimum amount of code required to pass the test :)
(fn [i] (take i '(1 1 2 3 5 8 13 21)))


;; 27. Palindrome Detector
;; Write a function which returns true if the given sequence is a palindrome.
;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
;; (false? (__ '(1 2 3 4 5)))
;; (true? (__ "racecar"))
#(= (seq %) (reverse %))
#(let [median (quot (count %) 2)]
  (= (take median %) (take median (reverse %))))

;; 28. Flatten a Sequence
;; Write a function which flattens a sequence.
;; Special Restrictions: flatten
;; (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(fn flat [s]
  (reduce
   (fn [acc x]
     (concat acc
             (if (sequential? x) (flat x) [x])))
   [] s))

(fn flat [coll]
  (let [x (first coll) xs (next coll)]
    (concat
     (if (sequential? x)
       (flat x)
       [x])
     (when (sequential? xs)
       (flat xs)))))

(fn flat [x] (if (coll? x) (mapcat flat x) [x]))

;; 29. Get the Caps
;; Write a function which takes a string and returns a new string
;; containing only the capital letters.
;; (= (__ "HeLlO, WoRlD!") "HLOWRD")
(fn [s] (apply str (filter #(Character/isUpperCase %) s)))
#(clojure.string/replace % #"[^A-Z]+" "")
#(apply str (re-seq #"[A-Z]" %))

;; 30. Compress a Sequence
;; Write a function which removes consecutive duplicates from a sequence.
;; (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
#(reduce
  (fn [acc x]
    (if (= x (last acc))
      acc
      (conj acc x)))
  [] %)

#(map first (partition-by identity %))

;; 31. Pack a Sequence
;; Write a function which packs consecutive duplicates into sub-lists.
;; (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
#(partition-by identity %)


;; 32. Duplicate a Sequence
;; Write a function which duplicates each element of a sequence.
;; (= (__ [1 2 3]) '(1 1 2 2 3 3))
#(reduce (fn [acc x] (conj acc x x)) [] %)
#(mapcat list % %)
#(interleave % %)

;; 33. Replicate a Sequence
;; Write a function which replicates each element of a sequence a
;; variable number of times.
;; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
(fn [coll n]
  (if (= n 1) coll
    (apply interleave (repeat n coll))))
(fn [coll n] (mapcat #(repeat n %) coll))
#(mapcat (partial repeat %2) %1)

;; 34. Implement range
;; Write a function which creates a list of all integers in a given range.
;; Special Restrictions: range
;; (= (__ 1 4) '(1 2 3))
(fn [l r]
  (loop [rng [l] x (inc l)]
    (if (= x r) rng
      (recur (conj rng x) (inc x)))))
(fn [x y] (take (- y x) (iterate inc x)))
(fn [from to]
  (take-while #(< % to)
              (iterate inc from)))

;; 38. Maximum value
;; Write a function which takes a variable number of parameters
;; and returns the maximum value
;; Special Restrictions: max, max-key
;; (= (__ 1 8 3 4) 8)
(fn [& xs] (reduce (fn [a b] (if (> a b) a b)) xs))
(comp last sort list)
#(last (sort %&))

;; 39. Interleave Two Seqs
;; Write a function which takes two sequences and returns the first item
;; from each, then the second item from each, then the third, etc.
;; Special Restrictions: interleave
;; (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
#(mapcat list %1 %2)
mapcat (fn [& x] x)
mapcat list

(fn [s1 s2]
  (loop [s1 s1
         s2 s2
         result []]
    (if-not (and (seq s1) (seq s2))
      result
      (recur
       (rest s1)
       (rest s2)
       (conj result (first s1) (first s2))))))

;; 40. Interpose a Seq
;; Write a function which separates the items of a sequence by a given value.
;; Special Restrictions: interpose
;; (= (__ 0 [1 2 3]) [1 0 2 0 3])
#(butlast (interleave %2 (repeat %)))
#(next (interleave (repeat %) %2))
#(next (mapcat (fn [x] [% x]) %2))

;; 41. Drop Every Nth Item
;; Write a function which drops every Nth item from a sequence.
;; (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
#(->> % (partition-all (dec %2) %2) flatten)
#(apply concat (partition-all (dec %2) %2 %))
#(mapcat (partial take (dec %2)) (partition-all %2 %))


;; 42. Factorial Fun
;; Write a function which calculates factorials.
#(apply * (range 1 (inc %)))

(fn ! [n]
  (if (zero? n) 1
      (* n (! (- n 1)))))

;; 43. Reverse Interleave
;; Write a function which reverses the interleave process into x
;; number of subsequences.
;; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(fn [coll n]
  (reduce #(map conj % %2)
          (take n (repeat []))
          (partition n coll)))

(fn [coll n] (map #(take-nth n (drop % coll)) (range n)))
#(apply map list (partition %2 %)) ; See goo.gl/oklqNY
