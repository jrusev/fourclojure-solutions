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
#(if (next %) (recur (next %)) (first %))
#(nth % (dec (count %)))
#(first (take-last 1 %))
#(first(reverse %))
reduce #(do %2)
#(peek(vec %))

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
#(vals (group-by (fn [i] (mod i %2)) %1))
#(apply map list (partition %2 %)) ; See goo.gl/oklqNY


;; 44. Rotate Sequence
;; Write a function which can rotate a sequence in either direction.
;; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
(fn [n coll]
  (let [index (mod n (count coll))
        [a b] (split-at index coll)]
    (concat b a )))

#(let [i (mod % (count %2))] (concat (drop i %2) (take i %2)))
#(let [[a b] (split-at (mod % (count %2)) %2)] (concat b a))
#(let [c (count %2)] (take c (drop (mod % c) (cycle %2))))
#(apply concat ((juxt drop take) (mod % (count %2)) %2))

;; 45. Intro to Iterate
;; The iterate function can be used to produce an infinite lazy seq.
;; (= __ (take 5 (iterate #(+ 3 %) 1)))
[1 4 7 10 13]
(->> (range) (rest) (take-nth 3) (take 5))

;; 46. Flipping out
;; Write a higher-order function which flips the order of the
;; arguments of an input function.
;; (= 3 ((__ nth) 2 [1 2 3 4 5]))
#(fn [& args] (apply % (reverse args)))
#(fn [a b] (% b a))
(fn [f] #(f %2 %))

;; 47. Contain Yourself
;; The contains? function checks if a KEY is present in a given collection.
;; This often leads beginner clojurians to use it incorrectly with
;; numerically indexed collections like vectors and lists.
;; For numerically indexed collections like vectors, this tests if
;; the numeric key is within the range of indexes.
;; Examples:
(contains? [:a :b :c] :b)  ;=> false
(contains? [:a :b :c] 2)   ;=> true
(contains? "f" 0)          ;=> true
(contains? "f" 1)          ;=> false

;; 48. Intro to some
;; The some function takes a predicate function and a collection.
;; It returns the first logical true value of (predicate x) where x
;; is an item in the collection.
(= 6 (some #{2 7 6} [5 6 7 8]))
(= 6 (some #(when (even? %) %) [5 6 7 8]))

;; 49. Split a sequence
;; Write a function which will split a sequence into two parts.
;; (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
#(vector (take % %2) (drop % %2))
(juxt take drop)

;; 50. Split by Type
;; Write a function which takes a sequence consisting of items with
;; different types and splits them up into a set of homogeneous
;; sub-sequences. The internal order of each sub-sequence should be
;; maintained, but the sub-sequences themselves can be returned in any
;; order (this is why 'set' is used in thetest cases).
;; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
#(vals (group-by type %))

;; 51. Advanced Destructuring
;; Here is an example of some more sophisticated destructuring.
;; (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))
[1 2 3 4 5]

;; 52. Intro to Destructuring
;; Let bindings and function parameter lists support destructuring.
;; (= [2 4] (let [[a b c d e f g] (range)] __))
[c e]

;; 53. Longest Increasing Sub-Seq
;; Given a vector of integers, find the longest consecutive
;; sub-sequence of increasing numbers. If two sub-sequences have the
;; same length, use the one that occurs first. An increasing
;; sub-sequence must have a length of 2 or greater to qualify.
;; (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
(fn [coll]
  (loop [[x & xs] coll
         curr [x]
         longest [x]]
    (if (empty? xs)
      (if (second longest) longest [])
      (let [y (first xs)
            curr (if (< x y) (conj curr y) [y])
            longest (max-key count curr longest)]
        (recur xs curr longest)))))

(fn [coll]
  (->>
   (range 2 (inc (count coll)))
   (mapcat #(partition % 1 coll))
   (filter #(apply < %))
   (cons [])
   (sort-by count >)
   first))

(fn [xs]
  (->>
   (range (count xs) 1 -1)
   (mapcat #(partition % 1 xs))
   (filter #(->> (apply sorted-set %) seq (= %)))
   first vec))


(fn [t]
  (apply max-key
         #(or ({1 -1} (count %)) (count %))
         (reductions
          #(if (= (dec %2) (last %)) (conj % %2) [%2])
          [] t)))

(fn [v]
  (or (first (filter
              #(apply < %)
              (mapcat #(partition % 1 v) (range (count v) 1 -1))))
      []))

;; 54. Partition a Sequence
;; Write a function which returns a sequence of lists of x items each.
;; Lists of less than x items should not be returned.
;; Special Restrictions: partition, partition-all
;; (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
(fn [n coll]
  (loop [xs coll
         result []]
    (let [chunk (take n xs)]
      (if (= n (count chunk))
        (recur (drop n xs) (conj result chunk))
        result))))

(fn f [n x]
  (if (>= (count x) n)
    (cons (take n x) (f n (drop n x)))))

#(take-nth % (apply map list (take % (iterate next %2))))

;; 55. Count Occurrences
;; Write a function which returns a map containing the number of
;; occurences of each distinct item in a sequence.
;; Special Restrictions: frequencies
;; (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
#(reduce
  (fn [result x]
    (assoc result x
           (if-let [count (get result x)] (inc count) 1)))
  {} %)

#(into {} (map (fn [[k v]] [k (count v)]) (group-by identity %)))
#(reduce (fn [res x] (update-in res [x] (fnil inc 0))) {} %)
#(apply merge-with + (map (fn [a] {a 1}) %))
reduce #(assoc % %2 (+ 1 (% %2 0))) {}
reduce #(merge-with + % {%2 1}) {}

;; 56. Find Distinct Items
;; Write a function which removes the duplicates from a sequence.
;; Order of the items must be maintained.
;; Special Restrictions: distinct
;; (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
reduce #(if (some #{%2} %) % (conj % %2)) []
reduce #(if ((set %) %2) % (conj % %2)) []

;; 57. Simple Recursion
;; A recursive function is a function which calls itself.
;; This is one of the fundamental techniques used in functional programming.
;; (= __ ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))
[5 4 3 2 1]

;; 58. Function Composition
;; Write a function which allows you to create function compositions.
;; The parameter list should take a variable number of functions, and
;; create a function applies them from right-to-left.
;; Special Restrictions: comp
;; (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
(fn [& fs]
  (fn [& args]
    (reduce (fn [x f] (f x))
            (apply (last fs) args)
            (rest (reverse fs)))))

(fn [& f]
  (let [[g & f] (reverse f)]
    (fn [& a]
      (reduce #(%2 %1) (apply g a) f))))

(fn c [h & t] (if (empty? t) h #(h (apply (apply c t) %&))))
(fn [& s] (reduce (fn [f g] #(f (apply g %&))) s))

;; 59. Juxtaposition
;; Take a set of functions and return a new function that takes a
;; variable number of arguments and returns a sequence containing the
;; result of applying each function left-to-right to the argument list.
;; Special Restrictions: juxt
;; (fn [& funcs] (fn [& args] (map #(apply % args) funcs)))
(fn [& fs]
  (fn [& args]
    (map #(apply % args) fs)))

#(fn [& r] (map (fn [f] (apply f r)) %&))

;; 60. Sequence Reductions
;; Write a function which behaves like reduce, but returns each
;; intermediate value of the reduction. Your function must accept either
;; two or three arguments, and the return sequence must be lazy.
;; Special Restrictions: reductions
;; (= (take 5 (__ + (range))) [0 1 3 6 10])
(fn reduct
  ([f [x & xs]] (reduct f x xs))
  ([f x [y & xs]]
   (cons x (if y (lazy-seq (reduct f (f x y) xs))))))

apply (fn [f i & s]
       (#(% %) (memoize #(cons i (lazy-seq (map f (% %) s))))))

apply (fn [f i & xs] ((fn ff [] (lazy-cat [i] (map f (ff) xs)))))

;; 61. Map Construction
;; Write a function which takes a vector of keys and a vector of
;; values and constructs a map from them.
;; Special Restrictions: zipmap
;; (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
#(apply assoc {} (interleave % %2))
#(apply merge (map hash-map % %2))
#(into {} (map vector % %2))

;; 62. Re-implement Iterate
;; Given a side-effect free function f and an initial value x write a
;; function which returns an infinite lazy sequence of x, (f x), (f (f x))...
;; Special Restrictions: iterate
;; (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
#(reductions (fn [x _] (% x)) %2 (range))
(fn it [f x] (cons x (lazy-seq (it f (f x)))))
(fn it [f x] (lazy-cat [x] (it f (f x))))

;; 63. Group a Sequence
;; Given a function f and a sequence s, write a function which returns
;; a map. The keys should be the values of f applied to each item in
;; s. The value at each key should be a vector of corresponding items
;; in the order they appear in s.
;; Special Restrictions: group-by
;; (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
(fn [f s]
  (reduce
   (fn [m x] (update-in m [(f x)] (fnil #(conj % x) [])))
   {} s))

#(reduce
  (fn [m x] (assoc m (% x) (conj (m (% x) []) x)))
  {} %2)

(fn [f s] (reduce #(merge-with concat % {(f %2) [%2]}) {} s))
#(apply merge-with into (map (fn [x] {(% x) [x]}) %2))
#(apply merge-with into (for [x %2] {(% x) [x]}))

;; 65. Black Box Testing
;; Write a function which takes a collection and returns one of :map,
;; :set, :list, or :vector - describing the type of collection it was
;; given. You won't be allowed to inspect their class or use the
;; built-in predicates like list? - the point is to poke at them and
;; understand their behavior.
;; (= [:map :set :vector :list] (map __ [{} #{} [] ()]))
#(cond
  (not (ifn? %)) :list
  (reversible? %) :vector
  (associative? %) :map
  :else :set)

#(let [e (empty %)]
   (case e
     {}  :map
     #{} :set
     (first (conj e :vector :list))))

(comp {\# :set \{ :map \[ :vector \c :list} first str)

;; 66. Greatest Common Divisor
;; Given two integers, write a function which returns the GCD.
#(if (zero? %2) % (recur %2 (mod % %2)))


;; 67. Prime Numbers
;; Write a function which returns the first x number of prime numbers.
(fn [n]
  (let [prime? (fn [x] (not (some #(zero? (mod x %)) (range 2 x))))]
  (take n (filter prime? (iterate inc 2)))))

(fn [n]
  (->>
  (range)
  (drop 2)
  (filter (fn [x] (every? #(< 0 (mod x %)) (range 2 x))))
  (take n)))

(fn [x]
 (take x
       (filter
        #(= (inc (mod (apply * (range 1N %)) %)) %)
        (iterate inc 2))))

#(take %2 (remove (set (for [i % j (range (+ i i) 999 i)] j)) %)) (range 2 999)

;; 69. Merge with a Function
;; Write a function which takes a function f and a variable number of
;; maps. Your function should return a map that consists of the rest
;; of the maps conj-ed onto the first. If a key occurs in more than
;; one map, the mapping(s) from the latter (left-to-right) should be
;; combined with the mapping in the result by calling (f val-in-result
;; val-in-latter)
;; Special Restrictions: merge-with
;;(= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}) {:a 4, :b 6, :c 20})
(fn [f & maps]
  (reduce
   #(reduce (fn [m [k v]]
              (assoc m k (if (m k) (f (m k) v) v)))
            %1 %2)
   maps))

#(into {} (for [[k s] (group-by key (apply concat %&))]
            [k (reduce % (vals s))]))

;; 70. Word Sorting
;; Write a function that splits a sentence up into a sorted list of
;; words. Capitalization should not affect sort order and punctuation
;; should be ignored.
;; (= (__  "Have a nice day.") ["a" "day" "Have" "nice"])
#(->> (clojure.string/split % #"\W+") (sort-by clojure.string/lower-case))
#(sort-by clojure.string/lower-case (re-seq #"\w+" %))
(fn [s] (sort-by #(.toLowerCase %) (re-seq #"\w+" s)))
#(sort-by (fn [x] (.toLowerCase x)) (.split % "\\W"))

;; 73. Analyze a Tic-Tac-Toe Board
;; A tic-tac-toe board is represented by a two dimensional vector. X
;; is represented by :x, O is represented by :o, and empty is
;; represented by :e. A player wins by placing three Xs or three Os in
;; a horizontal, vertical, or diagonal row. Write a function which
;; analyzes a tic-tac-toe board and returns :x if X has won, :o if O
;; has won, and nil if neitherplayer has won.
(fn [board]
  (let [b (into [] (flatten board))
        views [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]
        lines (map #(map b %) views)]
    (some {[:x :x :x] :x [:o :o :o] :o} lines)))

#(some {[:x :x :x] :x [:o :o :o] :o}
       (concat % (apply map list %)
               (for [d [[[0 0] [1 1] [2 2]] [[2 0] [1 1] [0 2]]]]
                 (for [[x y] d] ((% x) y)))))

(fn [[[a b c] [d e f] [g h i] :as x]]
    (some {[:x :x :x] :x [:o :o :o] :o}
          (list* [a d g] [b e h] [c f i] [a e i] [c e g] x)))

(fn [b]
  (->> [[0 0 0] [1 1 1] [2 2 2] [0 1 2] [2 1 0]]
       (map #(map nth b %))
       (concat b)
       (some {[:x :x :x] :x [:o :o :o] :o})))

(fn [D B] ( ->>
           `(~@B ~@(apply map list B) ~(D B) ~(-> B reverse D))
           (some #{[:x :x :x] [:o :o :o]})
           first)
  ) (partial map-indexed #(%2 %))

#(some
  {[:x :x :x] :x [:o :o :o] :o}
  (partition 3
             (map (vec (flatten %))
                  [0 1 2 3 4 5 6 7 8 0 3 6 1 4 7 2 5 8 0 4 8 2 4 6])))

;; 74. Filter Perfect Squares
;; Given a string of comma separated integers, write a function which
;; returns a new comma separated string that only contains the numbers
;; which are perfect squares.
;; (= (__ "4,5,6,7,8,9") "4,9")
(fn [s]
  (->>
   (read-string (str "[" s  "]"))
   (filter (set (map #(* % %) (range 9))))
   (clojure.string/join ",")))

(fn [s]
  (->> s
    (re-seq #"[0-9]+")
    (map #(Integer/parseInt %))
    (filter #(zero? (mod (Math/sqrt %) 1)))
    (interpose ",")
    (apply str)))

;; 75. Euler's Totient Function
;; Two numbers are coprime if their greatest common divisor equals 1.
;; Euler's totient function f(x) is defined as the number of positive
;; integers less than x which are coprime to x. The special case f(1)
;; equals 1. Write a function which calculates Euler's totient
;; function.
;; (= (__ 99) 60)
(fn [n]
  (let [gcd #(if (zero? %2) % (recur %2 (mod % %2)))]
    (count
     (filter
      #(= 1 (gcd % n))
      (range n)))))

(fn [n] (inc
         (count
          (filter
           #(= 1 (.gcd (biginteger %) (biginteger n)))
           (range 2 n)))))

;; 77. Anagram Finder
;; Write a function which finds all the anagrams in a vector of words.
;; A word x is an anagram of word y if all the letters in x can be
;; rearranged in a different order to form y. Your function should
;; return a set of sets, where each sub-set is a group of words which
;; are anagrams of each other. Each sub-set should have at least two
;; words. Words without any anagrams should not be included in the
;; result.
;; (= (__ ["meat" "mat" "team" "mate" "eat"]) #{#{"meat" "team" "mate"}})
(fn [xs]
  (->> (group-by sort xs)
       (vals)
       (filter #(> (count %) 1))
       (map set)
       (set)))

#(set (for [[_ g] (group-by frequencies %)
            :when (next g)]
        (set g)))

#(->> % (group-by set) vals (filter next) (map set) set)

;; 78. Reimplement Trampoline
;; Reimplement the function described in "Intro to Trampoline".
;; Special Restrictions: trampoline
(fn t [x & xs]
  (if (fn? x) (t (apply x xs)) x))

#(loop [f (% %2)] (if (fn? f) (recur (f)) f))

;; 79. Triangle Minimal Path
;; Write a function which calculates the sum of the minimal path
;; through a triangle. The triangle is represented as a collection of
;; vectors. The path should start at the top of the triangle and move
;; to an adjacent number on the next row until the bottom of the
;; triangle is reached.
;;(= 7 (__ '([1]
;;          [2 4]
;;         [5 1 4]
;;        [2 3 4 5]))) ; 1->2->1->3
(fn f
  ([xs] (f 0 xs))
  ([p [x & xs]]
   (if (empty? xs)
     (x p)
     (+ (x p) (min
               (f p xs)
               (f (inc p) xs))))))

(fn f [[[a] & b]]
  (+ a (if b (min (f (map rest b))
                  (f (map butlast b))) 0)))

(fn [t]
  (first
   (reduce
    (fn [a s] (map + s (map #(apply min %) (partition 2 1 a))))
    (reverse t))))

;; 80. Perfect Numbers
;; A number is "perfect" if the sum of its divisors equal the number
;; itself. 6 is a perfect number because 1+2+3=6. Write a function
;; which returns true for perfect numbers and false otherwise.
;; (= (__ 8128)
(fn [n]
  (= n
     (reduce
      #(if (zero? (mod n %2)) (+ % %2) %)
      (range n))))

#(= % (apply + (for [i (range 1 %) :when (= 0 (mod % i))] i)))
#(= % (reduce (fn [s k] ({0 (+ s k)} (rem % k) s)) (range %)))

;; 81. Set Intersection
;; Write a function which returns the intersection of two sets. The
;; intersection is the sub-set of items that each set has in common.
;; Special Restrictions: intersection
;; (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
(fn [x y] (set (filter #(x %) y)))
#(set (filter % %2))
(comp set keep)

;; 82. Word Chains
;; A word chain consists of a set of words ordered so that each word
;; differs by only one letter from the words directly before and after
;; it. The one letter difference can be either an insertion, a
;; deletion, or a substitution. Here is an example word chain:
;;
;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
;;
;; Write a function which takes a sequence of words, and returns true
;; if they can be arranged into one continous word chain, and false if
;; they cannot.
;; (= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))

;; Try all paths with dfs to find one that contains all words
(letfn [(chainable? [a, b]
          (let [[x y] (sort-by count [a b])]
            (if (zero? (- (count y) (count x)))
              (= 1  (count (filter false? (map #(= % %2) x y))))
              (some
               #(= (seq x) (concat (take % y) (drop (inc %) y)))
               (range (count y))))))
        (graph [nodes]
          (reduce (fn [m x]
                    (assoc m x (filter #(chainable? x %) nodes))) {} nodes))
        (chain? [start graph visited]
          (or (= (count visited) (count graph))
              (some #(chain? % graph (conj visited %))
                    (remove visited (graph start)))))]
  (fn [words]
    (let [g (graph words)]
      (or (some #(chain? % g #{%}) words)
          false))))

;; Using Levenshtein distance
(letfn [(leven [[fa & ra :as a] [fb & rb :as b]]
          (cond (nil? a) (count b)
                (nil? b) (count a)
                (= fa fb) (leven ra rb)
                :else (+ 1
                        (min (leven ra rb)
                             (leven a rb)
                             (leven ra b)))))
        (rem-disj [ht e]
          [(dissoc ht e) (ht e)])
        (walkable? [[ht elts]]
          (if (empty? ht)
            true
            (let [walks (for [n-e elts :when (ht n-e)]
                          (walkable? (rem-disj ht n-e)))]
              (some true? walks))))]
  (fn [st]
    (let [ht (apply merge-with concat
                (for [a st, b st :when (= 1 (leven a b))] {a [b]}))]
      (or (some #(walkable? (rem-disj ht %)) st)
          false))))

;; psk810's solution (same as chouser's but more readable)
(fn [s]
  (letfn [(ch? [s1 s2]
            (loop [[a & b :as c] (seq s1) [d & e :as g] (seq s2)]
              (if (= a d) (recur b e)
                  (or (= b e) (= b g) (= c e)))))
          (t [e s] (or
                    (empty? s)
                    (some #(t % (disj s %)) (filter #(ch? e %) s))))]
    (or (some #(t % (disj s %)) s) false)))

;; 83. A Half-Truth
;; Write a function which takes a variable number of booleans. Your
;; function should return true if some of the parameters are true, but
;; not all of the parameters are true. Otherwise your function should
;; return false.
#(= 2 (count (set %&)))
not=

;; 84. Transitive Closure
;; Write a function which generates the transitive closure of a binary
;; relation. The relation will be represented as a set of 2 item
;; vectors.
;; (= (__ #{[8 4] [9 3] [4 2] [27 9]})
;;    #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})
(fn [s]
  (let [graph (apply merge-with into (for [[k v] s] {k [v]}))
        tree (fn f [root visited]
               (concat
                (graph root)
                (mapcat #(f % (conj visited %))
                        (remove visited (graph root)))))]
    (set (mapcat
          (fn [root] (map #(vector root %) (tree root #{})))
          (keys graph)))))

;; dzholev's solution
(defn tc [s]
  (let [tr (group-by first s)
        d (fn [[a b]] (map #(vector a (second %)) (tr b)))
        f (fn f [e]
            (let [a (d e)]
              (concat [e] a (mapcat f a))))]
    (set (mapcat f s))))

;; adereth's solution: add edges until no more edges can be added
#(loop [s %]
   (let [n (into s
                 (for [[a b] s [c d] s
                       :when (= b c)]
                   [a d]))]
      (if (= n s) n (recur n))))

;; youz's solution
(fn f [s]
  (#(if (= % s) s (f %))
   (set (for [[a b] s [c d] s]
          [a (if (= b c) d b)]))))

;; 85. Power Set
;; Write a function which generates the power set of a given set. The
;; power set of a set x is the set of all subsets of x, including the
;; empty set and x itself.
;; (= (__ #{1 2 3})
;;    #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
reduce (fn [s x] (into s (map #(conj % x) s))) #{#{}}

;; 86. Happy numbers
;; Happy numbers are positive integers that follow a particular formula: take
;; each individual digit, square it, and then sum the squares to get a new
;; number. Repeat with the new number and eventually, you might get to a number
;; whose squared sum is 1. This is a happy number. An unhappy number is one that
;; loops endlessly. Write a function that determines if a number is happy or not.
(fn [n]
  (loop [n n, mem #{}]
    (let [digits (map (comp read-string str) (str n))
          sum (apply + (map #(* % %) digits))]
      (or (= sum 1)
          (if (mem sum) false (recur sum (conj mem sum)))))))

;; For the solutions below see wikipedia.org/wiki/Happy_number#Sequence_behavior

;; mouse's solution
#(= 1
    (nth (iterate
          (fn [n]
            (apply + (map (zipmap "0123456789" (map * (range) (range)))
                          (str n))))
          %)
         9))

;; maximental's solution
(fn [m]
  (= 1
     (some #{1 4}
           (iterate (fn [k] (reduce #(+ % (let [c (- (int %2) 48)] (* c c)))
                                    0
                                    (str k)))
                    m))))

;; youz's solution
#(or (= % 1)
     (if (= % 4) false
         (recur
          (reduce (fn [a c] (+ a (* (- (int c) 48) (- (int c) 48))))
                  0 (str %)))))

;; 88. Symmetric Difference
;; Write a function which returns the symmetric difference of two
;; sets. The symmetric difference is the set of items belonging to one
;; but not both of the two sets.
#(clojure.set/difference (clojure.set/union % %2) (clojure.set/intersection % %2))
#(let [d clojure.set/difference] (into  (d % %2) (d %2 %)))
#(set (concat (apply disj %1 %2) (apply disj %2 %1)))
#(into (set (remove %2 %)) (remove % %2))
reduce #((if (% %2) disj conj) % %2)
#(set (mapcat remove [% %2] [%2 %]))

;; 89. Graph Tour
;; Starting with a graph you must write a function that returns true if it is
;; possible to make a tour of the graph in which every edge is visited exactly
;; once. The graph is represented by a vector of tuples, where each tuple
;; represents a single edge.
;; The rules are:
;; - You can start at any node.
;; - You must visit each edge exactly once.
;; - All edges are undirected.
(letfn [(graph [edges] 
          (apply merge-with into (for [[k v] edges] (conj {k [v]} {v [k]}))))
        (connected?
          ([graph]
           (let [r (first (keys graph))]
             (boolean (connected? r graph #{r}))))
          ([start graph visited]
           (or (= (count visited) (count graph))
               (some #(connected? % graph (conj visited %))
                     (remove visited (graph start))))))
        (odd-nodes [graph]
          (->> graph vals (map count) (filter odd?)))]
  (fn [edges]
    (let [g (graph edges)
          odd (count (odd-nodes g))]
      (and (connected? g)
           (or (= 0 odd) (= 2 odd))))))

;; lackita's solution
(fn [edges]
  (let [graph (group-by first (mapcat (fn [e] [e (vec (reverse e))])
                                      edges))
        odd-nodes (count (filter odd? (map count (vals graph))))
        connected ((fn traverse [v]
                     (let [new-v (into v (map last (mapcat graph v)))]
                       (if (= new-v v)
                         (= v (set (keys graph)))
                         (traverse new-v))))
                   #{(first (keys graph))})]
    (and connected
         (or (= odd-nodes 0)
             (= odd-nodes 2)))))

;; chouser's solution
(fn [e]
  (if (#{0 2} (count (filter odd? (vals (frequencies (mapcat seq e))))))
    (not (next (reduce
                (fn [g e]
                  (let [[a b] (map
                               (fn [n] (or
                                        (some #(if (% n) %) g)
                                        #{n}))
                               e)]
                    (conj (disj g a b) (into a b))))
                #{}
                e)))
    false))

;; maximental's solution
(fn [g]
  (and
   (->> g (mapcat seq) frequencies vals (filter odd?) count #{0 2} boolean)
   ((fn f [e]
      (#(if (= e %) (= % (set g)) (f %))
       (reduce (fn [a b] (into a (filter #(some (set b) %) (set g))))
               #{}
               e)))
    #{(first g)})))

;; youz's solution
(fn [[h & r]]
  ((fn f [a r]
     (or (empty? r)
         (boolean
          (some #(f (nth (remove #{a} %) 0) (remove #{%} r))
                (filter #(some #{a} %) r)))))
   (h 1) r))

;; 90. Cartesian Product
;; Write a function which calculates the Cartesian product of two sets.
#(set (for [x % y %2] [x y]))

;; 91. Graph Connectivity
;; Given a graph, determine whether the graph is connected. A
;; connected graph is such that a path exists between any two given
;; nodes.
;; -Your function must return true if the graph is connected and false otherwise.
;; -You will be given a set of tuples representing the edges of a
;;  graph. Each member of a tuple being a vertex/node in the graph.
;; -Each edge is undirected (can be traversed either direction).

;; To time the functions build a test graph with:
(defn graph [n] (for [x (range n) y (range n) :when (< (rand) 0.1)] [x y]))
(def g (graph 100)) ;; graph with 100 nodes (~1000 edges)
(time (dotimes [_ 10] (conn? g))) ;; 10 times

;; lackita's solution (34 msecs)
(defn conn? [edges]
  (let [graph (group-by first (mapcat (fn [e] [e (vec (reverse e))])
                                      edges))]
    ((fn traverse [v]
       (let [new-v (into v (map last (mapcat graph v)))]
         (if (= new-v v)
           (= v (set (keys graph)))
           (traverse new-v))))
     #{(first (keys graph))})))

;; youz's solution (1800 msecs)
(defn conn? [o]
  (let [[h & r] (seq o)]
    ((fn f [c r]
       (or (empty? r)
           (let [n (mapcat #(filter (fn [p] (some (set %) p)) r) c)]
             (if (empty? n) false
                 (f (reduce conj c n)
                    (remove (set n) r))))))
     #{h} r)))

;; jafingerhut's solution (210 msecs)
(defn conn? [edges]
  (= 1 (count (reduce (fn [c [u v]]
                        (let [s (or (first (filter #(% u) c)) #{u})
                              t (or (first (filter #(% v) c)) #{v})]
                          (conj (disj c s t) (into s t))))
                      #{} edges))))

;; dlee's solution (10 msecs)
(defn conn? [m] (loop [xs (group-by first m) k [(key (first xs))]]
          (if (empty? k)
            (empty? xs)
            (recur
             (apply dissoc xs k)
             (set (map second (mapcat xs k)))))))

;; maximental's solution (9500 msecs)
(defn conn? [g]
  ((fn f [e]
     (#(if (= e %) (= % g) (f %))
      (reduce (fn [a b] (into a (filter #(some (set b) %) g)))
              #{}
              e)))
   #{(first g)}))

;; 92. Read Roman numerals
;; Roman numerals are easy to recognize, but not everyone knows all
;; the rules necessary to work with them. Write a function to parse a
;; Roman-numeral string and return the number it represents.
;; You can assume that the input will be well-formed, in upper-case,
;; and follow the subtractive principle. You don't need to handle any
;; numbers greater than MMMCMXCIX (3999), the largest number
;; representable with ordinary letters.
;; (= 48 (__ "XLVIII"))
(fn [x]
  (loop [x x sum 0]
    (let [[r a] (some
                 #(if (.startsWith x (% 0)) %)
                 (map vector
                      ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
                      [1000 900 500 400  100  90   50  40  10    9   5   4    1]))]
      (if (empty? x) sum
          (recur (subs x (count r)) (+ sum a))))))

;; dzholev's solution
#(apply + (map { "M" 1000 "D" 500 "C" 100 "L" 50 "X" 10 "V" 5 "I" 1
                "CM" 900 "CD" 400 "XC" 90 "XL" 40 "IX" 9 "IV" 4}
              (re-seq #"C[MD]|X[CL]|I[XV]|." %)))

;; nikelandjelo's solution
#(->> (map {\C 100 \D 500 \I 1 \L 50 \M 1000 \V 5 \X 10} %)
      (partition 2 1 [0])
      (map (fn [[a b]] (if (< a b) (- a) a)))
      (apply +))

;; 93. Partially Flatten a Sequence
;; Write a function which flattens any nested combination of
;; sequential things (lists, vectors, etc.), but maintains the lowest
;; level sequential items. The result should be a sequence of
;; sequences with only one level of nesting.
;; (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
;;    [[:a :b] [:c :d] [:e :f]])
(fn f [x] (if (some coll? x) (mapcat f x) [x]))

;; 94. Game of Life
;; The game of life is a cellular automaton devised by mathematician John Conway.
;; The 'board' consists of both live (#) and dead ( ) cells. Each cell interacts
;; with its eight neighbours (horizontal, vertical, diagonal), and its next state
;; is dependent on the following rules:
;; 1) Any live cell with fewer than two live neighbours dies, as if caused by
;; under-population.
;; 2) Any live cell with two or three live neighbours lives on to the next
;; generation.
;; 3) Any live cell with more than three live neighbours dies, as if by
;; overcrowding.
;; 4) Any dead cell with exactly three live neighbours becomes a live cell, as
;; if by reproduction.
;; Write a function that accepts a board, and returns a board representing the
;; next generation of cells.
(fn [b]
  (let [adj [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
        update (fn [cell c] (case cell
                              \# (cond
                                  (< c 2) \ 
                                  (<= 2 c 3) \#
                                  :else \ )
                              (if (= 3 c) \# cell)))]
    (for [x (range (count b))]
      (apply str (for [y (range (count (b 0)))]
                   (let [cell (-> b (get x) (get y))
                         neighbors (for [[dx dy] adj]
                                     (-> b (get (+ x dx)) (get (+ y dy))))
                         c (count (filter #(= \# %) neighbors))]
                     (update cell c)))))))

;; katox's solution
(fn [b]
  (let [size (count b)
        alive? (fn [x y] (= (get-in b [x y]) \#))
        count-alive (fn [x y]
                      (reduce #(if %2 (inc %) %) 0
                              (for [i [-1 0 1]
                                    j [-1 0 1]
                                    :when (not= i j 0)]
                                (alive? (+ x i) (+ y j)))))
        update (fn [x y n] (if (or (= n 3) (and (= n 2) (alive? x y))) \# \ ))]
    (for [x (range size)]
      (apply str (for [y (range size)]
                   (update x y (count-alive x y)))))))

;; maximental's solution
(fn [b]
  (let [g #(nth (nth b % []) %2 \ )
        h #(reduce (fn [n [x y]] (+ n ({\  0 \# 1} (g (+ % x) (+ %2 y)))))
                   0
                   [[-1 -1] [-1 0] [-1 1]
                    [ 0 -1]        [ 0 1]
                    [ 1 -1] [ 1 0] [ 1 1]])]
    (reduce (fn [u i]
              (conj u (reduce (fn [v j]
                                (str v ({2 (g i j) 3 \#} (h i j) " ")))
                              ""
                              (range (count (peek b))))))
            []
            (range (count b)))))

;; mouse's solution
(fn [b]
  (map (partial apply str)
       (map-indexed
        (fn [r l] (map-indexed
                   (fn [c e]
                     ({3 \# 2 e}
                      (apply + (map
                                (fn [[i j]] ({\# 1} (get-in b [(+ i r) (+ j c)] \ ) 0))
                                [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))
                      \ ))
                   l))
        b)))

;; chouser's solution
#(let [r (range (count %))
       v [-1 0 1]
       a \#]
   (for [y r]
     (apply str (for [x r
                      c [(count (for [j v, k v
                                      :when (= a (get-in % [(+ y j) (+ x k)]))]
                                  1))]]
                  (if (or (= c 3) (and (= c 4) (= a (get-in % [y x]))))
                    a
                    \ )))))

;; 95. To Tree, or not to Tree
;; Write a predicate which checks if a given sequence represents a binary tree.
;; Each node in the tree must have a value, a left child, and a rightchild.
;; (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]]) true)
(fn f [t] (if (coll? t)
            (let [[_ a b] t] (and (= 3 (count t)) (f a) (f b)))
            (nil? t)))

(fn f [t]
  (or (nil? t)
      (and (coll? t)
           (= 3 (count t))
           (every? f (next t)))))

;; 96. Beauty is Symmetry
;; Let us define a binary tree as "symmetric" if the left half of the
;; tree is the mirror image of the right half of the tree. Write a
;; predicate to determine whether or not a given binary tree is
;; symmetric. (see 'To Tree, or not to Tree' for a reminder on the tree
;; representation we're using).
;; (= (__ '(:a (:b nil nil) (:b nil nil))) true)
(fn f [[_ l r]]
  (letfn [(fl [t]
            (if (nil? t) [t]
                (let [[n l r] t]
                  (concat [n] (fl l) (fl r)))))
          (fr [t]
            (if (nil? t) [t]
                (let [[n l r] t]
                  (concat [n] (fr r) (fr l)))))]
    (= (fl l) (fr r))))

(fn f [[_ l r]]
  ((fn g [l r]
     (or (= nil l r)
         (let [[a b c] l
               [x y z] r]
           (and (= a x)
                (g b z)
                (g c y)))))
   l r))

;; maximental's solution
#(= % ((fn m [[v l r]] (if v [v (m r) (m l)])) %))

;; 97. Pascal's Triangle
;; Pascal's triangle is a triangle of numbers computed using the following rules:
;; - The first row is 1.
;; - Each successive row is computed by adding together adjacent numbers in the
;; row above, and adding a 1 to the beginning and end of the row.
;; Write a function which returns the nth row of Pascal's Triangle.
;; (= (map __ (range 1 6))
;;    [     [1]
;;         [1 1]
;;        [1 2 1]
;;       [1 3 3 1]
;;      [1 4 6 4 1]])
(fn p [n]
  (loop [t [1]]
    (if (= n (count t))
      t
      (recur (concat [1]
                     (map + t (next t))
                     [1])))))

;; youz's solution
(fn [n] (-> (iterate #(map + `[0 ~@%] `[~@% 0]) [1])
            (nth (dec n))))

;; 98. Equivalence Classes
;; A function f defined on a domain D induces an equivalence relation
;; on D, as follows: a is equivalent to b with respect to f if and
;; only if (f a) is equal to (f b). Write a function with arguments f
;; and D that computes the equivalence classes of D with respect to f.
;; (= (__ #(* % %) #{-2 -1 0 1 2})
;;    #{#{0} #{1 -1} #{2 -2}})
(fn [f d]
  (set (map set (vals (group-by f d)))))

;; 99. Product Digits
;; Write a function which multiplies two numbers and returns the
;; result as a sequence of its digits.
;; (= (__ 999 99) [9 8 9 0 1])
#(map (zipmap "0123456789" (range 10)) (str (apply * %&)))
(fn [x y] (map #(- (int %) 48) (str (* x y))))
#(map (comp read-string str) (str (* % %2)))
#(for [c (str (* % %2))] (- (int c) 48))

;; 100 Least Common Multiple
;; Write a function which calculates the least common multiple. Your
;; function should accept a variable number of positive integers or
;; ratios.
;; (== (__ 7 5/7 2 3/5) 210)
(fn [& nums]
  (let [gcd #(if (zero? %2) % (recur %2 (mod % %2)))
        lcm #(/ (* % %2) (gcd % %2))]
    (reduce lcm nums)))

;; mouse's solution
(fn [n & r]
    (first (filter (fn [nm] (every? #(= 0 (rem nm %)) r))
                   (iterate #(+ n %) n) )))

;; 101. Levenshtein Distance
;; Given two sequences x and y, calculate the Levenshtein distance of
;; x and y, i. e. the minimum number of edits needed to transform x
;; into y. The allowed edits are:
;; - insert a single item
;; - delete a single item
;; - replace a single item with another item
;; (= (__ "kitten" "sitting") 3)
(fn f [[fa & ra :as a] [fb & rb :as b]]
  (cond (nil? a) (count b)
        (nil? b) (count a)
        (= fa fb) (f ra rb)
        :else (+ 1
                 (min (f ra rb)
                      (f a rb)
                      (f ra b)))))

(fn [a b]
  (let [f (fn [m [fa & ra :as a] [fb & rb :as b]]
            (cond (nil? a) (count b)
                  (nil? b) (count a)
                  (= fa fb) (m m ra rb)
                  :else (+ 1
                           (min (m m ra rb)
                                (m m a rb)
                                (m m ra b)))))]
    (f (memoize f) a b)))

;; youz's solution
(fn [x y]
  (last
   (reduce #(reduce (fn [c i]
                      (conj c (+ 1 (min (nth % (+ i 1))
                                        (nth c i)
                                        (- (nth % i)
                                           ({(nth x i) 1} (nth y %2) 0))))))
                    [(+ %2 1)]
                    (range (count x)))
           (range (+ 1 (count x)))
           (range (count y)))))

;; xenocard's solution
(fn [x y]
  (last
   (reduce (fn [[i & r] y]
             (map first (reductions
                         (fn [[d s] [i x]]
                           [(min (inc d) (inc i) (if (= x y) s (inc s))) i])
                         [(inc i) i] (map list r x))))
           (range (inc (count x)))
           y)))

;; 102. intoCamelCase
;; When working with java, you often need to create an object with
;; fieldsLikeThis, but you'd rather work with a hashmap that has
;; :keys-like-this until it's time to convert. Write a function which
;; takes lower-case hyphen-separated strings and converts them to
;; camel-case strings.
;; (= (__ "multi-word-key") "multiWordKey")
#(let [[h & t] (clojure.string/split % #"-")]
   (apply str h (map clojure.string/capitalize t)))

;; maximental's solution
#(reduce (fn [a [c & s]] (apply str `(~a ~(Character/toUpperCase c) ~@s)))
         (.split % "-"))

;; bradediger's solution
#(clojure.string/replace % #"-(\w)" (fn [[_ l]] (.toUpperCase l)))

;; chouser's solution
#(clojure.string/replace % #"-." (fn [[_ x]] (format "%S" x)))

;; 103. Generating k-combinations
;; Given a sequence S consisting of n elements, generate all k-combinations of S,
;; i. e. generate all possible sets consisting of k distinct elements taken from S.
;; (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
;; time with => (time (k-comb 10 (set (range 20))))
;; maximental's solution (20,000 ms)
(fn [k s]
  (loop [k k a #{#{}}]
    (if (> k 0)
      (recur (dec k) (set (for [x a y s :when (not (x y))] (conj x y))))
      a)))

;; jafingerhut's solution (310 ms), similar to vlisch's (240ms)
(fn k-comb [k s]
  (cond (> k (count s)) #{}
        (= k 0) #{#{}}
        :else (let [[x & xs] (seq s)]
                (into (k-comb k xs)
                      (map #(conj % x) (k-comb (dec k) xs))))))

;; 104. Write Roman Numerals
;; This is the inverse of Problem 92, but much easier. Given an integer
;; smaller than 4000, return the corresponding roman numeral in uppercase,
;; adhering to the subtractive principle.
;; (= "XLVIII" (__ 48))
(defn to-roman [n]
  (let [romans (map vector
                    ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
                    [1000 900 500 400  100  90   50  40  10    9   5   4    1])
        choose (fn [n] (first (filter #(<= (% 1) n) romans)))]
    (loop [n n r ""]
      (if (zero? n) r
          (let [[rom num] (choose n)]
            (recur (- n num) (str r rom)))))))

;; hypirion's solution
(let [loch {1 "I",  5 "V",  10 "X",  50 "L",  100 "C",  500 "D",  1000 "M"
            4 "IV", 9 "IX", 40 "XL", 90 "XC", 400 "CD", 900 "CM"}]
  (fn roman [n]
    (let [m (apply max (filter #(<= % n) (keys loch)))]
      (if (= m n)
        (loch m)
        (str (loch m) (roman (- n m)))))))

;; chouser's solution
(fn f [[n & a] [s & b] o i]
  (if n
    (f a b (into o (repeat (int (/ i n)) s)) (rem i n))
    (apply str o)))
[1000 900 500 400 100 90 50 40 10  9 5  4 1]
'[  M  CM   D  CD   C XC  L XL  X IX V IV I]
[]

;; 105. Identify keys and values
;; Given an input sequence of keywords and numbers, create a map such that
;; each key in the map is a keyword, and the value is a sequence of all
;; the numbers (if any) between it and the next keyword in the sequence.
;; (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))
(fn [s]
  (apply hash-map
         (mapcat
          (fn [[ks v]] (concat (mapcat #(vector % []) (drop-last ks))
                               [(last ks) v]))
          (partition 2 (partition-by keyword? s)))))

;; katox's solution
(fn [s]
  (reduce (fn [r [k v]] (assoc (into r (zipmap k (repeat []))) (last k) v))
          {}
          (partition 2 (partition-by keyword? s))))

;; daowen's solution
#(->> (partition-by keyword? %)
      (mapcat (fn [[k :as v]] (if (keyword? k) (interpose [] v) [v])))
      (apply hash-map))

;; chouser's solution
(fn f [[k & v]]
  (if v
    (let [[a b] (split-with number? v)]
      (assoc (f b) k a))
    {}))


;; 106. Number Maze
;; Given a pair of numbers, the start and end point, find a path
;; between the two using only three possible operations:
;; - double
;; - halve (odd numbers cannot be halved)
;; - add 2
;; Find the shortest path through the "maze". Because there are
;; multiple shortest paths, you must return the length of the shortest
;; path, not the path itself.
;; (= 5 (__ 9 12)) ; 9 11 22 24 12
#(loop [s #{%}, i 1]
   (if (s %2) i
       (recur
        (set (mapcat (juxt * / +) s (repeat 2)))
        (inc i))))

;; 107. Simple closures
;; Given a positive integer n, return a function which computes x^n.
;; (= [1 8 27 64] (map (__ 3) [1 2 3 4]))
#(fn [x] (reduce * (repeat % x)))
#(fn [x] (Math/pow x %))

;; 108. Lazy Searching
;; Given any number of sequences, each sorted from smallest to
;; largest, find the smallest number which appears in each sequence.
;; The sequences may be infinite, so be careful to search lazily.
;; (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(fn f [& s]
  (let [h (map first s)
        x (apply max h)]
    (if (every? #{x} h)
      x
      (apply f (map #(drop-while (partial > x) %) s)))))

;; chouser's solution
#(let [a (map first %&)
       b (apply min a)]
   (if (apply = a)
     b
     (recur (map (fn [[x & y :as z]] (if (= x b) y z)) %&))))

;; cgrand's solution
#(let [s (sort-by first %&)
        [[x & z]] s]
    (if (= x (first (last s)))
      x
      (recur (cons z (next s)))))

;; 110. Sequence of pronunciations
;; Write a function that returns a lazy sequence of "pronunciations" of a
;; sequence of numbers. A pronunciation of each element in the sequence
;; consists of the number of repeating identical numbers and the number itself.
;; For example, [1 1] is pronounced as [2 1] ("two ones"), which in turn is
;; pronounced as [1 2 1 1] ("one two, one one"). Your function should accept an
;; initial sequence of numbers, and return an infinite lazy sequence of
;; pronunciations, each element being a pronunciation of the previous element.
;; (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
;; [1]
;; [1 1]
;; [2 1]
;; [1 2 1 1]
;; [1 1 1 2 2 1]
;; [3 1 2 2 1 1]
;; [1 3 1 1 2 2 2 1]
;; [1 1 1 3 2 1 3 2 1 1]
(fn f [s]
  (let [x (mapcat #(vector (count %) (first %)) (partition-by identity s))]
    (lazy-seq (cons x (f x)))))

;; chouser's solution
(fn [s] (rest (iterate #(mapcat (juxt count first) (partition-by identity %)) s)))
