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
