(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (recur (rest coll))))

(defn max-element [a-seq]
  (when (seq a-seq)
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
    (let [size1 (count seq-1)
          size2 (count seq-2)]
      (if (> size1 size2)
        seq-1
        seq-2)))

(defn longest-sequence [a-seq]
  (let [[fst snd & xs] a-seq
        e-max (seq-max fst snd)]
    (if (empty? xs)
      e-max
      (recur (conj (vec xs) e-max)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    []
    (let [[x & xs] a-seq
          coll (my-filter pred? xs)]
      (if (pred? x)
        (cons x coll)
        coll))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (let [[x & xs] a-seq]
      (if (= x elem)
        true
        (recur elem xs)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [[x & xs] a-seq]
      (if (pred? x)
        (cons x (my-take-while pred? xs))
        []))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [[x & xs] a-seq]
      (if (not (pred? x))
        a-seq
        (recur pred? xs)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (not (empty? a-seq)) (empty? b-seq)) false
    :else (let [[x & xs] a-seq
                [y & ys] b-seq]
            (if (not (= x y))
              false
              (recur xs ys)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) 
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) 
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat 
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [coll (reverse a-seq)]
    (if (empty? coll)
      '(())
      (map reverse (tails coll)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [size (count a-seq)
          rotate (fn [xs] (conj (vec (rest xs)) (first xs)))]
      (take size (iterate rotate a-seq)))))

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
  (count-elem-helper 0 elem coll))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elems (distinct a-seq)
          quantity (fn [elem] (count-elem elem a-seq))]
      (zipmap elems (map quantity elems)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [repeat-freq (fn [map-item] 
                      (let [k (first (seq map-item))
                            v (second (seq map-item))]
                        (repeat v k)))]
    (apply concat (map repeat-freq a-map))))

(defn my-take [n coll]
    (if (or (<= n 0) (empty? coll))
      '()
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (recur (dec n) (rest coll))))

(defn halve [a-seq]
  (let [size-to-take (int (/ (count a-seq) 2))]
    [(my-take size-to-take a-seq)
     (my-drop size-to-take a-seq)]))

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

