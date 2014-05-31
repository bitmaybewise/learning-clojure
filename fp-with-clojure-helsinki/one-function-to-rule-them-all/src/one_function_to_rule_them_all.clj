(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (loop [[y & ys] a-seq
          new-seq []]
      (if (empty? ys)
        (conj new-seq y)
        (recur ys (conj new-seq y x))))))

(defn my-count [a-seq]
  (reduce + 0 (map (fn [x] 1) a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    a-seq
    (loop [[x & xs] a-seq
           new-seq []]
      (if (empty? xs)
        (cons x new-seq)
        (recur xs (cons x new-seq))))))

(defn min-max-element [a-seq]
  [(apply min a-seq)
   (apply max a-seq)])

(defn insert [sorted-seq n]
  (seq (set (cons n sorted-seq))))

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  (let [items (set a-seq)
        odd-times? (fn [x] (odd? (count (filter #{x} a-seq))))]
    (set (filter odd-times? items))))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more] 
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (loop [[a & as] more
                       b (* x y)]
                  (if (empty? as)
                    (* b a)
                    (recur as (* b a))))))

(defn pred-and
  ([]   (fn [x] true))
  ([x?] (fn [x] (x? x)))
  ([x? y?] (fn [x] (and (x? x) (y? x))))
  ([x? y? & more?] (fn [x] (reduce #(and % %2) (map #(% x) (conj more? x? y?))))))

(defn my-map [f a-seq]
  [:-])
