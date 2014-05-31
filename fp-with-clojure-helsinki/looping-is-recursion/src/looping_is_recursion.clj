(ns looping-is-recursion)

(defn power [base exp]
  (let [f (fn [b e acc] 
            (cond
              (= b 0) 0
              (= e 0) acc
              :else (recur b (dec e) (* acc b))))]
    (f base exp 1)))

(defn last-element [a-seq]
  (when (seq a-seq)
    (let [[x & xs] a-seq]
      (if (empty? xs)
        x
        (recur xs)))))

(defn seq= [seq1 seq2]
  (if (empty? seq2)
    (= seq1 seq2)
    (let [[x & xs] seq1
          [y & ys] seq2]
      (if (not (= x y))
        false
        (recur xs ys)))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) n
      :else (recur (inc n) (rest s)))))

(defn avg [a-seq]
  (/ (apply + a-seq) (count a-seq)))

(defn parity [a-seq]
  (let [items (set a-seq)
        odd-times? (fn [x] (odd? (count (filter #{x} a-seq))))]
    (set (filter odd-times? items))))

(defn fast-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (loop [x 0 y 1 z n]
            (if (= (dec z) 1)
              (+ x y)
              (recur y (+ x y) (dec z))))))

(defn cut-at-repetition [a-seq]
  (loop [ini (first a-seq)
         xs  (rest a-seq)
         new-seq [ini]]
    (if (or (empty? xs)
            (= (first xs) ini))
      new-seq
      (recur ini (rest xs) (conj new-seq (first xs))))))

