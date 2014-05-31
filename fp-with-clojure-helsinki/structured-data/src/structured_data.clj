(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (v 0) (v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[a _] [b _]]]
  (- b a))

(defn height [[[_ a] [_ b]]]
  (- b a))

(defn square? [[[a b] [c d]]]
  (even? (+ a b c d)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and (<= x1 p1 x2)
         (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[a b] [c d]] inner]
    (and (contains-point? outer [a c])
         (contains-point? outer [b d]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [[_ y]] y)]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) 
          (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [aname (:name author)
        adeath (:death-year author)
        abirth (:birth-year author)]
    (if abirth
      (str aname " (" abirth " - " adeath ")")
      aname)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [quantity (count books)]
    (cond
      (= 0 quantity) "No books."
      (= 1 quantity) (str "1 book. " (book->string (first books)) ".")
      :else (str quantity " books. " 
              (apply str 
                (interpose ". "
                  (map book->string books))) "."))))

(defn books-by-author [author books]
  (let [book-has-author? 
          (fn [book] (has-author? book author))]
    (filter book-has-author? books)))

(defn author-by-name [name authors]
  (let [has-name? 
          (fn [author] (= name (:name author)))]
    (first (filter has-name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
