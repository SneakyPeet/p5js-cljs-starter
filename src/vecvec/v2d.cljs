(ns vecvec.v2d)


(defn- rand-n
  ([] (rand-n 1))
  ([n] (let [n (rand n)
             neg (rand-int 100)]
         (if (even? neg)
           n
           (* -1 n)))))


(defn new [x y] [x y])


(defn x
  "Returns the x value of a vector"
  [[vx _]] vx)


(defn y
  "Returns the y value of a vector"
  [[_ vy]] vy)


(defn random
  "Returns a random 2d floating point number vector with x and y between 0 (inclusive) and
  n (default 1) (exclusive)"
  ([n] [(rand-n n) (rand-n n)])
  ([] (random 1)))


(defn add
  "Adds Vector 1 to Vector 2"
  ([[v1x v1y] [v2x v2y]]
   [(+ v1x v2x) (+ v1y v2y)]))


(defn abs [v]
  (map js/Math.abs v))
