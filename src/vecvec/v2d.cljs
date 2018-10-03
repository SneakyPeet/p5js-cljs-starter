(ns vecvec.v2d)


(defn- rand-n
  ([] (rand-n 1))
  ([n] (let [n (rand n)
             neg (rand-int 100)]
         (if (even? neg)
           n
           (* -1 n)))))


(defn new
  "Creates a new 2d vector"
  [x y] [x y])


(defn x
  "Returns the x value of a vector"
  [[vx _]] vx)


(defn y
  "Returns the y value of a vector"
  [[_ vy]] vy)


(defn random
  "Returns a random 2d floating point number vector with x and y between -n  and
  n (exclusive) (n defaults to 1)"
  ([n] [(rand-n n) (rand-n n)])
  ([] (random 1)))


(defn add
  "Adds Vector 1 to Vector 2"
  ([[v1x v1y] [v2x v2y]]
   [(+ v1x v2x) (+ v1y v2y)]))


(defn abs
  "Returns a vector with x and y absolute"
  [v]
  (map js/Math.abs v))


(defn dist-sq
  "Calculates the squared euclidean distance between this vector and another"
  [[v1x v1y] [v2x v2y]]
  (let [dx (- v1x v2x)
        dy (- v1y v2y)]
    (+ (* dx dx) (* dy dy))))


(defn dist
  "Calculates the euclidean distance between this vector and another"
  [v1 v2]
  (js/Math.sqrt (dist-sq v1 v2)))

