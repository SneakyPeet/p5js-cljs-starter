(ns p5.core
  (:require [vecvec.v2d :as v2d]
            [p5.components.frame-rate :as frame-rate]))

(enable-console-print!)

;;;; STATE

(def screen-size 800)
(def start-dots 150)

(def dot-radius 5)
(def start-direction-max-magnitude 2)

(def color-start 20)




(defn start-color []
  (case (rand-int 3)
    0 [color-start 0 0]
    1 [0 color-start 0]
    2 [0 0 color-start]))


(defn dot []
  {:position (v2d/abs (v2d/random screen-size))
   :direction (v2d/random start-direction-max-magnitude)
   :color (start-color)
   :expected-radius dot-radius
   :radius dot-radius})


(defn dots [] (->> (range start-dots)
                   (map #(dot))))


(defn move-dot [{:keys [position direction radius] :as dot}]
  (let [dot-jiggle (* 4 (/ dot-radius radius))
        new-position (-> position
                         (v2d/add direction)
                         (v2d/add (v2d/random dot-jiggle)))
        [px py] new-position
        [dx dy] direction
        dxn (if (or (>= radius px) (>= px (- screen-size radius))) (* -1 dx) dx)
        dyn (if (or (>= radius py) (>= py (- screen-size radius))) (* -1 dy) dy)
        new-direction (v2d/new dxn dyn)]
    (assoc dot :position new-position
           :direction new-direction)))


(defn move [dots]
  (map move-dot dots))


(defn drop-n
  [n coll]
  (let [coll (vec coll)]
    (concat (subvec coll 0 n)
            (subvec coll (inc n)))))


(defn drop-indexes
  [indexes coll]
  (loop [result coll
         indexes (-> indexes sort reverse)]
    (if (empty? indexes)
      result
      (recur (drop-n (first indexes) result)
             (rest indexes)))))


(defn collide [dots]
  (loop [result []
         dots dots]
    (if (empty? dots)
      (->> result (sort-by :radius) reverse)
      (let [dot1 (first dots)
            dots (rest dots)
            collisions
            (->> dots
                 (map-indexed
                  (fn [i dot2]
                    (let [d (v2d/dist (:position dot1) (:position dot2))
                          collide? (or (< d (* 0.75 (:radius dot1)))
                                       (< d (* 0.75 (:radius dot2))))]
                      (when collide? {:dot dot2
                                      :i i}))))
                 (remove nil?))]
        (if (empty? collisions)
          (recur (conj result dot1)
                 dots)
          (let [collision-indexes (map :i collisions)
                radius-increase (->> collisions
                                     (map #(get-in % [:dot :radius]))
                                     (reduce +))
                new-color (->> (conj collisions {:dot dot1})
                               (map #(get-in % [:dot :color]))
                               (reduce (fn [[r1 g1 b1] [r2 g2 b2]]
                                         [(+ r1 r2) (+ g1 g2) (+ b1 b2)]) [0 0 0]))
                new-dot (-> dot1
                            (assoc :color new-color)
                            (update :expected-radius + radius-increase))]
            (recur result
                   (conj (drop-indexes collision-indexes dots) new-dot))))))))


(defn grow-dot [{:keys [radius expected-radius] :as dot}]
  (if (> expected-radius radius)
    (update dot :radius inc)
    dot))


(defn grow [dots]
  (map grow-dot dots))


(defonce *state (atom (dots)))


;;;; P5

(defn draw-dot [dot]
  (let [{:keys [position color radius]} dot
        [x y] position
        [r g b] color]
    (js/fill r g b)
    (js/ellipse x y (* 2 radius) (* 2 radius))))


(defn setup []
  (js/createCanvas screen-size screen-size)
  (js/rectMode "CENTER")
  (js/noStroke))


(defn draw []
  (js/background 240)
  (doseq [dot @*state]
    (draw-dot dot))
  (frame-rate/show)
  (swap! *state #(-> %
                     move
                     collide
                     grow))
  #_(prn (count @*state)))


;;;; INIT

(doto js/window
  (aset "setup" setup)
  (aset "draw" draw))


;;;; FIGWHEEL

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! *state update-in [:__figwheel_counter] inc)
)
