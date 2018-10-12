(ns p5.core
    (:require [vecvec.v2d :as v2d]))

(enable-console-print!)


(defn position [col-count i]
  (let [x (mod i col-count)
        y (/ (- i x) col-count)]
    [x y]))


(defn index [col-count x y] (+ x (* y col-count)))


(defn neighbours [row-count col-count cell]
  (let [{:keys [i x y]} cell
        x- (dec x)
        x+ (inc x)
        y- (dec y)
        y+ (inc y)
        neighbours [[x- y-] [x- y ] [x- y+]
                    [x  y-]         [x  y+]
                    [x+ y-] [x+ y ] [x+ y+]]]
    (->> neighbours
         (filter (fn [[x y]]
                   (and (>= x 0) (>= y 0)
                        (< x row-count) (< y col-count))))
         (map (fn [[x y]] (index col-count x y))))))


(defn cell [col-count i]
  (let [[x y] (position col-count i)]
    {:i i
     :type :empty
     :x x :y y
     :flagged? false
     :opened? false}))


(defn init-cells [row-count col-count]
  (let [size (* row-count col-count)]
    (->> (range size)
         (map #(cell col-count %))
         (map (juxt :i identity))
         (into {}))))


(defn place-bombs [bomb-count cells-map]
  (loop [bomb-count bomb-count
         available-spaces (set (keys cells-map))
         result cells-map]
    (if (<= bomb-count 0)
      result
      (let [bomb-index (nth (vec available-spaces) (rand-int (count available-spaces)))]
        (recur
         (dec bomb-count)
         (disj available-spaces bomb-index)
         (assoc-in result [bomb-index :type] :bomb))))))


(defn set-neighbours [row-count col-count cells-map]
  (->> cells-map
       (map (fn [[i cell]]
              (let [neighbours (neighbours row-count col-count cell)
                    bombs-touching (->> neighbours
                                        (map #(get cells-map %))
                                        (map :type)
                                        (filter #(= :bomb %))
                                        (count))]
                (assoc cell
                       :neighbours neighbours
                       :bombs-touching bombs-touching
                       :type (cond
                               (= :bomb (:type cell)) :bomb
                               (> bombs-touching 0) :bomb-adjacent
                               :else (:type cell))))))
       (map (juxt :i identity))
       (into {})))


(defn board [row-count col-count bomb-count]
  (let [size (* row-count col-count)
        cells (->> (init-cells row-count col-count)
                   (place-bombs bomb-count)
                   (set-neighbours row-count col-count))]
    cells))


(defn adjacent-open-cells
  [board i]
  (loop [cells-to-check #{i}
         cells-checked #{}
         cells-to-open []]
    (if (empty? cells-to-check)
      cells-to-open
      (let [i (first cells-to-check)
            {:keys [neighbours opened?] :as cell} (get board i)
            t (:type cell)
            should-open? (and (not= :bomb t) (not opened?))]
        (if-not should-open?
          (recur (disj cells-to-check i)
                 (conj cells-checked i)
                 cells-to-open)
          (let [neighbours-to-add (if-not (= :empty t)
                                    []
                                    (->> neighbours
                                         (filter #(not (contains? cells-checked %)))
                                         (filter #(not (contains? cells-to-check %)))))]
            (recur (into (disj cells-to-check i) neighbours-to-add)
                   (conj cells-checked i)
                   (conj cells-to-open i))))))))


;;;; STATE

(defonce *state (atom {:dead? false
                       :win? false
                       :board nil}))


(defn toggle-cell-flag [board i]
  (update-in board [i :flagged?] not))


(defn open-cell [board i]
  (let [cell (get board i)
        bomb? (= :bomb (:type cell))
        empty? (= :empty (:type cell))
        next-board
        (cond
          bomb? (->> (assoc-in board [i :killer?] true)
                       (map (fn [[i cell]]
                              [i (assoc cell :opened? true)]))
                       (into {}))
          empty? (reduce (fn [b i]
                             (assoc-in b [i :opened?] true))
                           board (adjacent-open-cells board i))
          :else (assoc-in board [i :opened?] true))
        win? (->> next-board
                  vals
                  (filter #(and (false? (:opened? %)) (not= :bomb (:type %))))
                  count
                  (= 0))]
    {:board next-board
     :dead? bomb?
     :win? win?}))

;;;; P5

(def size 20)
(def rows 30)
(def cols 30)
(def bombs 120)

(defn new-board [] (board rows cols bombs))

(defn setup []
  (let [board (new-board)]
    (swap! *state assoc :board board)
    (js/createCanvas (+ 1 (* rows size)) (+ 1 (* cols size)))
    (js/rectMode "CENTER")
    (js/noStroke)))


(defn draw-initial-cell []
  (if (:win? @*state)
    (js/fill 0 255 0)
    (js/fill 240))
  (js/rect 0 0 size size))


(defn draw-flagged-cell []
  (draw-initial-cell)
  (js/fill 255 150 0)
  (js/noStroke)
  (js/ellipse (/ size 2) (/ size 2) (/ size 3) (/ size 3)))


(defn draw-empty-cell []
  (js/fill 150)
  (js/rect 0 0 size size))


(defmulti draw-cell :type)


(defmethod draw-cell :empty [_]
  (draw-empty-cell))


(defmethod draw-cell :bomb [_]
  (draw-empty-cell)
  (js/fill 255 0 0)
  (js/noStroke)
  (js/ellipse (/ size 2) (/ size 2) (/ size 2) (/ size 2)))


(defmethod draw-cell :bomb-adjacent [cell]
  (draw-empty-cell)
  (js/noStroke)
  (js/fill (* 50 (:bombs-touching cell)) 0 0)
  (js/textSize 15)
  (js/text (str (:bombs-touching cell)) (/ size 2.2) (/ size 1.5)))


(defn draw []
  (doseq [[i {:keys [x y flagged? opened?] :as cell}] (:board @*state)]
    (js/push)
    (js/stroke 180)
    (js/translate (* x size) (* y size))
    (cond
      opened? (draw-cell cell)
      flagged? (draw-flagged-cell)
      :else (draw-initial-cell))
    (js/pop))
  (js/noLoop))


(defn mouse-clicked []
  (let [mx js/mouseX
        my js/mouseY
        in-bounds? (and (< mx (* size rows)) (< my (* size cols)))
        x (js/Math.floor (/ mx size))
        y (js/Math.floor (/ my size))
        i (index cols x y)
        ctrl-pressed? (= 16 (when js/keyIsPressed js/keyCode))
        dead? (:dead? @*state)
        win? (:win? @*state)
        f (cond
            (or dead? win?) #(hash-map :dead? false :board (new-board) :win? false)
            (not in-bounds?) identity
            ctrl-pressed? #(update % :board toggle-cell-flag i)
            :else #(open-cell (:board %) i))]
    (swap! *state f)
    (js/redraw)))

;;;; INIT

(doto js/window
  (aset "setup" setup)
  (aset "draw" draw)
  (aset "mouseClicked" mouse-clicked))


;;;; FIGWHEEL

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! *state update-in [:__figwheel_counter] inc)
)
