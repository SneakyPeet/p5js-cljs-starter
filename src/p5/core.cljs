(ns p5.core
    (:require [vecvec.v2d :as v2d]))

(enable-console-print!)

;;;; STATE

(def size 500)


(defn dots [] (->> (range 50)
                   (map #(hash-map :position (v2d/abs (v2d/random size))
                                   :color [(rand-int 255) (rand-int 255) (rand-int 255)]))))


(defn move [dots]
  (map #(update % :position v2d/add (v2d/random 2)) dots))


(defonce *state (atom (dots)))


;;;; P5

(defn draw-dot [dot]
  (let [{:keys [position color]} dot
        [x y] position
        [r g b] color]
    (js/fill r g b)
    (js/ellipse x y 10 13)))


(defn setup []
  (js/createCanvas 500 500)
  (js/rectMode "CENTER")
  (js/noStroke))


(defn draw []
  (js/background 240)
  (doseq [dot @*state]
    (draw-dot dot))
  (swap! *state move))


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
