# P5 CLJS STARTER KIT

A starter project for drawing with P5.js using clojurescript.

This project does not try and wrap P5.js. Instead it simply provides the ability to manage drawing state with clojurescript data types while allowing you to directly access the drawing power of p5.js.

see `src/vecvec/v2d.cljs` for some 2d vector helpers

## Example

Some random dots moving around the screen

```
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
  (frame-rate/show)
  (swap! *state move))

```

## Components

* Frame Rate: Toggle frame rate display using `(p5.components.frame-rate/show)`
* Child Canvas: A canvas that gets it's size from it's parent element `(p5.components.child-canvas/create-canvas "my-id")`


## Vector Math

For immutable 2d vectors and helper functions see `src/vecvec/v2d.cljs`.

A 2d vector is represented using normal clojure vectors `[x y]`. This allows for easy destructuring and immutability.

```
(let [[x y] (-> (v2d/random)
                (v2d/add (v2d/new 1 2)
                v2d/abs))]
  x)
```

## Setup

To get an interactive development environment run:

    lein figwheel

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min
