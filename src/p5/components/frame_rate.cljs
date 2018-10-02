(ns p5.components.frame-rate)


(defonce *frame-rate-element (atom nil))

(def *refresh? (atom true))

(def refresh-rate 500)


(defn- create-frame-rate-element []
  (let [e (-> (js/createP "")
              (.style "font-size" "40px")
              (.style "position" "fixed")
              (.style "top" "0px")
              (.style "left" "0px")
              (.style "font-family" "sans-serif")
              (.style "margin" "20px"))]
    (reset! *frame-rate-element e)))


(defn- has-shown? []
  (some? @*frame-rate-element))


(defn bound-color [n]
  (if (number? n)
    (min 255 (max 0 n))
    0))


(defn- draw [r g b]
  (let [r (bound-color r)
        g (bound-color g)
        b (bound-color b)
        color (str "rgb(" r "," g "," b ")")
        rate (js/floor (js/frameRate))]
    (-> @*frame-rate-element
        (.style "color" color)
        (.html rate))))


(defn show
  ([]
   (show 100))
  ([greyscale]
   (show greyscale greyscale greyscale))
  ([r g b]
   (when-not (has-shown?)
     (create-frame-rate-element))
   (when @*refresh?
     (reset! *refresh? false)
     (js/setTimeout #(reset! *refresh? true) refresh-rate)
     (draw r g b))))
