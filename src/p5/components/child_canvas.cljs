(ns p5.components.child-canvas)


(defn- resize [canvas parent-id options]
  (let [{:keys [width height]} options
        element (js/document.getElementById parent-id)
        width (or width (.-offsetWidth element))
        height (or height (.-offsetHeight element))]
    (.size canvas width height)))


(defn create-canvas
  "Creates a canvas that takes the size of it's parent element.
   Fix dimensions by passing :width or :height as options.
   Adds window resize event listener.
   Disable listener with :no-resize"
  [parent-id & options]
  (let [options (apply hash-map options)
        canvas (-> (js/createCanvas 0 0)
                   (.parent parent-id)
                   (resize parent-id options))]
    (when-not (:no-resize options)
      (js/window.addEventListener "resize" #(resize canvas parent-id options)))
    canvas))
