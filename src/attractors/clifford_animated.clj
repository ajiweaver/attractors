(ns attractors.clifford-animated
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [attractors.clifford :as cl]))

(def N 500000)
(defn setup []
  (q/color-mode :hsb)
  (q/background 31)
  (q/frame-rate 15)
  (println "Generating state...")
  (time (update-state [])))

(defn update-state [state]
  (def cl/a (+ cl/a 0.01))
  (if (q/key-pressed?)
    (do
      (println "stopped")
      (q/save-frame "clifford-attractor-#####.jpg")
      (q/no-loop)))
  (time (cl/gen-state [(cl/rand-init)] N)))


(defn draw-points [state]
  (q/stroke 202 10)
  ;(q/background 31)
  (doseq [{:keys [x y]} state]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the next point
      (let [scale (/ (q/height) 4.1)]
        (q/point (* scale x) (* scale y))))))

(q/defsketch attractors
  :title "Animated clifford attractor"
  :size [1900 1200]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-points
  ;:draw draw-lines
  ;:features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode]
  )

