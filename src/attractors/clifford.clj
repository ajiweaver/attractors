(ns attractors.clifford
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;(def a -1.24458046630025)
;(def b -1.25191834103316)
;(def c -1.81590817030519)
;(def d -1.90866735205054)

;(def a -1.4)
;(def b 1.7)
;(def c 1.0)
;(def d 0.7)

(def a -1.3)
(def b 1.72)
(def c 1.3)
(def d 0.432)

(def init_loc {:x 0 :y 0.1})

(defn update-x
  "Update the x location"
  [x y]
  (+ (Math/sin (* a y))
     (* c (Math/cos (* a x)))))

(defn update-y
  "Update the y location"
  [x y]
  (+ (Math/sin (* b x))
     (* d (Math/cos (* b y)))))

(defn extend-state [state]
  (let [{:keys [x y]} (first state)
        next_x (update-x x y)
        next_y (update-y x y)]
    (cons {:x next_x
           :y next_y}
          state
          )))
(comment
  (cons 1 [1 2 3])
  (def state [{:x 0 :y 0}])
  (extend-state state)
  ; [{:x 0, :y 0} {:x -1.81590817030519, :y -1.90866735205054}]
  )

(defn gen-state
  [init_state N]
  (loop [counter 0
         state init_state]
    (if (>= counter N)
      state
      (let [new_state (extend-state state)]
        (recur (inc counter) new_state)))))
(comment
  (time (first (take 100000000 (repeat 1))))
  (time (last (take 100000 (repeat 1))))
  (time (extend-state [init_loc]))
  (time (gen-state [init_loc] 10000))
; [{:x 0, :y 0}
;  {:x -1.81590817030519, :y -1.90866735205054}
;  {:x 1.8481708102963523, :y 2.157001821533527}
;  {:x 0.768878037628628, :y 0.9894234289095707}]
  )

(defn rand-init []
  {:x (rand 0.1)
   :y (rand 0.1)})

(def N 100000)
(defn setup []
  (q/color-mode :hsb)
  (q/background 31)
  (q/frame-rate 15)
  (println "Generating state...")
  (update-state []))

(defn update-state [state]
  (if (q/key-pressed?)
    (do
      (println "stopped")
      (q/save-frame "clifford-attractor-#####.jpg")
      (q/no-loop)))
  (gen-state [(rand-init)] N))


(defn draw-points [state]
  (q/stroke 202 10)
  ;(q/background 31)
  (doseq [{:keys [x y]} state]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the next point
      (let [scale (/ (q/width) 6)]
        (q/point (* scale x) (* scale y))))))
(comment
  (and false true true true)
  (range 2 4)
  (def aa [1 2 3])
  (get aa 0) ; 1
  (count aa) ; 3
  (range 1 (count aa)) ; (1 2)
  )

;(defn draw-lines [state]
  ;(q/text (str "display density: " (q/display-density)) 10 20)
  ;(doseq [ii (range 1 (count state))]
    ;(let [previous (nth state (dec ii))
          ;current (nth state ii)]
     ;; Move origin point to the center of the sketch.
     ;(q/with-translation [(/ (q/width) 2)
                          ;(/ (q/height) 2)]
       ;; Draw the next point
       ;(let [scale (/ (q/width) 7)
             ;{x1 :x y1 :y} previous
             ;{x2 :x y2 :y} current]
         ;(q/line (* scale x1) (* scale y1)
                  ;(* scale x2) (* scale y2))))))
  ;(q/no-loop))

(comment
  (def aa (gen-state [init_loc] 10))
  )

(q/defsketch attractors
  :title "Clifford attractor"
  :size [1600 1000]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-points
  ;:draw draw-lines
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode]
  )
