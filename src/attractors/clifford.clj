(ns attractors.clifford
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;;(def a -1.24458046630025)
;;(def b -1.25191834103316)
;;(def c -1.81590817030519)
;;(def d -1.90866735205054)

;; triangular cradle

;(def a -1.4)
;(def b 1.7)
;(def c 1.0)
;(def d 0.7)

;; cradle / basket

;(def a -1.3)
;(def b 1.72)
;(def c 1.3)
;(def d 0.432)

;; isolated

;(def a -1.4)
;(def b -1.7)
;(def c -1.0)
;(def d -0.7)

;; bird

;(def a -1.1)
;(def b 2.72)
;(def c 1.3)
;(def d 0.432)

;; 4 dots

;(def a -1.1)
;(def b 1.72)
;(def c 0.3)
;(def d 0.432)

;; whale

;(def a -1.1)
;(def b 1.72)
;(def c 2.202)
;(def d 0.432)

(defn next-x
  "Update the x location"
  [{:keys [points a c]}]
  (let [{:keys [x y]} (first points)]
    (+ (Math/sin (* a y))
       (* c (Math/cos (* a x))))))

(defn next-y
  "Update the y location"
  [{:keys [points b d]}]
  (let [{:keys [x y]} (first points)]
    (+ (Math/sin (* b x))
       (* d (Math/cos (* b y))))))

(defn next-point [state]
  (let [next_point {:x (next-x state)
                    :y (next-y state)}]
    (-> state
        (update :points #(cons next_point %)))))
(comment
  (let [state {:a 1 :b 2 :c 1 :d 1 :points '({:x 0 :y 0})}]
    (next-point state))
  (let [points '({:x 1 :y 0})]
    (println points)
    (let [{:keys [x y]} (first points)]
      (println x)
      (println y))
    )
  (time (first (take 1e9 (repeat 1))))
  )


(defn add-points
  "Add `N` points to the :points key in `init-state`"
  [init-state N]
  (loop [counter 0
         state init-state]
    (if (>= counter N)
      state
      (let [new_state (next-point state)]
        (recur (inc counter) new_state)))))
(comment
  (time (first (take 100000000 (repeat 1))))
  (time (last (take 100000 (repeat 1))))
  (time (next-point [(rand-init)]))
  (let [state {:a 1 :b 2 :c 1 :d 1 :points '({:x 0 :y 0})}]
    (add-points state 10))
  )

(defn rand-init []
  {:x (rand 0.1)
   :y (rand 0.1)})

(def max-noise 0.001)

(defmacro update-parameter
  [mm param previous]
  `(assoc ~mm ~param (+ (rand max-noise) ~previous)))

(defn update-state [{:keys [a b c d] :as state}]
  ;(if (q/key-pressed?)
    ;(do
      ;(println "stopped")
      ;(q/save-frame "clifford-attractor-#####.jpg")
      ;(q/no-loop)))
  (-> state
      (update-parameter :a a)
      (update-parameter :b b)
      (update-parameter :c c)
      (update-parameter :d d)
      ;(assoc :a (+ a (rand max-noise)))
      ;(assoc :b (+ b (rand max-noise)))
      ;(assoc :c (+ c (rand max-noise)))
      ;(assoc :d (+ d (rand max-noise)))
      (assoc :points (list (rand-init)))
      (add-points N)
      )
  )
(comment
  (let [state {:a 1 :b 2 :c 1 :d 1 :points '({:x 0 :y 0})}]
    ;(update-state state)
    (update-parameter state :a 1)
    )
  (-> (init-triangular-cradle 10)
      (assoc :a (+ (rand 0.5) 1.0))
      (assoc :b (+ (rand 0.5) 1.0))
      (assoc :c (+ (rand 0.5) 1.0))
      (assoc :d (+ (rand 0.5) 1.0))
      )
  (rand 0.01)
  )

(defn init-triangular-cradle
  "Initialize the triangular cradle with N points"
  [N]
  (let [state {:a -1.4
               :b 1.7
               :c 1.0
               :d 0.7
               :points (list (rand-init))}]
    (add-points state N)))
(comment
  (-> (init-triangular-cradle 10)
      (assoc :a 2)
      (update-parameter :b))
  )

(def N 3500000)
(defn setup []
  (q/color-mode :hsb)
  (q/background 31)
  (q/frame-rate 15)
  (println "Generating state...")
  (time (init-triangular-cradle N)))

(defn draw-points [{:keys [a b c d points]}]
  (println a)
  (println b)
  (println c)
  (println d)
  (println "------------")
  (q/stroke 202 10)
  (q/background 31)
  (doseq [{:keys [x y]} points]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(- (/ (q/width) 2) 80)
                         (/ (q/height) 2)]
      ; Draw the next point
      (let [scale (/ (q/height) 4.1)]
        (q/point (* scale x) (* scale y)))))
  (q/save-frame "animated-clifford-attractor-######.jpg"))
(comment
  (and false true true true)
  (range 2 4)
  (def aa [1 2 3])
  (get aa 0) ; 1
  (count aa) ; 3
  (range 1 (count aa)) ; (1 2)
  )

(q/defsketch attractors
  :title "Clifford attractor"
  :size [1500 1200]
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
