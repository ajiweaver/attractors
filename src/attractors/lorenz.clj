(ns attractors.lorenz
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [uncomplicate.neanderthal core native]))

(defn rand3
  "Random 3-D point"
  ([] (rand3 1))
  ([n] (map rand (take 3 (repeat n)))))
(comment
  (rand3) ; (0.32156523908043144 0.01163999433421814 0.6694365066286546)
  (rand3 10) ; (1.5107152316269967 2.742298764442549 6.596894999301836)

  (def a (dv 1 2 3))
  (let [point (dge 1 4 (list 1 1 1 1))
        Pr (dge 4 4 (list 1 0 0  0
                          0 1 0  0
                          0 0 -1 0
                          0 0 0  1))]
    (print point)
    (print Pr)
    (mm point Pr)
        )
  (dge 3 2 [1 2 3 4 5 6])
; #RealGEMatrix[double, mxn:3x2, layout:column, offset:0]
;    ▥       ↓       ↓       ┓
;    →       1.00    4.00
;    →       2.00    5.00
;    →       3.00    6.00
;    ┗                       ┛
;

  )

(defn step
  ([{:keys [a b c dt points]}]
   (step a b c dt (first points)))
  ([a b c dt last-point]
   (let [[x y z] last-point
         dx (* a (- y x))
         dy (- (* x (- b z)) y)
         dz (- (* x y) (* c z))
         nx (+ x (* dt dx))
         ny (+ y (* dt dy))
         nz (+ z (* dt dz))]
     (list nx ny nz))))
(comment
  (step {:a 10 :b 28 :c (/ 8 3) :dt 0.02 :points '((0 0 1))})
  ; (0.0 0.0 0.9466666666666667)
  )

(defn add-point
  [state]
  (let [new-point (step state)]
    (update state :points #(cons new-point %))))

(defn gen-points
  "Generate N points"
  ([{:keys [a b c dt points]} N]
   (gen-points a b c dt (first points) N))
  ([a b c dt init-point N]
    (reduce
      (fn [points point]
        (let [new-point (step a b c dt (first points))]
          (cons new-point points)))
      (list init-point)
      (take N (repeat 1)))))

(comment
  (time (cons '(1 1 1) '((0 0 0))))
  (add-point {:a 10 :b 28 :c (/ 8 3) :dt 0.02 :points '((0 0 1))})
  ; {:a 10, :b 28, :c 8/3, :dt 0.02, :points ((0.0 0.0 1.02) (0 0 1))}

  (def test-init-state {:a 10, :b 28, :c 8/3, :dt 0.02, :points '((2 1 1))})
  (def aa (time (gen-points test-init-state 100000)))
; ((2.122416 1.061208 1.061208)
;  (2.0808 1.0404 1.0404)
;  (2.04 1.02 1.02)
;  (2 1 1))

  (gen-points 10 28 8/3 0.02 '(2 1 1) 3)
; ((2.122416 1.061208 1.061208)
;  (2.0808 1.0404 1.0404)
;  (2.04 1.02 1.02)
;  (2 1 1))

; {:a 10,
;  :b 28,
;  :c 8/3,
;  :dt 0.02,
;  :points ((0 0N -245/27) (0 0 -5/3) (0 0 1))}
  (let [state {:points '((1 1))}]
    (update state :points #(cons '(1 2) %)))
  )

;(def N_points 80000)
(def N_points 10000)
(defn setup []
  (q/background 200)
  (q/frame-rate 10000)
  (q/color-mode :hsb)
  (let [init-state {:a 11
                    :b 28
                    :c 8/3
                    :dt 0.004
                    ;:points (list (rand3))}
                    :points (list (list 0.001 0.10 0.2))}
        points (time (gen-points init-state N_points))]
    (assoc init-state :points points)))

;(defn update-state [{:keys [points] :as state}]
  ;(if (> (count points) 50000)
    ;(do
      ;(q/no-loop))
    ;(let [points (time (gen-points state N_points))]
    ;(update state :points #(concat points %)))))

(defn update-state [state]
  (q/no-loop)
  state)

(defn project
  "Project a 3D point to 2D space"
  [[x y z]]
  (let [z' (* -1 z)
        x' (/ x z')
        y' (/ y z')]
    (list x' y')))

(def Pr (dge 4 4 (list 0 0  0  0
                       1 -2  0  0
                       0 -1 -1 -1
                       0 0  0  0)))
;(def Mv (dge 4 4 (list 1 0  1 2
                       ;0 10 1 2
                       ;0 0  1 5
                       ;0 0  0 1)))
(def Mv (dge 4 4 (list 1 2  1 0
                       0 1  1 0
                       1 0  1 0
                       2 2  5 1)))

(defn normalize [v]
  (let [mag (nrm2 v)]
    (scal mag v)))
(defn forward-vec [from to]
  (normalize (xpy to (scal -1 from))))
(defn cross-product [A B]
  (let [[ax ay az] (apply list A)
        [bx by bz] (apply list B)
        cx (- (* ay bz) (* az by))
        cy (- (* az bx) (* ax bz))
        cz (- (* ax by) (* ay bx))]
    (list cx cy cz)))
(defn right-vec [tmp forward]
  (let [ntmp (normalize tmp)]
    (cross-product ntmp forward)))
(defn up-vec [forward right]
  (cross-product forward right))
(defn look-at [from to tmp]
  (let [forward (forward-vec from to)
        right (right-vec tmp forward)
        up (up-vec forward right)
        [Tx Ty Tz] (apply list from)
        [fx fy fz] (apply list forward)
        [rx ry rz] (apply list right)
        [ux uy uz] (apply list up)
        ]
    (trans (dge 4 4 (list rx ux fx Tx
                          ry uy fy Ty
                          rz uz fz Tz
                          0  0  0  1)))
    ))

(def Mv (look-at (dv 0 -1.5 1) (dv 0.2 0.3 0) (dv 0 2 0)))

(comment
  (look-at (dv 0 1 0) (dv 0 0 1) (dv 1 0 0))
  (cross-product (dv 0 0 1) (dv 0 1 0))
  (right-vec (dv 0 1 0) (dv 1 0 0))
  (up-vec (dv 0 1 0) (dv 0 0 1))
  (let [[ax ay az] (apply list (dv 0 1 0))
        [bx by bz] (apply list (dv 0 0 1))
        cx (- (* ay bz) (* az by))
        cy (- (* az bx) (* ax bz))
        cz (- (* ax by) (* ay bx))]
    (print cx cy cz))
  (nrm2 (dv 1 1 1))
  (Math/sqrt 3)
  (normalize (dv 1 1 1))
  (normalize (xpy (scal -1 (dv 1 1 1)) (dv 2 2 2) ))
  (forward-axis (dv 1 1 1) (dv 2 2 2)))

(defn to-seq
  [m]
  (reduce
    (fn [new-seq p] (concat new-seq p))
    (list)
    m))

(defn project'
  "Project a 3D point to 2D space"
  [point]
  (let [q (dge 4 1 (concat point (list 1)))
        ;q' (mm q Pr)
        q' (mm Mv q)
        [x y z] (to-seq q')
        z' (* -1 z)
        x' (/ x z')
        y' (/ y z')]
    (list x' y')))
(comment
  (def aa (mm Mv (dge 4 1 (list 1 1 1 1))))
  (to-seq aa)

  (project (list 1 2 1))

  (project' (list 1 1 1))
  )

(defn scale
  [point alpha]
  (map #(* % alpha) point))

;(defn draw-state [{points :points}]
  ;(q/stroke 202 80)
  ;(let [[x y z] (first points)
        ;;current-point (first points)
        ;;previous-point (second points)
        ;;p1 (project (previous-point))
        ;;p2 (project current-point)
        ;z' (* -1 z)
        ;x' (/ x z')
        ;y' (/ y z')
        ;]
    ;(q/with-translation [(/ (q/width) 2)
                         ;(/ (q/height) 2)]
      ;(let [scale (* (q/height) 0.2)]
        ;(q/point (* scale x') (* scale y') ))))
        ;;(q/line p1 p2))))
      ;;(q/point x' y')))
  ;)

(defn draw-state [{all-points :points}]
  (q/stroke 30 150)
  ;(q/stroke 180 150)
  (println "Drawing...")
  (time (loop [points all-points]
    (if (< (count points) 2)
      (do
        (println "done!")
        (q/exit)
        (q/no-loop)
        )
      (let [current-point (first points)
            previous-point (second points)
            p1 (project' previous-point)
            p2 (project' current-point)
            alpha (* (q/height) 0.21)
            p1' (scale p1 alpha)
            p2' (scale p2 alpha)
            bound-check (fn [p]
                          (reduce
                            #(and %1 %2)
                            (map #(and (>= % -60) ( <= % 40)) p)))
            ]
      (if (and
            (bound-check p1)
            (bound-check p2))
        (q/with-translation [(/ (q/width) 2)
                           (/ (q/height) 2)]
        ;(let [[x y] p2']
          ;(q/point x y)
          ;)
        (q/line p1' p2')
        ))
      (recur (rest points)))))))
(comment
  (reduce #(and %1 %2) (map #(>= % 1) '(1 2 3)))
  (rest [1 2 3])
  (def aa (time (take-last 999998 (take 1000000 (repeat 1)))))
  (def aa (time (rest (rest (take 1000000 (repeat 1))))))
  (let [state
        (assoc test-init-state :points (gen-points test-init-state 10))]
    (draw-state state))
  )

(q/defsketch attractors
  :title "Lorenz attractor"
  :renderer :svg
  :output-file "/home/saulssto/generative_art/attractors/out/lorenz.svg"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode]
  )

