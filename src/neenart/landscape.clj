(ns landscape.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pprn]
            [clojure.inspector :as inspector]))


;;;;;;;;;;;;;;;;;;;;;;;;;
; Global helper functions

(defn t []
  (* 0.001 (q/millis)))

(defn pulse
  "Generates an orbit about a value"
  [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid  (+ low half)
        x    (q/sin (* (t) (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn get-extrema [vec]
  [(reduce #(if (< %2 %1) %2 %1) vec)
   (reduce #(if (> %2 %1) %2 %1) vec)])

(defn nested-col-extrema
  "Given nested vectors, returns max from n-th column"
  [nested-list n-th]
  (let [z-vals (map #(nth % n-th) (reduce concat [] nested-list))]
    (get-extrema z-vals)))


;;;;;;;;;;;;;;;;;;;;;;;;;
; Debugging functions

(defmacro dbg [& body]
  `(let [x# ~@body]
    (pprn/pprint (str "dbg: " (quote ~@body) "=" (x#))
                 x#)))

(defmacro dbg* [form]
  (pprn/pprint (eval form)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Mathematical functions

(defn coeff []
  (- (rand-int 10) 5))

(defn f [x y]
  (/ 1
     (q/cos
       (+ (* (pulse 0.8 1.6 0.05) x x)
          (* y y)))))

(defn f* [x y]
  (+ (* (pulse -5 5 10) x x x)
     (* (pulse 5 -5 7) x x y)
     (* (pulse -5 5 17) x y y)
     (* (pulse 5 -5 13) y y y)
     (* 2 x y)
     (* 1 x)
     (* -2 y)))

(defn f2 [x y]
  (* x x y (q/sin (* x y))))

(defn f3 [x y]
  (* x y (q/sin (* x y))))


;;;;;;;;;;;;;;;;;;;;;;;;;
; graph-layout functions

(defn range-width [rng]
  (+ (Math/abs (first rng)) (Math/abs (last rng))))

(def x-range [-3 3])

(def y-range [-3 3])

(defn center-origin [] (q/translate (/ (q/width) 2) (/ (q/height) 2) 0))

;(defn center-origin [] (q/translate 0 0 0))
;(defn center-origin [] (println "center-origin"))

(defn draw-axes []
  (q/push-matrix)
  (center-origin)
  (q/scale 5 -5 5)

  ; +x
  (q/fill 255 0 0)
  (q/box 111 1.5 1.5)

  ; +y
  (q/fill 0 255 0)
  (q/box 1.5 111 1.5)

  ; +z
  (q/fill 0 0 255)
  (q/box 1.5 1.5 111)

  (q/pop-matrix))


;;;;;;;;;;;;;;;;;;;;;;;;;
; Number crunching

(defn compute-surface
  "Computes function and returns 2-dimensional array of values"
  [func & args]

  (let [vertices (map
                   (fn [y]
                     (into []
                           (map (fn [x] [x y (func x y)])
                                (range (first x-range)
                                       (last x-range)
                                       (/ (range-width x-range) 25)))))
                   (range (first y-range)
                          (last y-range)
                          (/ (range-width y-range) 25)))]

    (if (contains? (set args) :flattened)
        (reduce conj vertices)
        vertices)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions

(defn points-to-segments
  "Takes a list of points and returns a list of line segments"
  [points]
  (map #(vec (list (nth points %) (nth points (+ % 1))))
       (range (- (count points) 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rendering / drawing functions

(defn render-surface-vertices
  "Render surface using `vertex` instead of primitive shapes (sphere, box)"
  [vertex-rows]

  (center-origin)
  ;(def a (reduce (fn [[x y z]] (list x y z)) (reduce concat vertex-rows)))

  (spit "./points"
        (clojure.string/join "" (map (fn [p] (str (clojure.string/join " " (map float p)) "\n"))
             (reduce concat vertex-rows))))
  (let [z-extrema (nested-col-extrema vertex-rows 2)
        scale-x   (/ (q/width) (range-width x-range))
        scale-y   (/ (q/width) (range-width x-range))
        scale-z   (/ (q/height) (range-width z-extrema))]

    (q/scale scale-x scale-y scale-z)
    (q/stroke-weight 0.01)
    (q/fill 0 250 0 30)

    (doseq [index (range (- (count vertex-rows) 1))]

      (let [segment-rows (->> vertex-rows
                              (map points-to-segments))]

        (doseq [vertices (map
                           #(concat %1
                             (reverse %2))
                           (nth segment-rows index)
                           (nth segment-rows (+ index 1)))]

          (let [extra-vertices (map (fn [[x y z]] [(- x 0.2) (- y 0.2) (- z 0.2)]) vertices)
                all-vertices   (concat vertices
                                       (combo/nth-permutation extra-vertices
                                                              (combo/permutation-index [\d \a \b \c])))]

            (q/begin-shape)
            (doseq [[x* y* z*] all-vertices]
              (q/vertex x* y* z*))
            (q/end-shape :close)))))))


(defn draw3d
  "Main parent rendering function"
  [state]
  (q/background 250)
  (q/stroke 160)
  (q/stroke-weight 0.001)

  (let [fov  (/ (Math/PI) 6)
        camZ (/ (/ (q/height) 2) (q/tan (/ fov 2)))]

    ;(println (str "camZ=" camZ))
    (q/begin-camera)
    ;(q/perspective fov, (/ (q/width) (q/height)) 0.001 3)
    (q/rotate-z (/ (Math/PI) -2))
    (q/rotate-y (/ (Math/PI) 2))
    (q/scale 1 1 -1)
    (q/translate 0 0 (/ (q/height) 2))
    ;(q/translate (* (q/cos (t)) 3) (* (q/sin (t)) 3) (/ (q/height) 2))

    (q/end-camera)
    ;(println (q/camera))
    ;(q/print-camera))

  ;(draw-axes)

  (let [surface-vertices (compute-surface f*)]
    (render-surface-vertices surface-vertices))))
    ;(q/camera (q/cos (t)) (q/sin (t)) (/ (q/height) 2) 0 0 (/ (q/height) 2) 0 0 1)))


(defn setup []
  (q/frame-rate 15)
  (q/no-loop)
  {:navigation-3d {}})


(defn on-key-down [state event]
  (println state)
  state)

(q/defsketch landscape
  :host "host"
  :title "Polynomial Landspace"
  :size [700 700]
  :setup setup
  :draw draw3d
  ;:update update
  :renderer :p3d
  :middleware [m/fun-mode m/navigation-3d]
  :key-pressed on-key-down)

(defn -main [& args]
  (println "Am Victoonius meinen Victoonium at the Tumerrades"))

