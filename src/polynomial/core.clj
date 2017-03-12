(ns polynomial.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
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
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn get-extrema [vec] 
  [(reduce #(if (< %2 %1) %2 %1) vec)
   (reduce #(if (> %2 %1) %2 %1) vec)])

(defn nested-col-extrema
  "Given nested vectors, returns max from n-th column"
  [nested-list n-th]
  (let [z-vals (map #(nth % n-th) (reduce concat [] nested-list))]
    ;(pprn/pprint (str "z-vals" (into [] z-vals)))
    ;(pprn/pprint (str "MAX=" (str (max (into [] z-vals)))))
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
  (/ 1 (q/cos
         (+ (* (pulse 0.8 1.6 15) x x)
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


;;;;;;;;;;;;;;;;;;;;;;;;;
; graph-layout functions

(defn range-width [rng]
  (+ (Math/abs (first rng)) (Math/abs (last rng))))

(def x-range [-3 0])
(def y-range [-3 3])

(defn center-origin [] (q/translate (/ (q/width) 2) (/ (q/height) 2) 0))

(defn draw-axes []
  (q/push-matrix)
  (center-origin)
  (q/rotate 0)
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
  
  (let [vertices (map (fn [y]
                        (into [] (map (fn [x] [x y (func x y)])
                                      (range (first x-range)
                                             (last x-range)
                                             (/ (range-width x-range) 15)))))
                      (range (first y-range)
                             (last y-range)
                             (/ (range-width y-range) 15)))]
    
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
  
  ; x: x range fills entire width of screen
  ; y: take y to be the axis "coming out of the screen"
  ;     - as deep as half of the width of the x-range
  ;        -- same scale as x
  ; z: take z to be the traditional y-axis
  ;     - take height to be the difference between extrema 
  ;        of the image of function + some padding
  
  (center-origin)
  (let [z-extrema (nested-col-extrema vertex-rows 2)
        scale-x (/ (q/width) (range-width x-range))
        scale-y (/ (q/width) (range-width x-range))
        scale-z (/ (q/height) (range-width z-extrema))]
    
    (q/rotate 0)
    (q/scale scale-x scale-y scale-z)
    (q/stroke-weight 0.01)
    (q/fill 0 250 0 60)
    
    ;(doseq [row (map points-to-segments vertex-rows)]
    ;  (println row)
    ;  )
    
    (doseq [vertex-row vertex-rows]
      ;(println (points-to-segments vertex-row))
      (q/begin-shape)
      (doseq [[x y z] vertex-row]
        ;(q/push-matrix)
        ;(q/translate x z y)
        (q/vertex x y z))
      (q/end-shape))))



(defn draw3d
  "Main parent rendering function"
  [state]
  (q/background 250)
  (draw-axes)
  ;(dbg (compute-surface f*))
  (let [surface-vertices (compute-surface f2)]
    ;(println-str surface-vertices)
    (render-surface-vertices surface-vertices)))


; (defn update 
;   "Update drawing function"
;   [state]
;   state)

(defn setup []
  (q/print-camera)
  (q/print-projection)
  (q/lights)
  (q/frame-rate 1)
  (q/stroke 160)
  (q/stroke-weight 0.005)
  (q/background 250)
  (q/fill 50 230 (+ (* 20 (q/sin (t))) 230) 50)
  (center-origin)
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
