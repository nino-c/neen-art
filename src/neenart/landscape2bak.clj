(ns neenart.landscape2
  (:require [quil.core :refer :all]
            [quil.middleware :as qm]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.impl.pprint :as mprint]
            [clojure.core.matrix.operators :as mo]
            [common.core :refer :all]
            [common.util :refer :all :include-macros true]
            [common.mathematical :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;
; Settings

(def x-range [-3 3])
(def y-range [0 5])

(def num-sections 25)

(def projection-matrix (mat/matrix [[1 0 0]
                                   [0 1 0.5]]))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Number crunching

(defn compute-surface
  "Computes function and returns 2-dimensional array of values"
  [func & args]
  (let [x-range*   (map float (range (first x-range)
                                     (last x-range)
                                     (/ (range-width x-range) num-sections)))
        y-range*   (map float (range (first y-range)
                                     (last y-range)
                                     (/ (range-width y-range) num-sections)))]
    (let [vertices (map (fn [y] 
                          (map (fn [x] 
                                 [x y (func x y)]) 
                               x-range*)) 
                        y-range*)]
      (if (contains? (set args) :flattened)
          (reduce concat vertices)
          vertices))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions

(defn vertices-to-segments 
  "Takes a list of vertices and returns a list of line segments"
  [vertices]
  (map #(vec (list (nth vertices %) (nth vertices (+ % 1)))) 
       (range (- (count vertices) 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Construction of polynomial

(defn rand-or-zero [a b prob-of-zero]
  (if (<= (rand) prob-of-zero)
    0
    (* (- (rand-int (+ (abs a) (abs b))) 
         (/ (+ (abs a) (abs b)) 2))
       (sin (* (t) (rand-int 6))))))


(defn random-polynomial [degree]
  (let [terms (for [x (range 1 (inc degree))
                    y (range 1 (inc degree))
                    :when (and 
                       (<= (+ x y) degree)
                       (> (+ x y)) 0)]
                {:coeff (rand-or-zero -10 10 0.3) 
                   :x x 
                   :y y})] 
    {:terms  terms
     :func   (fn [x y]
               (apply + (map 
                          (fn [term]
                            (* (:coeff term)
                               (pow x (:x term))
                               (pow y (:y term)))) 
                          terms)))
     :string (reduce 
               (fn [a b] (str a " + " b)) 
               (map 
                 (fn [term] 
                   (str (:coeff term) 
                         "x^" (:x term)
                         "y^" (:y term))) 
                 terms))}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rendering / drawing functions

;(defn render-surface-vertices
;  "Render surface using `vertex` instead of primitive shapes (sphere, box)"
;  [vertex-rows]
;  
;  ; x: x range fills entire width of screen
;  ; y: take y to be the axis "coming out of the screen"
;  ;     - as deep as half of the width of the x-range
;  ;        -- same scale as x
;  ; z: take z to be the traditional y-axis
;  ;     - take height to be the difference between extrema 
;  ;        of the image of function + some padding
;  
;  (center-origin)
;  (let [z-extrema (nested-col-extrema vertex-rows 2)
;        scale-x (/ (q/width) (range-width x-range))
;        scale-y (/ (q/width) (range-width x-range))
;        scale-z (/ (q/height) (range-width z-extrema))]
;    
;    (q/rotate 0)
;    (q/scale scale-x scale-y scale-z)
;    (q/stroke-weight 0.01)
;    (q/fill 0 250 0 60)
;    
;    ;(doseq [row (map points-to-segments vertex-rows)]
;    ;  (println row)
;    ;  )
;    
;    (doseq [vertex-row vertex-rows]
;      ;(println (points-to-segments vertex-row))
;      (q/begin-shape)
;      (doseq [[x y z] vertex-row]
;        ;(q/push-matrix)
;        ;(q/translate x z y)
;        (q/vertex x y z))
;      (q/end-shape))))



(defn render-surface-vertices
  "Render surface using `vertex` instead of primitive shapes (sphere, box)"
  [vertex-rows]
  (let [vertices-flattened (reduce concat vertex-rows)]
    (let [z-extrema (nested-col-extrema vertex-rows 2)
          scale-x   (/ (width)  (range-width x-range))
          scale-y   (/ (width)  (range-width x-range))
          scale-z   (/ (height) (range-width z-extrema))]
      (println "extrema of domain: z <- " z-extrema)
      ;;;;;
      (push-matrix)
      (center-origin)
      (rotate 0)
      (scale scale-x scale-z)
      (stroke-weight 0.01)
      (fill 0 100 60)
      (doseq [vertex-row vertex-rows]
      ;(println (points-to-segments vertex-row))
      (begin-shape)
      (doseq [[x y] vertex-row]
        (vertex x y))
      (end-shape))
      (pop-matrix)
      )))

;(defn draw-surface [func]
;  "Appects a (mathematical) function and renders the surface"
;  (let [vertex-rows (compute-surface func)
;        points3d (reduce concat vertex-rows)]
;    ;(println (str "vertices------" (list vertex-rows)))
;    (let [x-lim (get-extrema-of points3d 0)
;          y-lim (get-extrema-of points3d 1)
;          z-lim (get-extrema-of points3d 2)]
;      (println "extrema of domain: x <- " x-lim ", y <- " y-lim ", z <- " z-lim)
;      
;      (let [scale-x (float (/ (width) (range-width x-range)))
;            scale-y scale-x
;            scale-z (float (/ (height) (Math/abs (- (second z-lim) (first z-lim)))))]
;        ;(println (str "scale" scale-x ", " scale-z))
;        (push-matrix)
;        (center-origin)
;        (scale scale-x scale-z)
;        (stroke 0 0 50)
;        (stroke-weight 2)
;        (doseq [vertex-row vertex-rows]
;          (loop [vertex-row' vertex-row]
;            (println "vertex-row''" vertex-row')
;            (let [x (nth (first vertex-row') 0)
;                  y (nth (first vertex-row') 2)]
;              (println (str "count vertex-row' = " (second vertex-row')))
;              (let [x' (nth (second vertex-row') 0)
;                    y' (nth (second vertex-row') 2)]
;                  (println (str "(" x ", " y ") -> (" x' ", " y' ")"))
;                  (line x y x' y')
;                  (if (> (count vertex-row') 1) 
;                    (recur (rest vertex-row'))))
;              )))
;        (pop-matrix)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Quil / animation

(defn update-state [state] 
  state)

(defn draw-state [state]
  (background 0 0 100)
  (let [func (:func (:surface-function state))
        vertex-rows (compute-surface func)]
    ;;;
    (render-surface-vertices vertex-rows))
  state)

(defn setup []
  (frame-rate 30)
  (no-loop)
  (color-mode :hsb (* 2 PI) 100 100 100)
  {:surface-function (random-polynomial 5)})

(defn on-key-down [state event]
  (println state)
  state)

(defsketch landscape
  :host "host"
  :title "Polynomial Landspace"
  :size [700 700]
  :setup setup
  :draw draw-state
  :update update-state
  :middleware [qm/fun-mode]
  :key-pressed on-key-down)

(defn -main [& args]
  (println "I make beeg one."))

