(ns neenart.landscape2
  (:require [quil.core :refer :all]
            [quil.middleware :as qm]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.operators :as mo]
            [clojure.core.matrix.utils :as mu]
            [clojure.core.matrix.impl.pprint :as mp]
            [common.core :refer :all]
            [common.util :refer :all :include-macros true]
            [common.mathematical :refer :all]
            ;[numeric.expresso.core :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;
; Settings

(def x-range [-1.3 1.3])
(def y-range [0 1.1])
(def polynomial-degree 5)
(def num-sections 30)

(def viewpoint [0 0 0])
(def direction-vector [0 1 0])
(def projection-matrix (mat/matrix [[1 0 0]
                                    [0 1 0.5]]))


;;;;;;;;;;;;;;;;;;;;;;;;;
; Number crunching

(defn get-domain [matrix]
  (let [x-range*   (append (map float 
                                (range (first x-range)
                                       (last x-range)
                                       (/ (range-width x-range) 
                                          (- num-sections 1)))) (last x-range))
        y-range*   (append (map float 
                                (range (first y-range)
                                       (last y-range)
                                       (/ (range-width y-range) 
                                          (- num-sections 1)))) (last y-range))
        vertices   (map #(map (fn [x] [x % 0]) x-range*) y-range*)]
    (->> 
      vertices
      (maplist #(mat/mmul % matrix)))
    vertices))

(defn projection-matrix* [y]
  "projection matrix as a function of y-value"
  (let [y* (lerp 1 0.4 (/ (- y (first y-range)) (range-width y-range)))]
    (mat/matrix [[y*   0   0.5]
                 [0.5  0   y*]])))

(defn compute-surface
  "Computes function and returns 2-dimensional array of values"
  [func domain]
  (maplist (fn [[x y]] [x y (func x y)]) domain))


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
  (if (< (rand) prob-of-zero)
    0
    (* (- (rand-int (+ (abs a) (abs b))) 
          (/ (+ (abs a) (abs b)) 2))
       (sin (* (t) (rand-int 6))))))


(defn random-polynomial [degree]
  "Returns a polynomial in 2 variables with random coefficients"
;  (let [terms (for [x (range (inc degree))
;                    y (range (inc degree))
;                    :when (and 
;                            (<= (+ x y) degree)
;                            (> (+ x y) 0))]
;                {:coeff (rand-or-zero -10 10 -10) 
;                 :x x 
;                 :y y})] 
;    
;    {:terms  terms
;     :func   (fn [x y]
;               (apply + (map (fn [term]
;                               (* (:coeff term)
;                                  (pow x (:x term))
;                                  (pow y (:y term)))) terms)))
;     :string (reduce 
;               (fn [a b] (str a " + " b)) 
;               (map 
;                 (fn [term] 
;                   (str (:coeff term) 
;                        "x^" (:x term)
;                        "y^" (:y term))) 
;                 terms))})
  (let [terms* (for [x (range (inc degree))
                       y (range (inc degree))
                       :when (and 
                               (<= (+ x y) degree)
                               (> (+ x y) 0))]
                   {:x x :y y})
        coefficients  (take 20 (repeatedly 
                                 #(- (* (rand) 20) 10)))
        terms  (map #(assoc %1 :coeff %2) 
                    terms* (cycle coefficients))]
      {:terms  terms
       :func   (fn [x y]
                 (apply + (map (fn [term]
                                 (* (:coeff term)
                                    (pow x (:x term))
                                    (pow y (:y term)))) terms)))
       :string (reduce 
                 (fn [a b] (str a " + " b)) 
                 (map 
                   (fn [term] 
                     (str (:coeff term) 
                          "x^" (:x term)
                          "y^" (:y term))) 
                   terms))})
  
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rendering / drawing functions

(defn render-surface-vertices
  [vertex-rows]
  ;(println "-----------VR")
  ;(println vertex-rows)
  (let [z-extrema (nested-col-extrema vertex-rows 2)
        scale-x   (/ (width)  (range-width x-range))
        scale-y   (/ (width)  (range-width x-range))
        scale-z   (/ (height) (range-width z-extrema))
        height*   (* (height) scale-z)]
    
    ;(println "extrema of domain: z <- " z-extrema)
    ;;;;;
    
    (push-matrix)
    (center-origin)
    (scale scale-x scale-z)
    ;(println (str "scale= " scale-x ", " scale-z))
    
    (doseq [vertex-row vertex-rows]
      
      (fill (+ PI (- (random 0.5) 0.25)) 30 70 20)
      (begin-shape)
      (vertex (- 0 (/ (width) 2)) (* height* 2))
      (doseq [[x _ y] vertex-row]
        (vertex x y))
      (vertex (/ (width) 2) (* height* 2))
      (end-shape :close))
    
    (pop-matrix)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Quil / animation

(defn update-surface [surface]
  )

(defn draw-state' [state] 
  (background 0 0 100)
  (println (:string (:surface-function state)))
  (render-surface-vertices (:vertex-rows state))
  state)

(defn update-state' [state] 
  (let [theta  (* (/ PI 10) (sin (/ (t) 2)))
        M      (rotation-matrix 0 0 theta)
        func   (:func (:surface-function state))
        dom    (get-domain theta)]
    (assoc state 
         :vertex-rows (compute-surface func dom)
         :theta theta)))

;(defn update-state [state] 
;  (-> state
;    (assoc 
;      :surface-function 
;      (update-surface (:surface-function state)))))
;
;(defn draw-state [state]
;  (background 0 0 100)
;  (let [func (:func (:surface-function state))
;        vertex-rows (compute-surface func)]
;    ;;;
;    ; rotate axes
;    (let [theta        (* (/ PI 10) (sin (/ (t) 2)))
;          M            (rotation-matrix 0 0 theta)
;          vertex-rows* (->> vertex-rows
;                           (map (fn [row] (->> row
;                                            (map mat/as-vector)
;                                            (map #(mat/mmul % M))
;                                            (map vec)))))]
;      (render-surface-vertices vertex-rows*)))
;  state)

(defn setup []
  (frame-rate 3)
  (no-loop)
  (color-mode :hsb (* 2 PI) 100 100 100)
  (stroke-weight 0.0005)
  (stroke 0 0 10)
  ;(println (mp/pm (rotation-matrix 0 0 0)))
  ;(println (get-domain (rotation-matrix 0 0 0)))
  ;(println "====================")
  ;(println (get-domain (rotation-matrix 0 0 1)))
  (update-state' {:surface-function (random-polynomial polynomial-degree)}))

(defn on-key-down [state event]
  (println state)
  state)

(defsketch landscape
           :host "host"
           :title "Polynomial Landspace"
           :size [700 700]
           :setup setup
           :draw draw-state'
           :update update-state'
           :middleware [qm/fun-mode]
           :key-pressed on-key-down)


(defn -main [& args]
  (println "I make beeg one."))

