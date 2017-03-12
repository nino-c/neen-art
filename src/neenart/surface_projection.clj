(ns neenart.surface_projection
  (:require [clojure.core.matrix :as mat]
            [clojure.core.matrix.impl.pprint :as mprint]
            [clojure.core.matrix.operators :as mo]
            [clojure.pprint :refer :all]
            [neenutil.core :refer :all]
            [quil.core :refer :all]
            [quil.helpers.seqs :refer :all]
            [quil.middleware :as qm]))


(alter-var-root #'*debug* (fn [arg] true))
(defn t [] (float (/ (millis) 1000)))

(def x-range [-1.1 1.1])
(def y-range [0 1.2])
(def domain (for [x (range (first x-range) (last x-range) 0.05)
                  y (range (first y-range) (last y-range) 0.05)] [x y]))

(def projection-matrix (mat/matrix [[1 0 0]
                                   [0 1 0.5]]))

(defn pulse
  "Generates an orbit about a value"
  [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid  (+ low half)
        x    (sin (* (t) (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn to-matrix [p]
  (mat/matrix (map #(list %) p)))

(defn range-width [rng]
  (+ (Math/abs (first rng)) (Math/abs (last rng))))

(defn get-extrema [seq]
  [(reduce #(if (< %2 %1) %2 %1) seq)
   (reduce #(if (> %2 %1) %2 %1) seq)])

(defn get-extrema-of [seq index]
  (get-extrema 
    (map #(nth % index) seq)))

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

(defn update-state [state] state)

(defn draw-surface [points]
  (background 0 0 100)
  (stroke 0 0 80)
  
  (let [points2d (map 
                   (fn [p] 
                     (conj 
                       (mat/mmul projection-matrix p) (nth p 2))) 
                   points)]
    (let [x-lim (get-extrema-of points2d 0)
          y-lim (get-extrema-of points2d 1)
          z-lim (get-extrema-of points2d 2)]
      ;(println "extrema of domain: x <- " x-lim ", y <- " y-lim ", z <- " z-lim)
      (push-matrix)
      (translate (/ (width) 2) (/ (height) 2))
      (doseq [[x y z] points2d]
        (let [x' (map-range 
                   x (first x-lim) (second x-lim) (/ (width) -2) (/ (width) 2))
              y' (map-range
                   y (first y-lim) (second y-lim) (/ (height) -2) (/ (height) 2))]
          (fill (/ PI 3) 50 90)
          ;(rect x' y' 10 (- (height) y'))
          (ellipse x' y' 10 10)
          ))
      (pop-matrix))))
  
(defn draw-surface* [func]
  (background 0 0 100)
  (stroke 0 0 80)
  (let [points (map
                 (fn [pt]
                   [(first pt) (second pt) (func (first pt) (second pt))])
                 domain)]
    (println "extrema of codomain: " (get-extrema-of points 2))
    (let [points2d (map
                    (fn [pt]
                      (conj 
                        (mat/mmul projection-matrix pt) (nth pt 2)))
                    points)
          x-lim    (get-extrema-of points2d 0)
          y-lim    (get-extrema-of points2d 1)
          z-lim    (get-extrema-of points2d 2)]
      ;(println points2d)
      (println "--extrema of domain: x <- " x-lim ", y <- " y-lim ", z <- " z-lim)
      (let [slices   (group-by #(nth % 1) points2d)]
        (pprint slices)))))

(defn draw-state [state]
  ;(println (:string (:surface-function state)))
  (let [f (:func (:surface-function state))]
    (draw-surface* f)
;    (let [fmap (map 
;                 (fn [p] 
;                   [(first p) (second p) (f (first p) (second p))]) 
;                 domain)]
      ;(println "extrema of codomain: " (get-extrema-of fmap 2))
      ;(draw-surface fmap))
      )
  state)

(defn setup []
  (frame-rate 30)
  (no-loop)
  (color-mode :hsb (* 2 PI) 100 100 100)
  {:surface-function (random-polynomial 5)})

(defsketch surface_projection
  :host "id-of-canvas-if-html"
  :size [800 500]
  :setup setup
  :draw draw-state
  :update update-state
  :middleware [qm/fun-mode])

(defn -main [& args]
  (println "Tootaloot."))

