(ns neenart.landscape3
  (:require [common.core :refer :all]
            [common.util :refer :all :include-macros true]
            [common.mathematical :refer :all]
            ;[numeric.expresso.core :as e]
            [quil.core :refer :all]
            [quil.middleware :as qm]
            [clojure.math.combinatorics :as combo]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.operators :as mo]
            [clojure.core.matrix.utils :as mu]
            [clojure.core.matrix.impl.pprint :as mp]
            [clojure.test :as test]))


;; ===========================
;; Settings

(def x-range (linspace -2 2 0.5))
(def y-range (linspace 0 2 0.5))
(def polynomial-degree 5)

;; ===========================
;; Coordinate system
;;   
;; The screen is the `x-z` plane
;;   `y` comes "out of the screen"
;;
;; the "camera" location
;;
;; ==========================
;; Names
;;
;; Domain
;; Image
;;   Domain + Image = Vertices

(def viewpoint [0 0 0])
;;
;; the direction the camera is looking

(def direction-vector [0 1 0])
;;
;; the base domain for the surface function
;;   set of ordered pairs: (x,y)

;(def domain-x-y
;  (for [y y-range x x-range] [x y]))


(def domain-x-y (sort-by (fn [[x y]] [y x]) 
                         (combo/cartesian-product 
                           x-range y-range)))

(defn transform-domain
  "Takes domain as list of ordered pairs and returns
   a map with the original ordered pairs as keys and
   a transformation thereof as the values"
  [func]
  (apply array-map (interleave domain-x-y (map func domain-x-y))))

(def domain-mapped (transform-domain (fn [[x y]] [x y 0.0])))

;(defn sort-vertices
;  "Takes computed function as mapping of points
;   and returns as slices ready to draw on screen"
;  [vertices]
;  (println (str "vertices&&&&" vertices))
;  (sort-by (fn [[k v]] [(second k) (first k)]) vertices))

(defn compute-codomain
  [func domain]
  (println (str "compute: " func "--" domain))
  (apply array-map (interleave domain-x-y 
                               (map (fn [[x y _]] 
                                      [x y (func x y)]) 
                                    (vals domain)))))

;(defn get-vertices
;  "Computes function, unites image with domain, 
;   stores in state map"
;  [func domain]
;  (println (str "====domain" domain))
;  (zipmap (keys domain) (map (fn [[k [x y _]]] 
;                              [x y (func x y)]) domain)))

(defn pre-transform-domain
  "Applies transformation to entire domain,
   and retains a mapping back to the original
   domain for drawing purposes"
  [matrix domain]
  (transform-domain (mat/mmul (rotation-matrix 0 0 (/ PI 3)) matrix)))


;; ==========================
;; Quil functions

; take original domain 
;   - transform into slice-rows
; iterate through the slice-rows
;   - use the items in slice-rows
;       to lookup the keys on the
;       map storing vertices

(defn update-state [state]
  (assoc state 
         :codomain (->> domain-mapped
                     (pre-transform-domain (rotation-matrix 0 0 (/ PI 3)))
                     )))

(defn draw-state [state]
 (background 0 0 100)
 (let [codomain     (:codomain state)
       slices       (->> codomain
                      (group-by (fn [[k v]] (second k))))
       z-extrema    (get-extrema-of (vals codomain) 2)
       scale-x      (/ (width)  (range-width x-range))
       scale-y      (/ (width)  (range-width x-range))
       scale-z      (/ (height) (range-width z-extrema))
       height*      (* (height) scale-z)]
   
   (push-matrix)
   (center-origin)
   (scale scale-x scale-z)
   
   (doseq [y-val (keys slices)]
     (println "slices -- vals")
     (println (vals (get slices y-val)))
     (fill (+ PI (- (random 0.5) 0.25)) 30 70 20)
      (begin-shape)
      (vertex (- 0 (/ (width) 2)) (* height* 2))
      (doseq [[x _ y] (vals (get slices y-val))]
        (vertex x y))
      (vertex (/ (width) 2) (* height* 2))
      (end-shape :close))
   (pop-matrix))
 state)

(defn setup []
 (frame-rate 3)
 (no-loop)
 (color-mode :hsb (* 2 PI) 100 100 100)
 (stroke-weight 0.0005)
 (stroke 0 0 10)
 (let [surface-function   (random-polynomial 
                            polynomial-degree)
       codomain           (->> domain-mapped
                            (compute-codomain (:func surface-function)))
       rotated            (->> domain-mapped
                            (pre-transform-domain (rotation-matrix 0 0 (/ PI 3))))]
   (println rotated)
   {:surface-function     surface-function
    :codomain             codomain}))

(defsketch landscape
  :title      "Polynomial Landscape"
  :size        [800 600]
  :middleware  [qm/fun-mode]
  :setup       setup
  :draw        draw-state)
  

(defn -main [& args]
  (println "I make beeg one."))

 