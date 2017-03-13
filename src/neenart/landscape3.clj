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

(def x-range (linspace -1.1 1.1 0.05))
(def y-range (linspace 0 2 0.1))
(def polynomial-degree 7)

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

(defn compute-codomain
  [func domain]
  ;(println (str "compute: "))
  ;(println (first domain))
  (apply array-map (interleave domain-x-y 
                               (map (fn [[x y _]] 
                                      [x y (func x y)]) 
                                    (vals domain)))))

(defn pre-transform-domain
  "Applies transformation to entire domain,
   and retains a mapping back to the original
   domain for drawing purposes"
  [matrix & domain]
  ;(println "===matr")
  ;(println domain)
  (transform-domain #(mat/mmul % matrix)))

;; ==========================
;; Quil functions

; take original domain 
;   - transform into slice-rows
; iterate through the slice-rows
;   - use the items in slice-rows
;       to lookup the keys on the
;       map storing vertices

(defn update-state [state]
  (let [theta  (* (/ PI 20) (t))
        matrix (rotation-matrix 0 0 theta)]
    (assoc state 
         :theta    theta
         :codomain (->> domain-mapped
                     (pre-transform-domain matrix)
                     (compute-codomain 
                       (:func 
                         (:surface-function state)))))))

(defn dot [x y]
  (push-style)
  (fill 0 50 50)
  (ellipse x y 0.05 0.05)
  (pop-style))

(defn draw-state [state]
 (background 0 0 100)
 (fill 50)
 (text (str "theta=" (:theta state)) 10 10)
 (println (str "theta=" (:theta state)))
 ;(println (:surface-function state))
 (ellipse 10 10 10 10)
 (let [codomain     (:codomain state)
       slices       (->> codomain
                      (group-by (fn [[k v]] (second k))))
       z-extrema    (get-extrema-of (vals codomain) 2)
       scale-x      (* 1 (/ (width)  (range-width x-range)))
       scale-y      (/ (width)  (range-width x-range))
       scale-z      (* -1 (/ (height) (range-width z-extrema)))
       height*      (* (height) scale-z)]
   
   (push-matrix)
   (center-origin)
   (scale scale-x scale-z)
   
   (push-style)
   (stroke-weight 0.01)
   ;(line (first x-range) 0 (last x-range) 0)
   (pop-style)
   
   ;(println "codomain---------")
   ;(println (first codomain) (last codomain))
   
   (doseq [y-val (keys slices)]
;     (println "slices -- vals")
;     (println (vals (get slices y-val)))
      
      (fill (+ PI (- (random 0.5) 0.25)) 30 70 20)
      (begin-shape)
      ;(println "%%%" (first x-range) "-" (range-width z-extrema))
      ;(println (vals (get slices y-val)))
      (vertex (first x-range) (* -0.5 (range-width z-extrema)))
      (doseq [[k [x _ y]] (get slices y-val)]
        ;(dot x y)
        (vertex (first k) y))
      (vertex (last x-range) (* -0.5 (range-width z-extrema)))
      (end-shape :close))
   (pop-matrix)) 
 state)

(defn setup []
 (frame-rate 30)
 ;(no-loop)
 (text-font (create-font "DejaVu Sans" 18 true))
 (color-mode :hsb (* 2 PI) 100 100 100)
 (stroke-weight 0.0005)
 (stroke 0 0 10)
 
 (let [surface-function   (random-polynomial 
                            polynomial-degree 5)
       surface-function'   (multivariate-polynomial polynomial-degree
                             '(0 -1 0 0 1))
       codomain           (->> domain-mapped
                            (pre-transform-domain (rotation-matrix 0 0 2))
                            (compute-codomain (:func surface-function)))]
   {:surface-function     surface-function
    :theta                2
    :codomain             codomain}))

(defsketch landscape
  :title      "Polynomial Landscape"
  :size        [800 600]
  :middleware  [qm/fun-mode]
  :setup       setup
  :draw        draw-state
  :update      update-state)
  

(defn -main [& args]
  (println "I make beeg one."))

 