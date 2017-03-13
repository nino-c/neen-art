(ns common.mathematical
  (:require [common.util :refer :all] 
            [common.core :refer :all] 
            [quil.core :refer :all]
            [clojure.math.combinatorics :as combo]
            [clojure.core.matrix :as mat]))


;; ============================
;; Domain / codomain

(defn linspace [min max delta]
  (append (map float (range min max delta)) max))

(defn range-width [rng]
  (+ (Math/abs (first rng)) (Math/abs (last rng))))

;; ============================
;; Random

(defn round* [x precision]
  (double (/ (round (* x precision)) precision)))

(defn nth-triangular [n]
  (last (map #(reduce + (range %)) (range n))))

;; ============================
;; Linear algebra


(defn A-x [theta] 
  (mat/matrix [[1                   0                    0          ]
               [0                   (cos theta)          (sin theta)]
               [0                   (* (sin theta) -1)   (cos theta)]]))

(defn A-y [theta]                   
  (mat/matrix [[(cos theta)         0                    (* (sin theta) -1)]
               [0                   1                    0                 ]
               [(sin theta)         0                    (cos theta)       ]]))

(defn A-z [theta]                   
  (mat/matrix [[(cos theta)         (sin theta)          0]
               [(* (sin theta) -1)  (cos theta)          0]
               [0                   0                    1]]))

(defn rotation-matrix [ang-x ang-y ang-z]
  (mat/mmul (A-z ang-z) (A-y ang-y) (A-x ang-x)))


;; ============================
;; Polynomials

(defn multivariate-polynomial [degree coefficients]
  (let [terms* (for [x (range (inc degree))
                     y (range (inc degree))
                     :when (and 
                             (<= (+ x y) degree)
                             (> (+ x y) 0))]
                 {:x x :y y})
        terms  (map #(assoc %1 :coeff %2) 
                    terms* (cycle coefficients))]
    
    {:terms    terms
     :func     (fn [x y & rest]
                 (apply + (map (fn [term]
                                 (* (:coeff term)
                                    (pow x (:x term))
                                    (pow y (:y term)))) terms)))
     :string   (reduce 
                 (fn [a b] (str a " + " b)) 
                 (map 
                   (fn [term] 
                     (str (:coeff term) 
                          "x^" (:x term)
                          "y^" (:y term))) 
                   terms))}))
               
(defn random-polynomial
  ([degree] (random-polynomial degree 100))
  ([degree ^Integer coefficient-variance]
    (let [num-terms    (- (nth-triangular (+ degree 3)) 1)
          coefficients (take num-terms 
          (repeatedly #(round* (- (* (rand) 
                                     coefficient-variance) 
                                  (/ coefficient-variance 2)) 100)))]
      ;(println (str num-terms " terms!"))
      (multivariate-polynomial degree coefficients))))


;; ============================
;; Extrema

(defn get-extrema [seq]
  [(reduce (fn [a b] (if (< b a) b a)) seq)
   (reduce (fn [a b] (if (> b a) b a)) seq)])

(defn get-extrema-of [seq index]
  (get-extrema 
    (map #(nth % index) seq)))

(defn nested-col-extrema
  "Given nested vectors, returns max from n-th column"
  [nested-list n-th]
  (let [z-vals (map #(nth % n-th) (reduce concat [] nested-list))]
    (get-extrema z-vals)))



;; ============================
;; Other

(defn pulse
  "Generates an orbit about a value"
  [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid  (+ low half)
        x    (sin (* (t) (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn orbit [width speed]
  ;(* width (sin (* (t) (float rate))))
  (let [half-width (/ width 2)]
    (pulse (* half-width -1) half-width speed)))



