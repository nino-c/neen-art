(ns neenart.pendulums
  (:require [clojure.core.matrix :as mat]
            [clojure.core.matrix.operators :as mo]
            [neenutil.core :refer :all]
            [quil.core :refer :all]
            [quil.helpers.seqs :refer :all]
            [quil.middleware :as qm]))

;; Utility functions
;; ---------------------------
;;    --> now imported from neenutil.core

(alter-var-root #'*debug* (fn [arg] true))

;; physical constants and variables
;; --------------------------------
;; basic unit of length here will be the pixel, not meter
;; ratio: 1 meter = 100 pixels

(def gravity 980)

;; 980 pixels/second^2 = 9.8 meters/second^2

(defn t [] (float (/ (millis) 1000)))

;; time is 1-to-1 normal speed

;; theta(t) = theta_0 e^(-b/2) sin(sqrt(omega^2 + (b/2)^2) * t + phi)
;; T = 2PI/omega = 2PI * sqrt(L/g)


;; main body of program
(def num-balls 25)

(defn create-ball [index]
  (let [L           (- (height) 75)
        ball-radius (* (/ (width) num-balls) 0.8)
        theta       (- (* PI (/ index num-balls)) (/ PI 2))]
    {:index                index
     :length               L
     :radius               ball-radius
     :original-angle       theta
     :damping-coefficient  0.1
     :color                (list (* (* 2 PI) (noise (/ index 10))) 80 30)
     :angle                theta}))

(defn draw-state [state]
  (background 0 0 100)
  (doseq [ball            (:balls state)]
    (let [L               (:length ball)
          g               gravity
          b               (:damping-coefficient ball)
          theta0          (:original-angle ball)
          omega           (sqrt (/ g L))
          T               (/ (* 2 PI) omega)
          theta           (* theta0
                             (exp (/ (* b (t)) -2))
                             (sin (* (sqrt (- (sq omega)
                                              (sq (/ b 2)))) (t))))
          top-center      [(/ (width) 2) 0]
          ball-center     (mo/+ top-center [0 L])
          x               (* (first ball-center) (cos theta))
          y               (* (second ball-center) (sin theta))]

      (push-matrix)
      (translate top-center)
      (rotate (/ PI 2))
      (stroke-weight 2)
      (stroke 0 0 80)
      (line 0 0 x y)
      (apply fill (:color ball))
      ;(apply fill (:color ball))
      (no-stroke)
      (apply ellipse
             (concat [x y]
                     (repeat 2 (:radius ball))))
      (pop-matrix))))

(defn setup []
  (frame-rate 30)
  (color-mode :hsb (* 2 PI) 100 100)
  ;(dbg (map create-ball (range 1 num-balls)))
  {:balls (map create-ball (range 1 num-balls))})

(defsketch pendulums
  :host "id-of-canvas-if-html"
  :size [800 500]
  :setup setup
  :draw draw-state
  :middleware [qm/fun-mode])

(defn -main [& args]
  (println "Am Victoonius meinen Victoonium at the Tumerrades"))

