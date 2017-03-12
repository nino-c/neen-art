(ns neenart.mass-spring-with-damping
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

(defn t [] (float (/ (millis) 500)))
;; time is 2x faster than actual for now

(def colors ['(50 120 230)
             '(49 153 91)
             '(0 153 0)
             '(115 76 57)
             '(1 102 148)])

;; main body of program
(def num-balls 35)


(defn create-ball [index]
  (let [col-width   (/ (width) num-balls)
        ball        {:index               index
                     :mass                (+ 100 (* index 5))
                     :spring-constant     300
                     :damping-coefficient 5
                     :length              300
                     :color               (list (* (* 2 PI) (noise (/ index 10))) 50 50)
                     :radius              (* col-width 0.75)}]
    (let [equilibrium-position [(+ (* col-width index) (/ col-width 2))
                                (:length ball)]
          initial-displacement [0 -150]]
      (-> ball
          (assoc :equilibrium-position equilibrium-position
                 :initial-displacement initial-displacement
                 :position             (mo/+ equilibrium-position
                                             initial-displacement))))))

(defn update-ball-position [ball]
  (let [m     (:mass ball)
        k     (:spring-constant ball)
        b     (:damping-coefficient ball)
        A     (second (:initial-displacement ball))
        omega (sqrt (- (/ k m) (sq (/ b (* 2 m)))))]
    (-> ball
        (assoc :position
               (mo/+ (:equilibrium-position ball)
                     [0 (* A (exp (* (/ b (* -2 (:mass ball))) (t)))
                           (cos (* omega (t))))])))))


(defn update-state
  "quil's update-state step"
  [state]
  (-> state
      (assoc :balls
             (map update-ball-position (:balls state)))))

(defn draw-state [state]
  (background 0 0 100)
  (doseq [ball (:balls state)]
    (let [[x y] (:position ball)]
      ;; draw string
      (stroke-weight 2)
      (stroke 0 0 80)
      (line x 0 x y)
      ;; draw ball
      (apply fill (:color ball))
      (no-stroke)
      (apply ellipse
             (concat [x y] (repeat 2 (:radius ball)))))))

(defn setup []
  (frame-rate 30)
  (color-mode :hsb (* 2 PI) 100 100)
  {:balls (map create-ball (range num-balls))})

(defsketch mass-on-spring-with-damping
  :host "id-of-canvas-if-html"
  :size [800 500]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [qm/fun-mode])

(defn -main [& args]
  (println "Am Victoonius meinen Victoonium at the Tumerrades"))

