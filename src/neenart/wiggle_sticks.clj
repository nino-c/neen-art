(ns neenart.wiggle-sticks
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

(def lines-per-group 80)
(defn t [] (float (/ (millis) 1000)))


(defn draw-state [state]
  (background 0 0 1)
  (doseq [sticks (:sticks state)]
    (stroke (:hue sticks) 0.4 1)
    (stroke-weight (:line-width state))
    (let [[start-x start-y] (mo/+ [0 0] [(/ (width) -2) (/ (height) -2)])
          dy                (* 2 (:line-width state))]
      (push-matrix)
      (translate [(/ (width) 2) (/ (height) 2)])
      (scale 2)
      (rotate (:angle sticks))
      (loop [i      0
             [x y]  [start-x start-y]]
        (when (< i lines-per-group)
          (apply line [x y (+ x (width)) y])
          (recur (inc i) [x (+ y dy)])))
      (pop-matrix)
    )))

(defn update-state [state]
  (-> state
    (assoc :sticks (map (fn [stick]
           (let [angle-delta   (* (:index stick) (/ PI 500))
                 current-angle (:angle stick)]
             (assoc stick :angle (+ current-angle 
                                    (* angle-delta 
                                       (cos (* (t) (+ 1 (/ (:index stick) 0.3))))))))) (:sticks state)))))

(defn create-stick-group [index]
  {:angle 0
   :index index
   :hue   (* (/ (* 2 PI) 3) index)})

(defn setup []
  (frame-rate 30)
  (color-mode :hsb (* 2 PI) 1 1)
  (blend-mode :blend)
  {:sticks     (map create-stick-group (range 3))
   :line-width (/ (/ (min (width) (height)) lines-per-group) 2)})

(defsketch wiggle-sticks
  :host "id-of-canvas-if-html"
  :size [800 500]
  :setup setup
  :draw draw-state
  :update update-state
  :middleware [qm/fun-mode])

(defn -main [& args]
  (println "Tootaloot."))

