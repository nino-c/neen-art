(ns landscape.camera_test
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pprn]
            [clojure.inspector :as inspector]))


(defn draw3d [state]
  (println state)
  ;(assoc state :navigation-3d {})
  )


(defn setup []
  (q/frame-rate 15)
  (q/no-loop)
  {:navigation-3d {} :points (parse-points (slurp "./points"))}
  )

(q/defsketch camera_test
  :host "host"
  :title "Camera Test"
  :size [700 700]
  :setup setup
  :draw draw3d
  :renderer :p3d
  :middleware [m/fun-mode m/navigation-3d]
  )

(defn -main [& args]
  (println "--Am Victoonius meinen Victoonium at the Tumerrades"))
