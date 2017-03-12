(ns neen.tester
  (:require [clojure.core.matrix :as mat]
            [clojure.core.matrix.operators :as mo]
            [neenutil.core :refer :all]
            [quil.core :refer :all]
            [quil.helpers.seqs :refer [steps seq->stream range-incl tap tally indexed-range-incl]]
            [quil.helpers.calc :refer [mul-add]]
            [quil.middleware :as qm]))


(defn n-seq [n]
  (seq->stream (repeatedly n starts-seq)))

;
;(defmacro n-seq [n]
;  `(let [~@(vec (map (fn [_] (list (symbol (str "aa" _)) '(starts-seq))) (range n)))
;          n-stream (seq->stream (map list ~@(map (fn [_] (list (symbol (str "aa" _)))) (range n))))]
;    n-stream))
;
;(defmacro n-seq [n]
;  `(seq->stream (map list ~@(vec (map (fn [_] (list 'starts-seq)) (range n))))))



(defn draw-point
  [x y noise-factor]
  (push-matrix)
  (translate x y)
  (rotate (* noise-factor (radians 360)))
  (stroke 0 150)
  (line 0 0 20 0)
  (pop-matrix))

(defn draw-all-points
  [x-start y-start step-size]
  (dorun
   (for [[x-idx x] (indexed-range-incl 0 (width) step-size)
         [y-idx y] (indexed-range-incl 0 (height) step-size)]
     (let [x-noise-shift (* x-idx 0.1)
           y-noise-shift (* y-idx 0.1)
           x-noise (+ x-start x-noise-shift)
           y-noise (+ y-start y-noise-shift)]
       (draw-point x y (noise x-noise y-noise))))))

(defn starts-seq []
  (let [noise-steps (steps (random 20) 0.01)
        noises      (map noise noise-steps)
        noises      (mul-add noises 0.5 -0.25)
        noise-tally (tally noises)]
    (map +
         (steps (random 10) 0.01)
         noise-tally)))

(defn setup []
  (smooth)
  (background 255)
  (frame-rate 24)

  (let [x-starts      (starts-seq)
        y-starts      (starts-seq)
        starts-str    (seq->stream (map list x-starts y-starts))]
    (set-state! :starts-str starts-str)))

(defn draw []
  (background 255)
  (let [[x-start y-start] ((state :starts-str))]
    (draw-all-points x-start y-start 5)))

(defsketch gen-art-25
  :title "Animated Rotated Lines"
  :setup setup
  :draw draw
  :size [300 300])

(defn -main [& args]
  (println "As mi argentunians goslemed thu mi migitigits, all go to poo."))

