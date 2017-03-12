(ns neenart.poop
  (:require [quil.core :refer :all]))

(defn t [] (/ (millis) 1000))

(defn draw []
  (let [A 0
        B (+ 0.5 
             (* 0.4 (sin (/ (t) 5))))
        C 1]
    (push-matrix)
    (translate (/ (width) 2) (/ (height) 2))
    (scale (* (width) 0.9))
    (translate -0.5 0)
    (background 255)
    (stroke 200)
    (stroke-weight 0.02)
    (line A 0 C 0)
    
    (stroke 60)
    (stroke-weight 0.03)
    (ellipse A 0 0.05 0.05)
    (ellipse B 0 0.05 0.05)
    (ellipse C 0 0.05 0.05)
    
    (pop-matrix)
    (text (str "phi=" (/ 1 B)) 15 (- (height) 25))
    ))

(defn setup []
  (frame-rate 30)
  (fill 50)
  (text-font (create-font "DejaVu Sans" 18 true))
  )

(defsketch phi
  :size [500 500]
  :setup setup
  :draw draw)