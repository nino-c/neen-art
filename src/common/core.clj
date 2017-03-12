(ns common.core
  (:require [quil.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;
; quil

(defn t []
  (/ (millis) 1000.0))

(defn center-origin [] 
  (translate (/ (width) 2) (/ (height) 2)))


;;;;;;;;;;;;;;;;;;;;;;;
; general


(defn maplist [func coll]
  (map (fn [row] (map func row)) coll))

(defn member?
  [item series]
  (if (some #(= item %) series) 
    true 
    false))

(defn append
  [lst item]
  (reverse (conj (reverse lst) item)))

