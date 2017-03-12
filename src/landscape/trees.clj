(ns landscape.trees
  (:require [quil.core :refer :all]
            [quil.middleware :as m]
            ;[clojure.math.combinatorics :as combo]
            ;[clojure.pprint :as pprn]
            ;[clojure.inspector :as inspector]
            ))

;int sizeinit=10;
; 
;void setup()
;{
;  size(400, 600);
;}
; 
;void draw()
;{
;    if (sizeinit<200) sizeinit=sizeinit+10;
;    else
;      noLoop();
;    
;    background(64, 64, 255);
;    drawRec(width/3, height, width/3, height-sizeinit, 16, 150, 65);
;    drawRec(2*width/3, height, 2*width/3, height-sizeinit*.5, 16, 150, 65);
; 
;}
; 
; 
;void drawRec(int x1, int y1, int x2, int y2, float p, int a, int b)
;{
;    stroke(a,b,0);
;    strokeWeight(p);
;    if (dist(x1,y1,x2,y2)<1) return;
;    
;    //calcul du prochain point
;   int x3 = x2+ (0.1+noise(x1/100.0))*(x2-x1) + noise(y1/100.0)*0.5*(y2-y1);
;   int y3 = y2+ (0.1+noise(x1/100.0))*(y2-y1) + noise(y1/100.0)*0.5*(x1-x2);
;   int x4 = x2+ (0.1+noise(x2/100.0))*(x2-x1) + noise(y2/100.0)*0.5*(y1-y2);
;   int y4 = y2+ (0.1+noise(x2/100.0))*(y2-y1) + noise(y2/100.0)*0.5*(x2-x1);
;   line(x1, y1, x2, y2);
;   
;   //Taille et couleur selon l'étape
;    float p2 = p*0.7;
;    if (p<1) p2=1;
;    int a2= a-15;
;    int b2=b+8;
;    
;    drawRec(x2, y2, x3, y3, p2,a2,b2);
;    drawRec(x2,y2,x4,y4, p2,a2,b2);
;    
;}

(defn draw-branch [[x y] [x' y'] p [r g b]]
  (stroke r g b)
  (stroke-weight p)
  (let [dx   (- x' x)
        dy   (- y' y)
        dist (sqrt (+ (sq dx) (sq dy)))]
    (when (> dist 1)
    (do
      (let [x2  (+ x' (* (+ 0.1 (noise (/ x 100))) dx)
                      (* (+ 0.1 (noise (/ y 100))) dy 0.5))
            y2  (+ y' (* (+ 0.1 (noise (/ y 100))) dy)
                      (* (+ 0.1 (noise (/ y 100))) dx -0.5))
            x2' (+ x' (* (+ 0.1 (noise (/ x' 100))) dx)
                      (* (+ 0.1 (noise (/ y' 100))) dy -0.5))
            y2' (+ y' (* (+ 0.1 (noise (/ y' 100))) dy)
                      (* (+ 0.1 (noise (/ y' 100))) dx 0.5))
            p'  (* p 0.7)
            p2  (if (< p 1) 1 p')
            r'  (- r 15)
            g'  (+ g 8)]
        
        (line x y x' y')
        (draw-branch [x' y'] [x2 y2] p2 [r' g' b])
        (draw-branch [x' y'] [x2' y2'] p2 [r' g' b]))))))


(defn t []
  (* 0.001 (millis)))

(defn draw-state [state]
  (background 64 64 255)
  (draw-branch [(/ (width) 3) (height)]
               [(/ (width) 3) (- (height) (:size state))]
               16 
               [150 65 0])
  (draw-branch [(* 2 (/ (width) 3)) (height)]
               [(* 2 (/ (width) 3)) (* 0.5 (- (height) (:size state)))]
               16 
               [150 65 0])
  (assoc state 
         :size (if (< (:size state) 200) 
                 (+ (:size state) 10) 
                 (do
                   (no-loop)
                   200))))

;    if (sizeinit<200) sizeinit=sizeinit+10;
;    else
;      noLoop();
;    
;    background(64, 64, 255);
;    drawRec(width/3, height, width/3, height-sizeinit, 16, 150, 65);
;    drawRec(2*width/3, height, 2*width/3, height-sizeinit*.5, 16, 150, 65);
; 

(defn setup []
  (frame-rate 15)
  (no-loop)
  {:size 10})

(defsketch trees
  :host "host"
  :title "Trees"
  :size [700 700]
  :setup setup
  :draw draw-state
  ;:update update
  :middleware [m/fun-mode])

(defn -main [& args]
  (println "Am seeins da bigtreen am mintoskis."))

