(ns landscape.tree-forest
  (:require [quil.core :refer :all]
            [quil.middleware :as m]
            ;[clojure.math.combinatorics :as combo]
            ;[clojure.pprint :as pprn]
            ;[clojure.inspector :as inspector]
            ))

;// This sketch builds on a prior work, "tree", created by frederic.vernier
;// http://studio.sketchpad.cc/sp/pad/view/ro.9KVkkjyrgcqo5/rev.907
; 
;int L = 0;
; 
;void setup(){
;  size(480, 600);
;}
; 
; 
;int drawRec(int x1, int y1, int x2, int y2, int n, int acc) {
;  float dd= dist(x1,y1,x2, y2);
; 
;  // Let's stop recursivity and draw some fruits
;  if (dd<1.4) {
;    randomSeed(acc*n);
;    if (random(0, 100)<=1 && L>80) {
;      int dev = random(96);
;      fill (255-dev/2, 64+dev, dev/2);
;      noStroke();
;      ellipse(x1-(L-90)/4, y1+(L-90)/2, (L-90)/2, (L-90));
;    }
;    return 1;
;  }
;    
;  randomSeed(n);
;  float r0 = random(0.5, 0.75);
;  float r1 = random(0.5, 0.75);
;  float r2 = 0.17+abs(noise(x2/100, y2/100)-0.5)/1;
;  float r3 = 0.17+abs(noise(x2/100, (y2-5)/100)-0.5)/1;
;  float p  = min(1, (x2-x1)/(y2-y1));
;  if (p==0)
;    if(n%2==0){r3=0;} // alternate side of the trunk
;    else {r2=0;}
;  
;  // left
;  int x3 = x2+ (x2-x1)*r0 + (y2-y1)*r2;
;  int y3 = y2+ (y2-y1)*r0 + (x1-x2)*r2;
; 
;  // right
;  int x4 = x2+ (x2-x1)*r1 - (y2-y1)*r3;
;  int y4 = y2+ (y2-y1)*r1 - (x1-x2)*r3;
;  
;  // on apelle  l'etape suivante (on apelle  l'etape suivante)
;  int res = drawRec(x2, y2, x3, y3, n+7, acc*2+0);
;  res +=    drawRec(x2, y2, x4, y4, n+5, acc*2+1);
;  
;  // on dessine l'etape courante (current step)
;  int ll = sqrt(res);//log(dd*1.9)*2.5;
;  if (p==0) ll+=max(0, 8-n/16);
;  float lum = (L-50)/50;
;  if (L<50) lum=0;
;  if (ll>5) {
;    strokeWeight(sqrt(ll));
;    lum = lum*(1-min(1, ll/25));
;    stroke (25+50*(1-lum), 30+60*(1-lum)+max(0, 50-L), 5+10*(1-lum));
;    line(x1, y1, x2, y2);
;  }
;  else if (L>50){
;    strokeWeight(sqrt(ll)+lum*3);
;    float l=random(30, 100)/100;
;    stroke (l*(105-min(50,n*2)), l*(105+min(100,n*3)), l*(100-min(50,n*2)));
;    line(x1, y1, x1+lum*(x2-x1), y1+lum*(y2-y1));
;  }
;  return res;
;}
; 
; 
;void draw()
;{
;  background(128, 128, 255);
;  stroke (0, 192, 64);
;  
;  drawRec(1*width/2, height, 1*width/2, height-1.5*L, 5, 0);
;  if (L>10) drawRec(2*width/7, height, 2*width/7, height-0.75*(L-10), 6, 0);
;  if (L>20) drawRec(5*width/7, height, 5*width/7, height-0.95*(L-20), 3, 0);
;  if(L<100)
;    L++; 
;  else noLoop();
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

