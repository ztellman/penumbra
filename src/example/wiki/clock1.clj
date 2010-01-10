(ns example.wiki.clock1
  (:use [penumbra.opengl])
  (:require [penumbra.app :as app]))

(defn draw-clock [hour minute second]
  (push-matrix
   (scale 0.5 0.5 1)
   (rotate 180 0 0 1)
   (push-matrix
    (rotate (* -30 (rem hour 24)) 0 0 1)
    (color 1 1 1)
    (line-width 5)
    (draw-lines (vertex 0 0) (vertex 0 -0.5)))
   (push-matrix
    (rotate (* -6 (rem minute 60)) 0 0 1)
    (color 1 1 1)
    (line-width 2)
    (draw-lines (vertex 0 0) (vertex 0 -1)))
   (push-matrix
    (rotate (* -6 (rem second 60)) 0 0 1)
    (color 1 0 0)
    (line-width 1)
    (draw-lines (vertex 0 0) (vertex 0 -1)))))

(defn init [state]
  (app/vsync! true)
  state)

(defn reshape [[x y w h] state]
  (let [aspect (/ (float w) h)
        height (if (> 1 aspect) (/ 1.0 aspect) 1)
        aspect (max 1 aspect)]
    (ortho-view (- aspect) aspect (- height) height -1 1)
    state))

(defn display [[delta time] state]
  (draw-clock (/ time 3600) (/ time 60) time)
  (app/repaint!))

(app/start 
  {:display display, :reshape reshape, :init init} 
  {})