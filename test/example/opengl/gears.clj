;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.gears
  (:use [penumbra opengl]
	    [cantor])
  (:require [penumbra.app :as app]
            [penumbra.text :as text]))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;Gear building functions

(defn circle
  [steps]
  (let [increment (/ 360 steps)]
    (map #(cartesian (polar2 %)) (map (partial * increment) (cycle (range steps))))))

(defn uneven-circle [steps low high]
  (map #(mul %1 %2) (circle steps) (cycle [high low low high])))

(defn gear-face [num-teeth inner low high]
  (let [steps (* 4 num-teeth)
        inner-circle (uneven-circle steps inner inner)
        outer-circle (uneven-circle steps low high)]
    (apply concat (interleave (map reverse (partition 2 1 inner-circle)) (partition 2 1 outer-circle)))))

(defn gear-teeth [num-teeth low high]
  (let [teeth (uneven-circle (* 4 num-teeth) low high)
        a (map #(vec3 % 0) teeth)
        b (map #(vec3 % 1) teeth)]
    (apply concat (interleave (map reverse (partition 2 1 a)) (partition 2 1 b)))))

(defn draw-gear-face [num-teeth inner low high]
  (let [vertices (take (inc (* 16 num-teeth)) (gear-face num-teeth inner low high))]
    (draw-quads
     (doseq [v vertices]
       (vertex v)))))

(defn draw-strip [vertices]
  (draw-quads
   (doseq [face (partition 4 vertices)]
     (let [face (vec face)
           u (sub (face 2) (face 0))
           v (sub (face 1) (face 0))]
       (normal (normalize (cross u v)))
       (doseq [v face]
         (vertex v))))))

(defn draw-gear-teeth [num-teeth low high]
  (draw-strip (take
               (inc (* 16 num-teeth))
               (gear-teeth num-teeth low high))))

(defn draw-gear-hole [num-teeth radius]
  (draw-strip (reverse
               (take (inc (* 16 num-teeth))
                     (gear-teeth num-teeth radius radius)))))

(defn draw-gear [num-teeth inner low high width]
  (material :front-and-back
            :ambient-and-diffuse [1 0.25 0.25 1])
  (push-matrix
    (scale 1 1 width)
    (translate 0 0 -0.5)
    ;;first gear face
    (normal 0 0 -1)
    (draw-gear-face num-teeth inner low high)
    ;;second gear face
    (push-matrix
      (normal 0 0 1)
      (translate 0 0 1)
      (draw-gear-face num-teeth inner low high))
    ;;gear teeth
    (push-matrix
      (draw-gear-teeth num-teeth low high))
    ;;inner hole
    (push-matrix
      (draw-gear-hole num-teeth inner))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init [state]
  (app/title! "Gears")
  (app/vsync! false)
  (enable :depth-test)
  (enable :lighting)
  (enable :light0)
  (shade-model :flat)
  (assoc state
    :gear (create-display-list (draw-gear 30 0.5 3 4 2))))

(defn reshape [[x y width height] state]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  (translate 0 0 -10)
  (light 0 :position [1 1 1 0])
  state)

(defn mouse-drag [[dx dy] _ button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn key-press [key state]
  (cond
   (= :escape key) (app/pause!)))

(defn display [[delta time] state]
  (text/write-to-screen (format "%d fps" (int (/ 1 delta))) 0 0)  
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (rotate (* 20. (rem time 360)) 0 0 1)
  ((:gear state))
  (app/repaint!))

;;By using this instead of display, we can recompile display and see
;;our changes immediately.  Try reversing the sign on rotate while the
;;app is running and see for yourself.
(defn display-proxy [& args]
  (apply display args))

(defn start []
  (app/start
   {:reshape reshape, :display display-proxy, :init init, :mouse-drag mouse-drag, :key-press key-press}
   {:rot-x 0, :rot-y 0, :gear nil}))

