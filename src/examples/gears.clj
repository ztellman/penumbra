;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns examples.gears
  (:use [penumbra opengl window]))

;;;;;;;;;;;;;;;;;;;;;;;;
;Gear building functions

(defmacro mirror
  "given [a b c], yields [b c a c b]"
  [divider & args]
  `(do
    ~@args
    ~divider
    ~@(reverse args)))

(defn draw-gear-face [num-teeth low mid high]
  (let [increment (/ 90. num-teeth)]
    (normal 0 0 1)
    (draw-quads
      (rotate increment 0 0 1)
      (dotimes [idx (inc num-teeth)]
        (mirror (rotate increment 0 0 1)
          (vertex 0 low 0)
          (vertex 0 high 0))

        (vertex 0 low 0)
        (vertex 0 high 0)
        (rotate increment 0 0 1)
        (vertex 0 mid 0)
        (vertex 0 low 0)

        (mirror (rotate increment 0 0 1)
          (vertex 0 low 0)
          (vertex 0 mid 0))

        (vertex 0 low 0)
        (vertex 0 mid 0)
        (rotate increment 0 0 1)
        (vertex 0 high 0)
        (vertex 0 low 0)))))

(defn draw-gear-teeth [num-teeth mid high]
  (let [increment (/ 90. num-teeth)
        tooth-slope (/ (- high mid) (/ (* Math/PI high) (/ 360 increment)))]
    (draw-quads
      (dotimes [idx num-teeth]
        (rotate increment 0 0 1)

        (normal 0 1 0)
        (mirror (rotate increment 0 0 1)
          (vertex 0 high 1)
          (vertex 0 high 0))

        (normal (- tooth-slope) 1 0)
        (push-matrix
          (mirror (do (rotate increment 0 0 1) (translate 0 (- mid high) 0))
            (vertex 0 high 1)
            (vertex 0 high 0)))
        (rotate increment 0 0 1)

        (normal 0 1 0)
        (mirror (rotate increment 0 0 1)
          (vertex 0 mid 1)
          (vertex 0 mid 0))

        (normal tooth-slope 1 0)
        (push-matrix
          (mirror (do (rotate increment 0 0 1) (translate 0 (- high mid) 0))
            (vertex 0 mid 1)
            (vertex 0 mid 0)))))))

(defn draw-gear-hole [num-teeth radius]
  (let [increment (/ 180. num-teeth)]
    (draw-quads
      (dotimes [idx (* 2 num-teeth)]
        (normal 0 -1 0)
        (mirror (rotate increment 0 0 1)
          (vertex 0 radius 0)
          (vertex 0 radius 1))))))

(defn draw-gear [num-teeth low mid high width]
  (material :front-and-back
    :ambient-and-diffuse [1 0.25 0.25 1])
  (push-matrix
    (scale 1 1 width)
    (translate 0 0 -0.5)
    (push-matrix
      (rotate 180 0 1 0)
      (rotate (/ 90. num-teeth) 0 0 1)
      (draw-gear-face num-teeth low mid high))
    (push-matrix
      (draw-gear-teeth num-teeth mid high))
    (push-matrix
      (translate 0 0 1)
      (draw-gear-face num-teeth low mid high))
    (push-matrix
      (draw-gear-hole num-teeth low))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init [state]
  (enable :cull-face)
  (enable :auto-normal)
  (enable :normalize)
  (enable :depth-test)
  (enable :lighting)
  (enable :light0)
  (shade-model :flat)
  (assoc state
    :gear (get-display-list (draw-gear 30 0.5 3 4 2))))

(defn reshape [[x y width height] state]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  (translate 0 0 -10)
  (light 0 :position [1 1 1 0])
  state)

(defn mouse-drag [[[dx dy] _] state]
  (assoc state
    :rot-x (- (:rot-x state) dy)
    :rot-y (- (:rot-y state) dx)))

(defn display [[delta time] state]
  (write-to-screen (format "%d fps" (int (/ 1 delta))) 0 1)
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (rotate (* 20. (rem time 360)) 0 0 1)
  (call-display-list (:gear state))
  (repaint))

(start
  {:reshape reshape, :display display, :init init, :mouse-drag mouse-drag}
  {:rot-x 0, :rot-y 0, :gear nil})
