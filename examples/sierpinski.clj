;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.examples.sierpinski)

(use 'penumbra.opengl.core 'penumbra.opengl.geometry 'penumbra.opengl.view 'penumbra.interface.window)

(defn draw-pyramid []
  (material 1 0.25 0.25 1)
  (draw-triangle-fan
    (vertex 0 1 0)
    (dotimes [_ 5]
      (rotate 90 0 1 0)
      (normal 1 0.5 1)
      (vertex 0.5 0 0.5)))
  (draw-quads
    (normal 0 -1 0)
    (dotimes [_ 4]
      (rotate -90 0 1 0)
      (vertex 0.5 0 0.5))))

(defn subdivide [display-list]
  (push-matrix
    (scale 0.5 0.5 0.5)
    (push-matrix
      (translate 0 1 0)
      (call-display-list display-list))
    (dotimes [_ 4]
      (rotate 90 0 1 0)
      (push-matrix
        (translate 0.5 0 0.5)
        (call-display-list display-list)))))

(defn sierpinski []
  (iterate
    #(get-display-list (subdivide %))
    (get-display-list (draw-pyramid))))

;;;;;;;;;;;;;;;;;
(def rot-x (ref 9))
(def rot-y (ref 202))
(def pyramid (atom nil))

(defn init []
  (enable :normalize)
  (enable :depth-test)
  (enable :multisample)
  (enable :cull-face)
  (shade-model :flat)
  (reset! pyramid (nth (sierpinski) 6)))

(defn reshape [x y width height]
  (frustum-view 50 (/ (double width) height) 0.1 100)
  (load-identity)
  (translate 0 -0.35 -1.75)
  (set-light-position 0 [1 1 1 0])
  (setup-fog :exp 0.75 0 10 [0 0 0 0]))

(defn mouse-drag [[dx dy] _]
  (dosync
    (ref-set rot-x (- @rot-x dy))
    (ref-set rot-y (- @rot-y dx))))

(defn display [delta time]
  (write (format "%d fps" (int (/ 1 delta))) 0 1)
  (rotate @rot-x 1 0 0)
  (rotate @rot-y 0 1 0)
  (call-display-list @pyramid))

(start {:display display, :mouse-drag mouse-drag, :reshape reshape, :init init})



