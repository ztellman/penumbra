;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;NOT COMPLETE

(ns examples.pong
  (:use [penumbra window opengl]))

(def p-h 0.3)  ;paddle height
(def p-w 0.05) ;paddle width
(def b-d 0.05) ;ball width/height

(defn rectangle [x y w h]
  (vertex x y 0)
  (vertex (+ x w) y 0)
  (vertex (+ x w) (+ y h) 0)
  (vertex x (+ y h) 0))

(defn clamp [x offset]
  (let [offset (- 1 (/ offset 2))]
    (max (* -1 offset) (min (* 1 offset) x))))

(defn reshape [[x y w h] s]
  (let [aspect (/ (float w) h)
        height (if (> 1 aspect) (/ 1.0 aspect) 1)
        aspect (max 1 aspect)]
    (ortho-view (- aspect) (- height) aspect height -1 1)
    s))

(defn key-press [key s]
  (assoc s
    :v-left
    (cond
      (= key "UP") -1
      (= key "DOWN") 1
      :else 0)))

(defn key-release [[key event] state]
  (assoc state
    :v-left 0))

(defn update [[dt t] s]
  (let [l (:left s), v-l (:v-left s)
        r (:right s), v-r (:v-right s)]
    (assoc s
    :left (clamp (+ l (* dt v-l)) p-h)
    :right (clamp (+ r (* dt v-r)) p-h))))

(defn display [_ s]
  (scale 0.9 0.9 1)
  (draw-line-loop (rectangle -1 -1 2 2)) ;perimeter
  (scale 0.99 0.99 1)
  (push-matrix
    (translate 0 (/ p-h -2) 0)
    (draw-quads (rectangle -1 (:left s) p-w p-h)) ;left paddle
    (draw-quads (rectangle (- 1 p-w) (:right s) p-w p-h))) ;right paddle
  (push-matrix
    (translate (/ b-d -2) (/ b-d -2) 0)
    (draw-quads (apply rectangle (concat (:ball s) [b-d b-d])))) ;ball
  (repaint))

(start
  {:reshape reshape, :display display, :key-press key-press, :key-release key-release, :update update}
  {:left 0, :right 0, :ball [0 0], :v-left 0, :v-right 0, :v-ball [0 0]})