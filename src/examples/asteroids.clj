;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns examples.asteroids
  (:use [penumbra opengl window])
  (:use [penumbra.opengl geometry]))

(def detail 20)

(defn sphere-vertices []
  (let [rotate-x (rotation-matrix (/ 180 detail) 1 0 0)
        arc (take
             (inc detail)
             (iterate (partial apply-matrix rotate-x) [0 1 0 0]))
        rotate-y (rotation-matrix (/ 360 detail) 0 1 0)]
    (take
     (* 2 detail)
     (iterate
      (partial map (partial apply-matrix rotate-y))
      arc))))

(def points (sphere-vertices))

(defn reshape [[x y w h] state]
  (frustum-view 50 (/ w h) 0.1 100)
  (load-identity)
  (translate 0 0 -3)
  state)

(defn mouse-drag [[[dx dy] _] button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn display [_ state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-points
   (doseq [arc points]
     (doseq [p arc]
       (apply vertex p)))))
   
(start
 {:reshape reshape, :display display, :mouse-drag mouse-drag}
 {:rot-x 0, :rot-y 0})