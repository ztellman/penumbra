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

;;NOT COMPLETE
;;;

(defn sphere-vertices [lod]
  (let [rotate-x (rotation-matrix (/ 180 lod) 1 0 0)
        arc (take
             (inc lod)
             (iterate (partial apply-matrix rotate-x) [0 1 0 0]))
        rotate-y (rotation-matrix (/ 180 lod) 0 1 0)]
    (take
     (inc (* 2 lod))
     (iterate
      (partial map (partial apply-matrix rotate-y))
      arc))))

(defn dot [u v]
  (apply + (map * u v)))

(defn normalize [v]
  (let [len (Math/sqrt (dot v v))]
    (map #(/ % len) v)))

(defn rand-vector []
  (normalize (map #(- % 1) (take 3 (repeatedly #(rand 2))))))

(defn offset-vertex [v vertex]
  (if (neg? (dot v (normalize vertex)))
    (map + vertex (repeat 0.05))
    (map - vertex (repeat 0.05))))

(defn offset-sphere [v vertices]
  (map
   (fn [arc] (map #(offset-vertex v %) arc))
   vertices))

(defn gen-asteroid [lod iterations]
  (nth
   (iterate
    #(offset-sphere (rand-vector) %)
    (sphere-vertices lod))
   iterations))

;;;

(defn init [state]
  (draw-wireframe)
  state)

(defn reshape [[x y w h] state]
  (frustum-view 50 (/ w h) 0.1 100)
  (load-identity)
  (translate 0 0 -3)
  state)

(defn key-press [key state]
  (if (= " " key)
    (assoc state
      :asteroid (offset-sphere (rand-vector) (:asteroid state)))
    state))

(defn mouse-drag [[[dx dy] _] button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn display [_ state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (doseq [arcs (partition 2 1 (:asteroid state))]
    (draw-quad-strip
     (doseq [[a b] (map list (first arcs) (second arcs))]
        (apply vertex a) (apply vertex b)))))
   
(start
 {:reshape reshape, :display display, :mouse-drag mouse-drag, :init init, :key-press key-press}
 {:rot-x 0, :rot-y 0, :asteroid (gen-asteroid 6 15)})