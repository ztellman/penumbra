;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns examples.tetris
  (:use [penumbra window opengl]))

(defn rotate* [angle [x y]]
  (let [angle (* angle (/ Math/PI 180.0))
        sin (Math/sin angle)
        cos (Math/cos angle)]
    [(- (* x cos) (* y sin)) (+ (* y cos) (* x sin))]))

(def two-way (cycle [#(rotate* 90 %) #(rotate* -90 %)]))
(def four-way  (repeat #(rotate* 90 %)))
(def one-way (repeat identity))

(defn parse-shape [shape]
  (let [even       #(if (even? %) % (dec %))
        lines     (map vector (map #(.trim %) (.split shape "\n")) (iterate inc 0))
        width     (even (count (ffirst lines)))
        height    (even (count lines))
        offset    [(/ width 2) (/ height 2)]
        positions (apply
                    concat
                    (map
                      (fn [[x y]] (partition 3 (interleave x (iterate inc 0) (repeat y))))
                      lines))
        filtered  (filter #(-> % first (= \X)) positions)
        relative  (map #(map - (rest %) offset) filtered)]
    relative))

(def shapes
     (map
      (fn [[a b]] [(parse-shape a) b])
      [["XXXX"
        two-way]
       ["X..
         XXX"
        four-way]
       ["XXX
         ..X"
        four-way]
       ["XX
         XX"
        one-way]
       [".XX
         XX."
        two-way]
       [".X.
         XXX"
        four-way]
       ["XX.
         .XX"
        four-way]]))

(defn init [state]
  (assoc state
    :shapes shapes))

(defn reshape [[x y w h] state]
  (let [aspect (/ (float w) h)
        height (if (> 1 aspect) (/ 1.0 aspect) 1)
        aspect (max 1 aspect)]
    (ortho-view (- aspect) (- height) aspect height -1 1)
    state))

(defn key-press [key state]
  (cond
    (= key :up)
    (let [[blocks fns] (first (:shapes state))]
      (assoc state
        :shapes (cons [(map #((first fns) %) blocks) (rest fns)] (rest (:shapes state)))))
    (= key :space)
    (assoc state
      :shapes (rest (:shapes state)))
    :else
    state))

(defn draw-quad [x y]
  (push-matrix
    (translate x y 0)
    (scale 0.975 0.975 1)
    (dotimes [_ 4]
      (rotate 90 0 0 1)
      (vertex 0.5 0.5 0))))

(defn display [_ state]
  (scale 0.25 0.25 1)
  (draw-quads
   (doseq [block (ffirst (:shapes state))]
     (apply draw-quad block))))

(start {:init init, :display display, :reshape reshape, :key-press key-press} {})


