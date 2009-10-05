;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns examples.tetris
  (:use [penumbra window opengl]))

(def shapes
  ["XXXX"
   "X..
    XXX"
   "..X
    XXX"
   "XX
    XX"
   ".XX
    XX."
   "XXX
    .X."
   "XX.
    .XX"])

(defn parse-shape [shape]
  (let [lines     (map vector (map #(.trim %) (.split shape "\n")) (iterate inc 0))
        width     (count (ffirst lines))
        height    (count lines)
        offset    [(/ width 2) (/ height 2)]
        positions (apply
                    concat
                    (map
                      (fn [[x y]] (partition 3 (interleave x (iterate inc 0) (repeat y))))
                      lines))
        filtered  (filter #(-> % first (= \X)) positions)
        relative  (map #(map - (rest %) offset) filtered)]
    relative))



