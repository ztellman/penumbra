;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.time
  (:use [penumbra.geometry :only [lerp]]))

(defn wall-time []
  (/ (System/nanoTime) 1e9))

(defn animation
  ([start finish duration]
     (animation start finish duration identity))
  ([start finish duration modifier]
     (let [t0 (wall-time)]
       (fn []
         (let [elapsed (modifier (- (wall-time) t0))]
           (if (> elapsed duration)
             finish
             (lerp start finish (/ elapsed duration))))))))

(defn clock
  ([]
     (clock identity))
  ([modifier]
     (let [t0 (wall-time)]
       (atom #(modifier (- (wall-time) t0))))))

(defn update-clock [c mod]
  (swap!
   c
   (fn [c]
     (let [t0 (c)
           c @(clock mod)]
       (fn [] (+ t0 (c))))))
  c)

(defn now [c]
  (@c))


