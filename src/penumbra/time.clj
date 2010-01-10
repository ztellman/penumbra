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

(defprotocol Composition
  (inner [this])
  (outer [this])
  (outer! [this o]))

(defn composition
  [inner outer]
  (let [outer (atom outer)]
    (reify
     clojure.lang.IDeref
     (deref [] (@outer (inner)))
     Composition
     (inner [] inner)
     (outer [] @outer)
     (outer! [o] (reset! outer o) nil))))

(defn clock
  ([]
     (let [start (wall-time)]
       (clock #(- (wall-time) start) identity)))
  ([inner-clock]
     (clock inner-clock identity))
  ([inner-clock modifier]
     (composition inner-clock modifier)))

(defn update-clock! [c outer-fn]
  (let [i-start ((inner c))
        o-start ((outer c) i-start)]
    (outer! c #(+ o-start (outer-fn (- % i-start))))))

(defn clock-speed!
  "Sets the speed of the clock, where 1 is real-time."
  [c speed]
  (update-clock! c #(* % speed)))

(defprotocol Animation
  (to! [this target time])
  (dest [this]))

(defn animation
  ([value clock]
     (animation value clock identity))
  ([value clock curve]
     (let [this {:origin (ref value)
                 :target (ref value)
                 :dest (ref (constantly 1))}
           sample #(let [t (@(:interpolator this))]
                     (if (= 1 t)
                       @(:dest this)
                       (lerp @(:origin this) @(:dest this) t)))]
       (reify
        clojure.lang.IDeref
        (deref [] (sample))
        Animation
        (dest [] @(:dest this))
        (to!
         [target time]
         (dosync
          (ref-set (:origin this) (sample))
          (ref-set (:target this) target)
          (let [start @clock]
            (ref-set (:interpolator this) #(max 0 (min 1 (/ (- @clock start) time)))))))))))