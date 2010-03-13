;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.time
  (:use [penumbra.geometry :only [lerp]])
  (:use [clojure.contrib.def :only [defn-memo]]))

(defn wall-time []
  (/ (System/nanoTime) 1e9))

(defprotocol composition
  (inner [this])
  (outer [this])
  (update! [this inner outer]))

(defn clock
  ([]
     (let [t0 (wall-time)]
       (clock #(- (wall-time) t0) identity)))
  ([inner outer]
     (let [o (ref outer)
           i (ref inner)]
       (reify
        composition
        (inner [] @i)
        (outer [] @o)
        (update! [inner outer]
                (let [i0 (inner)
                      o0 (outer i0)]
                  (dosync
                   (ref-set i #(- (inner) i0))
                   (ref-set o #(+ (outer %) o0))
                   nil)))
        clojure.lang.IDeref
        (deref [] (@o (@i)))))))

(defn-memo wall-clock []
  (clock wall-time identity))

(defn clock-speed! [clock speed]
  (update! clock (inner clock) #(* % speed)))

(defn now [c]
  @c)


