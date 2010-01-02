;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.clock)

(defstruct clock-struct
  :offset
  :start
  :speed)

(defn real-time []
  (/ (System/nanoTime) 1e9))

(defn create []
  (struct-map clock-struct
    :offset 0
    :start (real-time)
    :speed 1))

(defn now [clock-atom]
  (let [clock @clock-atom]
    (+ (:offset clock)
       (if-let [start (:start clock)]
         (* (:speed clock) (- (real-time) start))
         0))))

(defn speed [clock-atom clock-speed]
  (swap!
   clock-atom
   #(assoc %
      :start (real-time)
      :offset (now clock-atom)
      :speed clock-speed))
  nil)

(defn stop [clock-atom]
  (swap!
   clock-atom
   #(assoc %
      :offset (now clock-atom)
      :start nil))
  nil)

(defn start [clock-atom]
  (swap!
   clock-atom
   #(assoc %
      :start (real-time)))
  nil)