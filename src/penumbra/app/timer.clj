;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.timer)

(defstruct timer-struct
  :offset
  :start)

(defn real-time []
  (/ (System/nanoTime) 1e9))

(defn create []
  (struct-map timer-struct
    :offset (ref 0)
    :start (ref (real-time))))

(defn now [timer]
  (+ @(:offset timer)
     (if-let [start @(:start timer)]
       (- (real-time) start)
       0)))

(defn stop [timer]
  (dosync
   (ref-set (:offset timer) (now timer))
   (ref-set (:start timer) nil))
  nil)

(defn start [timer]
  (dosync
   (ref-set (:start timer) (real-time)))
  nil)