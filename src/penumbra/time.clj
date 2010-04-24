;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.time
  (:use [clojure.contrib.def :only [defn-memo]]))

(defn wall-time []
  (/ (double (System/nanoTime)) (double 1e9)))

(defprotocol Clock
  (speed! [c speed]))

(defn clock
  ([]
     (clock 0 1))
  ([offset speed]
     (let [t0 (wall-time)
           lookup (atom #(+ offset (* speed (- % t0))))]
       (reify
        clojure.lang.IDeref
        (deref
         [_]
         (@lookup (wall-time)))
        Clock
        (speed!
         [_ speed]
         (let [t0 (wall-time)
               offset (@lookup t0)]
           (reset! lookup #(+ offset (* speed (- % t0)))))
         nil)))))






