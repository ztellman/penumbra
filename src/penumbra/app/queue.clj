;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.queue
  (:use [penumbra.app.core])
  (:require [penumbra.slate :as slate])
  (:import [java.util Timer TimerTask]))

;;;

(defstruct queue-struct
  :actions
  :promises)

(defn create-queue []
  (let [q (repeatedly promise)]
    (with-meta
      (struct-map queue-struct
        :promises q
        :empty-promises q)
      {:type ::queue})))

(defn enqueue [q-ref f]
  (let [prm (dosync
             (let [head (first (:empty-promises @q))]
               (ref-set q (rest @q))
               head))]
    (deliver prm f)))

(defn dequeue [q-ref f]
  )

;;;

(defn create-thread [outer-fn inner-fn]
  (.start
   (Thread.
    #(outer-fn
      (fn []
        (let [s (slate/create)]
          (try (inner-fn)
               (finally (slate/destroy s)))))))))

(defn create-queue-thread []
  )

