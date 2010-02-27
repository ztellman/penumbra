;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.controller
  (:use [penumbra.opengl]
        [penumbra.app.core])
  (:require [penumbra.time :as time])
  (:import [java.util.concurrent CountDownLatch]))

;;;

(defstruct controller-struct
  :paused?
  :stopped?
  :invalidated?
  :latch)

(defn create []
  (with-meta
    (struct-map controller-struct
      :paused? (atom false)
      :stopped? (atom true)
      :invalidated? (atom true)
      :latch (atom (CountDownLatch. 1)))
    {:type ::controller}))

(defn stopped?
  ([] (stopped? *controller*))
  ([controller] @(:stopped? controller)))

(defn stop!
  ([]
     (stop! *controller*))
  ([controller]
     (reset! (:stopped? controller) true)
     nil))

(defn paused?
  ([] (paused? *controller*))
  ([controller] @(:paused? controller)))

(defn pause!
  ([]
     (pause! *controller*))
  ([controller]
     (reset! (:paused? controller) true)
     (reset! (:latch controller) (CountDownLatch. 1))
     nil))

(defn resume!
  ([]
     (resume! *controller*))
  ([controller]
     (reset! (:paused? controller) false)
     (reset! (:stopped? controller) false)
     (when-let [latch @(:latch controller)]
       (.countDown #^CountDownLatch latch))
     nil))

(defn invalidated?
  ([] (invalidated? *controller*))
  ([controller] @(:invalidated? controller)))

(defn try-latch!
  ([] (try-latch! @(:latch *controller*)))
  ([latch]
     (when latch
       (.await #^CountDownLatch latch))
     nil))

(defn repaint!
  ([]
     (repaint! *controller*))
  ([controller]
     (reset! (:invalidated? controller) true)
     nil))

(defn repainted!
  ([]
     (repainted! *controller*))
  ([controller]
     (reset! (:invalidated? controller) false)
     nil))

