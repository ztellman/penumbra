;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.loop
  (:use [penumbra.opengl])
  (:use [penumbra.app.core])
  (:require [penumbra.app.clock :as clock])
  (:import [org.lwjgl.opengl Display])
  (:import [java.util.concurrent CountDownLatch]))

;;;

(defstruct controller-struct
  :paused?
  :stopped?
  :invalidated?
  :latch)

(defn create-controller []
  (with-meta
    (struct-map controller-struct
      :paused? (atom false)
      :stopped? (atom true)
      :invalidated? (atom false)
      :latch (atom nil))
    {:type ::controller}))

(defn stopped?
  ([] (stopped? *controller*))
  ([controller] @(:stopped? controller)))

(defn update-stopped? []
  (or (not *update-stopped?*) @*update-stopped?*))

(defn stop
  ([] (stop *controller*))
  ([controller] (reset! (:stopped? controller) true)))

(defn stop-update []
  (when *update-stopped?*
    (reset! *update-stopped?* true)))

(defn paused?
  ([] (paused? *controller*))
  ([controller] @(:paused? controller)))

(defn pause
  ([]
     (pause *controller*))
  ([controller]
     (reset! (:paused? controller) true)
     (reset! (:latch controller) (CountDownLatch. 1))
     nil))

(defn resume
  ([]
     (resume *controller*))
  ([controller]
     (reset! (:paused? controller) false)
     (reset! (:stopped? controller) false)
     (when-let [latch @(:latch controller)]
       (.countDown #^CountDownLatch latch))))

(defn invalidated?
  ([] (invalidated? *controller*))
  ([controller] @(:invalidated? controller)))

(defn try-latch [latch]
  (when latch
    (.await #^CountDownLatch latch)))

(defn repaint
  ([]
     (repaint *controller*))
  ([controller]
     (reset! (:invalidated? controller) true)))

(defn repainted
  ([]
     (repaint *controller*))
  ([controller]
     (reset! (:invalidated? controller) false)))

(defmacro with-controller [controller & body]
  `(binding [*controller* ~controller]
     ~@body))

;;Loops

(defn- clock []
  (/ (System/nanoTime) 1e9))

(defn periodic-fn
  [hz]
  (let [frequency (atom hz)]
    (fn [f]
      (binding [*hz* frequency]
        (let [start (clock)]
          (f)
          (let [delta (- (clock) start)
                sleep (max 0 (- (/ 1 @*hz*) delta))]
            (when-not (zero? sleep)
              (Thread/sleep (long (* sleep 1e3)) (long (rem (* sleep 1e6) 1e6))))))))))

(defn timed-fn [f]
  (let [last-time (atom (clock/now *clock*))]
    (fn [& args]
      (let [current-time (clock/now *clock*)]
        (try
         (if f
           (apply f (list* [(- current-time @last-time) current-time] args))
           @(:state *app*))
         (finally
          (reset! last-time current-time)))))))

(defn update-loop
  [outer-fn inner-fn latch-fn complete-fn]
  (outer-fn
   (fn []
     (loop []
       (try-latch (latch-fn))
       (inner-fn)
       (when-not (complete-fn)
         (recur))))))

(defn secondary-loop [hz outer-fn inner-fn]
  (let [periodic (periodic-fn hz)]
    (->
     (Thread.
      (fn []
        (binding [*update-stopped?* (atom false)]
          (Thread/sleep (/ 1000 hz))
          (update-loop
           outer-fn
           #(periodic inner-fn)
           #(deref (:latch *controller*))
           #(or (update-stopped?) (stopped?))))))
     .start)))

(defn primary-loop [outer-fn inner-fn]
  (update-loop
   outer-fn
   inner-fn
   (constantly nil)
   #(or (paused?) (stopped?))))