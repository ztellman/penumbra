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
  (:require [penumbra.opengl.texture :as texture])
  (:require [penumbra.slate :as slate])
  (:import [org.lwjgl.opengl Display])
  (:import [java.util.concurrent CountDownLatch])
  (:require [penumbra.app.window :as window])
  (:require [penumbra.app.input :as input]))

;;;

(defn- repaint []
  (reset! (:invalidated? *app*) true))

(defn try-callback [callback & args]
  (when-let [f (callback (:callbacks *app*))]
    (let [state @(:state *app*)
          new-state (swap!
                     (:state *app*)
                     (fn [s]
                       (or (if (empty? args)
                             (f s)
                             (apply f (concat args [s])))
                        state)))]
      (when-not (identical? state new-state)
        (repaint)))))

(defmacro with-app [app & body]
  `(binding [*app* ~app
             *callback-handler* try-callback]
     (input/with-input (:input ~app)
       (window/with-window (:window ~app)
         ~@body))))

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
  (let [last-time (atom (clock))]
    (fn [& args]
      (let [current-time (clock)]
        (try
         (if f
           (apply f (list* [(- current-time @last-time) current-time] args))
           @(:state *app*))
         (finally
          (reset! last-time current-time)))))))

(defn update-loop
  [latch-fn complete-fn inner-loop]
  (loop []
    (when-let [latch (latch-fn)]
      (.await #^CountDownLatch latch))
    (inner-loop)
    (when-not (complete-fn)
      (recur))))

(defn secondary-loop [app hz f]
  (let [wrapper (periodic-fn hz)]
    (->
     (Thread.
      (fn []
        (with-app app
          (binding [*async-running?* (atom true)]
            (let [s (slate/create (:window app))]
              (try
               (Thread/sleep (/ 1000 hz))
               (update-loop
                #(deref (:latch app))
                #(or (not @*async-running?*) @(:stopped? app))
                #(wrapper f))
               (finally
                (slate/destroy s))))))))
     .start)))

(defn primary-loop [app f]
  (let [wrapper (periodic-fn 200)]
    (with-app app
      (update-loop
       (constantly nil)
       #(or @(:paused? app) @(:stopped? app))
       #(wrapper f)))))