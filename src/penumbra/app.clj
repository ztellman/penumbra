;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this
;;   software.

(ns penumbra.app
  (:use [clojure.contrib.def :only [defmacro- defvar-]])
  (:use [penumbra.opengl])
  (:use [penumbra.opengl.core :only [*texture-pool*]])
  (:require [penumbra.slate :as slate])
  (:require [penumbra.window :as window])
  (:require [penumbra.input :as input])
  (:import [org.lwjgl.opengl Display])
  (:import [java.util.concurrent CountDownLatch]))

(defstruct app-struct
  :window
  :input
  :callbacks
  :state
  :stopped?
  :paused?
  :latch
  :invalidated?)

(defvar- *app* nil
  "Current application.")

(defn- create [callbacks state]
  (struct-map app-struct
    :window (window/create)
    :input (input/create)
    :callbacks callbacks
    :state (atom state)
    :stopped? (atom true)
    :paused? (atom false)
    :latch (atom nil)
    :started false
    :invalidated? (atom true)))

(defn repaint
  "Forces a new frame to be redrawn"
  []
  (when *app*
    (reset! (:invalidated? *app*) true)))

(defn set-title [title]
  (Display/setTitle title))

(defn- try-callback [callback & args]
  (when-let [f (callback (:callbacks *app*))]
    (when (not (identical?
                @(:state *app*)
                (swap!
                 (:state *app*)
                 (if (empty? args)
                   f
                   (apply partial (list* f args))))))
      (repaint))))

(defmacro- with-app [app & body]
  `(binding [*app* ~app
             input/*callback-handler* try-callback
             window/*callback-handler* try-callback]
     (input/with-input (:input ~app)
       (window/with-window (:window ~app)
         ~@body))))

;;Loops

(defn clock [] (/ (System/nanoTime) 1e9))

(defvar- *hz* nil
  "Refresh rate of periodic function")

(defn frequency!
  "Update frequency of update-loop.  Can only be called from inside update-loop."
  [hz]
  (reset! *hz* hz))

(defn- periodic-fn
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

(defn- timed-fn [f]
  (let [last-time (atom (clock))]
    (fn [& args]
      (let [current-time (clock)]
        (try
         (if f
           (apply f (list* [(- current-time @last-time) current-time] args))
           @(:state *app*))
         (finally
          (reset! last-time current-time)))))))

(defn- update-loop
  [latch-fn complete-fn inner-loop]
  (loop []
    (when-let [latch (latch-fn)]
      (.await #^CountDownLatch latch))
    (inner-loop)
    (when-not (complete-fn)
      (recur))))

(defn- secondary-loop [app hz f]
  (let [wrapper (periodic-fn hz)]
    (->
     (Thread.
      (fn []
        (with-app app
          (let [s (slate/create (:window app))]
            (try
             (Thread/sleep (/ 1000 hz))
             (update-loop
              #(deref (:latch app))
              #(deref (:stopped? app))
              #(wrapper f))
             (finally
              (slate/destroy s)))))))
     .start)))

(defn- primary-loop [app f]
  (let [wrapper (periodic-fn 200)]
    (with-app app
      (update-loop
       (constantly nil)
       #(or @(:paused? app) @(:stopped? app))
       #(wrapper f)))))

;;;

(defn stop
  "Stops the application, and returns the app struct."
  ([]
     (stop *app*))
  ([app]
     (reset! (:stopped? app) true)))

(defn pause
  "Halts main loop, and yields control back to the REPL."
  ([]
     (pause *app*))
  ([app]
     (reset! (:paused? app) true)
     (reset! (:latch app) (CountDownLatch. 1))))

(defn- destroy
  ([]
     (destroy *app*))
  ([app]
     (try-callback :close)
     (input/destroy)
     (window/destroy)))

(defn- init
  ([]
     (init *app*))
  ([app]
     (with-app app
       (when (:stopped? *app*)
         (window/init)
         (try-callback :init)
         (try-callback :reshape (concat [0 0] @(:size window/*window*)))
         (input/init)
         (reset! (:stopped? app) false)))))

(defn update-once
  "Runs through the main loop once."
  ([]
     (update-once *app*))
  ([app]
     (with-app app
       (input/handle-keyboard)
       (input/handle-mouse)
       (window/check-for-resize)
       (try-callback :update)
       (if (or (Display/isDirty) @(:invalidated? app))
         (do
           (reset! (:invalidated? app) false)
           (clear 0 0 0)
           (push-matrix
            ((-> app :callbacks :display) @(:state app))))
         (Thread/sleep 15))
       (if (Display/isCloseRequested)
         (stop)
         (Display/update)))))

(defn resume
  "Resumes an app which has been paused or stopped."
  ([]
     (resume *app*))
  ([app]
     ))

(defn- alter-callbacks [app]
  (if (:started app)
    app
    (->
    app
    (assoc :started true)
    (update-in
     [:callbacks]
     (fn [callbacks]
       (->
        callbacks
        (update-in [:update] #(timed-fn %))
        (update-in [:display] #(timed-fn %))))))))

(defn start
  "Starts a window from scratch, or from a closed state."
  ([callbacks state]
     (start (create callbacks state)))
  ([app]
     (let [app (alter-callbacks app)]
       (with-app app
         (try
          (init)
          (reset! (:paused? app) false)
          (when-let [latch @(:latch app)]
            (.countDown #^CountDownLatch latch))
          (primary-loop app update-once)
          (catch Exception e
            (reset! (:stopped? app) true)
            (throw e))
          (finally
           (when (:stopped? app)
             (destroy)))))
       app)))

(defn start-update-loop
  ([hz f]
     (start-update-loop *app* hz f))
  ([app hz f]
     (secondary-loop
      app
      hz
      #(do
         (swap! (:state app) f)
         (repaint)))))



