;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.loop
  (:use [penumbra.opengl]
        [penumbra.app.core])
  (:require [penumbra.app.controller :as controller]
            [penumbra.app.window :as window]
            [penumbra.time :as time]))

;;;

;;;

(defn timed-fn
  "Creates a wrapper function which prepends any arguments with [dt t] in seconds."
  [clock f]
  (when f
    (let [previous (atom @clock)]
      (fn [& args]
        (let [now @clock]
          (try
           (apply f (list* [(- now @previous) now] args))
           (finally
            (reset! previous now))))))))

(defn create-thread
  "Creates a thread. 'outer-fn' is passed 'inner-fn' as its only argument."
  [app outer-fn inner-fn]
  (Thread.
   #(with-app app
      (outer-fn inner-fn))))

(defn pauseable-loop
  [app outer-fn inner-fn]
  (with-app app
    (try
     (outer-fn
      (fn []
        (loop []
          (controller/wait! app)
          (try
           (inner-fn)
           (catch Exception e
             (.printStackTrace e)
             (controller/stop! app :exception)))
          (when-not (controller/stopped? app)
            (recur)))))
     (catch Exception e
       (.printStackTrace e)
       (controller/stop! app :exception))
     (finally
      ))))

(defn pauseable-thread
  [app outer-fn inner-fn]
  (create-thread
   app
   #(%)
   #(pauseable-loop app outer-fn inner-fn)))

(defn basic-loop
  [app outer-fn inner-fn]
  (with-app app
    (try
     (outer-fn
      (fn []
        (loop []
          (try
           (inner-fn)
           (catch Exception e
             (.printStackTrace e)
             (controller/stop! app :exception)))
          (when-not (or (controller/paused? app) (controller/stopped? app))
            (recur)))))
     (catch Exception e
       (.printStackTrace e)
       (controller/stop! app :exception))
     (finally
      ))))