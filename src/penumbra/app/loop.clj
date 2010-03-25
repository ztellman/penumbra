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
  (:require [penumbra.opengl.context :as context]
            [penumbra.app.controller :as controller]
            [penumbra.app.window :as window]
            [penumbra.app.input :as input]
            [penumbra.slate :as slate]
            [penumbra.time :as time]))

;;;

(defmacro with-app [app & body]
  `(let [app# ~app]
     (binding [*app* app#
               *clock* (:clock app#)
               *queue* (deref (:queue app#))
               *controller* (:controller app#)
               *event* (:event app#)]
      (input/with-input (:input app#)
        (window/with-window (:window app#)
          ~@body)))))

;;;

(defn timed-fn [clock f]
  (let [previous (atom (time/now clock))]
    (fn [& args]
      (let [now (time/now clock)]
        (try
         (apply f (list* [(- now @previous) now] args))
         (finally
          (reset! previous now)))))))

(defn create-thread [app outer-fn inner-fn]
  (let [context (context/current)
        slate (try
               (slate/create)
               (catch Exception e
                 nil))]
    (Thread.
     #(context/with-context context
        (with-app app
          (outer-fn
           (fn []
             (if slate
               (slate/with-slate slate
                 (inner-fn))
               (inner-fn)))))))))

(defn secondary-loop
  [app outer-fn inner-fn]
  (with-app app
    (outer-fn
     (fn []
       (loop []
         (controller/try-latch!)
         (try
          (inner-fn)
          (catch Exception e
            (.printStackTrace e)))
         (when-not (controller/stopped?)
           (recur)))))))

(defn secondary-thread
  [app outer-fn inner-fn]
  (create-thread
   app
   #(%)
   #(secondary-loop app outer-fn inner-fn)))

(defn primary-loop
  [app outer-fn inner-fn]
  (context/with-context nil
    (with-app app
     (outer-fn
      (fn []
        (loop []
          (inner-fn)
          (when-not (or (controller/paused?) (controller/stopped?))
            (recur))))))))