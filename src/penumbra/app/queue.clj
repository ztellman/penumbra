;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.queue
  (:use [penumbra.app.core])
  (:require [penumbra.slate :as slate]
            [penumbra.app.loop :as loop]
            [penumbra.time :as time]))

;;;

(defn- identity-outer [x]
  (x))

(defn- create-thread [app clock heap]
  (loop/create-thread
   app identity-outer
   (fn []
     (loop/secondary-loop
      app identity-outer
      (fn []
        (if-let [actions
                 (dosync
                  (let [now @clock
                        top (take-while #(>= now (first %)) @heap)]
                    (when-not (empty? top)
                      (alter heap #(apply disj (list* % top)))
                      top)))]
          (doseq [a (map second actions)]
            (a))
          (Thread/sleep 1)))))))

(defn- create
  ([]
     (create *app*))
  ([app]
     (create app (:clock app)))
  ([app clock]
     (let [heap (ref (sorted-set-by #(- (compare (first %2) (first %1)))))]
       (dotimes [_ 1]
         (.start (create-thread app clock heap)))
       (fn [delay f]
         (dosync
          (alter heap #(conj % [(+ @clock delay) f])))))))

;;;

(defn- find-or-create [app clock]
  (let [queues (:queues app)]
    (when-not (get @queues clock)
      (dosync
       (alter queues #(assoc % clock (create app clock)))))
    (get @queues clock)))

(defn update
  ([f]
     (update 0 f))
  ([delay f]
     (update *clock* delay f))
  ([clock delay f]
     (update *app* clock delay f))
  ([app clock delay f]
     ((find-or-create app clock)
      delay
      #(try
        (f)
        (catch Exception e
          (.printStackTrace e))))))

(defn periodic-update
  ([hz f]
     (periodic-update *clock* hz f))
  ([clock hz f]
     (periodic-update *app* clock hz f))
  ([app clock hz f]
     (let [hz (atom hz)
           target (atom (+ @clock (/ 1 @hz)))]
       (letfn [(f* []
                (let [start @clock]
                  (binding [*hz* hz]
                    (f))
                  (let [hz @hz]
                    (when (pos? hz)
                      (update app clock (+ (/ 1 hz) (- @target start)) f*)
                      (swap! target #(+ % (/ 1 hz)))))))]
         (update app clock (/ 1 @hz) f*)))))



