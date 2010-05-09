;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.queue
  (:require [penumbra.app.loop :as loop]
            [penumbra.time :as time]
            [penumbra.app.core :as app]))

;;;

(defprotocol Queue
  (init- [q] "Initializes action queue.")
  (start-consumer-thread [q] "Starts a consumer thread for queue.")
  (enqueue- [q delay f] "Enqueues an action to be executed in 'delay' milliseconds")
  (periodic-enqueue- [q hz f] "Creates a recurring action to be executed 'hz' times a second"))

(defn- create-queue [app clock]
  (let [heap (ref (sorted-set-by #(- (compare (first %2) (first %1)))))
        queue (reify
               Queue
               (init- [this]
                      (start-consumer-thread this))
               (start-consumer-thread [_]
                                      (.start
                                       (Thread.
                                        (fn []
                                          (loop/basic-loop
                                           app
                                           (fn [x] (x))
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
                                               (Thread/sleep 1))))
                                          (println "exiting consumer thread")))))
               (enqueue- [this delay f]
                         (dosync
                          (alter heap #(conj % [(+ @clock delay) f])))
                         nil)
               (periodic-enqueue- [this hz f]
                                  (let [hz (atom hz)
                                        target (atom (+ @clock (/ 1 @hz)))]
                                    (letfn [(f* []
                                                (let [start @clock]
                                                  (binding [app/*hz* hz]
                                                    (f))
                                                  (let [hz @hz]
                                                    (when (pos? hz)
                                                      (enqueue- this (+ (/ 1 hz) (- @target start)) f*)
                                                      (swap! target #(+ % (/ 1 hz)))))))]
                                      (enqueue- this (/ 1 @hz) f*)))
                                  nil))]
    (init- queue)
    queue))

;;;

(defprotocol QueueHash
  (init! [q])
  (enqueue! [q clock delay f])
  (periodic-enqueue! [q clock hz f]))

(defn create [app]
  (let [hash (ref {})
        find-or-create (fn [clock]
                         (dosync
                          (if-let [q (@hash clock)]
                            q
                            (let [q (create-queue app clock)]
                              (alter hash #(assoc % clock q))
                              q))))]
    (reify
     QueueHash
     (init! [_]
            '(doseq [q (vals @hash)]
              (init- q)))
     (enqueue! [_ clock delay f]
               (enqueue- (find-or-create clock) delay f))
     (periodic-enqueue! [_ clock hz f]
                        (periodic-enqueue- (find-or-create clock) hz f)))))

