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
  (:require [penumbra.app.loop :as loop])
  (:import [java.util.concurrent Executors ExecutorService ThreadFactory]))

;;;

(defprotocol Queue
  (enqueue! [this delay f]))

(defn create-thread [app clock heap]
  (Thread.
   (fn []
     (loop/secondary-loop
      app #(%)
      (fn []
        (if-let [actions
                 (dosync
                  (let [now @@clock
                        top (take-while #(>= now (first %)) @heap)]
                    (when-not (empty? top)
                      (alter heap #(drop (count top) %))
                      (map second top))))]
          (doseq [a actions]
            (a))
          (Thread/sleep 0 500)))))))

(defn create
  ([]
     (create *app*))
  ([app]
     (let [clock  (:clock app)
           heap   (ref (sorted-set-by #(apply compare (map first %&))))]
       (dotimes [_ 1]
         (.start (create-thread app clock heap)))
       (reify
        Queue
        (enqueue! [delay f] (dosync (alter heap #(conj % [(+ @@clock delay) f]))))))))

;;;

(defn update
  ([f]
     (update 0 f))
  ([delay f]
     (update *queue* delay f))
  ([queue delay f]
     (enqueue!
      queue delay
      #(try
        (f)
        (catch Exception e
          (.printStackTrace e))))))

(defn recurring-update
  ([hz f]
     (recurring-update *app* hz f))
  ([app hz f]
     (let [queue @(:queue app)
           clock (:clock app)
           hz (atom hz)]
       (letfn [(f*
                []
                (let [start @@clock]
                  (binding [*hz* hz]
                    (f))
                  (let [hz @hz
                        end @@clock]
                    (when (pos? hz)
                      (update queue (-  (/ 1 hz) (- end start)) f*)))))]
         (update queue (/ 1 @hz) f*)))))



