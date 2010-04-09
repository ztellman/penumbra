;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.controller
  (:import [java.util.concurrent CountDownLatch]))

;;;

(defprotocol Controller
  (paused? [c])
  (pause! [c])
  (stopped? [c])
  (stop! [c] [c flag])
  (resume! [c])
  (invalidated? [c])
  (invalidated! [c flag])
  (wait! [c]))

;;;

(defn create []
  (let [paused? (ref false)
        stopped? (ref :initializing)
        invalidated? (ref true)
        latch (ref (CountDownLatch. 1))]
    (reify
     Controller
     (paused? [_] @paused?)
     (stopped? [_] @stopped?)
     (invalidated? [_] @invalidated?)
     (invalidated! [_ flag] (dosync (ref-set invalidated? flag)))
     (stop! [this] (stop! this true))
     (stop! [_ reason]
            (dosync
             (ref-set stopped? reason)))
     (pause! [_]
             (dosync
              (ref-set paused? true)
              (when-not @latch
                (ref-set latch (CountDownLatch. 1)))))
     (resume! [_]
              (dosync
               (ref-set paused? false)
               (ref-set stopped? false)
               (when @latch
                 (.countDown @latch)
                 (ref-set latch nil))))
     (wait! [_]
            (when-let [l @latch]
              (.await l))))))
