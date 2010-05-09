;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.event)

(defprotocol EventHandler
  (subscribe! [event hook f] "Subscribe to event 'hook' with callback 'f'.")
  (unsubscribe! [event hook f] "Unsubscribes callback 'f' from event 'hook'")
  (subscribe-once! [event hook f] "Subscribes callback 'f'. Once the callback is triggered, it is unsubscribed.")
  (publish- [event hook args] "Publishes an event. Any callbacks will be invoked with 'args'"))

(defn create []
  (let [event (ref {})]
    (reify
     EventHandler
     (subscribe!
      [_ hook f]
      (dosync
       (alter event (fn [e] (update-in e [hook] #(set (conj % f))))))
      nil)
     (unsubscribe!
      [_ hook f]
      (dosync
       (alter event (fn [e] (update-in e [hook] #(disj % f)))))
      nil)
     (publish-
      [_ hook args]
      (doseq [f (->> @event hook)]
        (apply f args))
      nil)
     (subscribe-once!
      [this hook f]
      (subscribe!
       this hook
       (letfn [(f* [& args]
                   (apply f args)
                   (unsubscribe! this hook f*))]
         f*))
      nil))))

(defn publish! [e hook & args]
  (publish- e hook args))