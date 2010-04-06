;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.data
  (:use [penumbra.geometry :only (rectangle)]))

(defprotocol Data
  (acquire! [d] "Increments the reference count.")
  (release! [d] "Decrements the reference count")
  (destroy! [d] "Destroys and releases all related resources.")
  (refcount [d] "Returns the reference count.")
  (refcount! [d count] "Sets the reference count.")
  (permanent? [d] "Returns whether the data is permanent.")
  (permanent! [d flag] "Sets whether data is permanent.")
  (unwrap [d] "Returns the the contained data.")
  (overwrite! [d bounds data] [d data] "Overwrites contained data.")
  (sizeof [d] "Memory, in bytes, used by data.")
  (mimic [d] [d dim] "Returns an equivalent data container.")
  (signature [d] "Returns a data signature, where equivalent signatures implies equivalent containers.")
  (matches? [a b] "Returns true if the signature of 'a' is compatible with 'b' (this is not necessarily bidirectional)")
  (params [t] "The parameters used to create the data"))

(defn unwrap! [d]
  (let [v (unwrap d)]
    (release! d)
    v))

(defmacro with-acquired [d & body]
  `(do
     (acquire! d)
     ~@body
     (release! d)))

(defn- available? [d]
  (and (<= (refcount d) 0)
       (not (permanent? d))))

;;;

(defprotocol DataCache
  (max-count! [c count])
  (max-size! [c size])
  (locate! [c sig])
  (add! [c d])
  (stats [c])
  (remove! [c d])
  (clear! [c]))

(defn create-cache
  ([]
     (create-cache 0 0))
  ([max-count max-size]
     (let [max-count (ref max-count)
           max-size (ref max-size)
           cache (ref [])
           total-size (ref 0)
           total-count (ref 0)]
       (reify
        DataCache
        (max-count!
         [_ count]
         (dosync (ref-set max-count count)))
        (max-size!
         [_ size]
         (dosync (ref-set max-size size)))
        (locate!
         [_ data]
         (dosync
          (let [match (->> @cache
                           (filter available?)
                           (filter #(matches? data %))
                           first)]
            (when match
              (refcount! match 1))
            match)))
        (add!
         [_ data]
         (dosync
          (let [max-count @max-count
                max-size @max-size]
            (when (or
                   (and (pos? max-size) (>= @total-size max-size))
                   (and (pos? max-count) (>= @total-count max-count)))
              (let [[available not-available] (partition available? @cache)]
                (doseq [d available]
                  (destroy! d))
                (ref-set cache not-available)
                (alter total-size #(- % (apply + (map sizeof available))))
                (alter total-count #(- % (count available)))))
            (alter cache #(conj % data))
            (alter total-count inc)
            (alter total-size #(+ % (sizeof data))))))
        (stats
         [_]
         nil)
        (remove!
         [_ d]
         (dosync
          (alter cache #(remove (fn [x] (= x d)) %)))
         (destroy! d))
        (clear!
         [_]
         (dosync
          (doseq [d @cache]
            (destroy! d))
          (ref-set cache [])))))))

;;;

