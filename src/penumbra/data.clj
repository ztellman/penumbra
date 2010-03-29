;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.data)

(defprotocol Data
  (acquire! [d])
  (release! [d])
  (destroy! [d])
  (refcount [d])
  (permanent? [d])
  (unwrap [d])
  (overwrite! [d offset data])
  (overwrite! [d data])
  (sizeof [d])
  (mimic [d])
  (mimic [d dim])
  (signature [d]))

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
  (add! [c d]))

(defn create-cache
  ([]
     (create-cache 0 0))
  ([max-count max-size]
     (let [max-count (ref max-count)
           max-size (ref max-size)
           cache (ref [])
           total-size #(apply + (map sizeof @cache))
           total-count #(count @cache)]
       (reify
        DataCache
        (max-count!
         [count]
         (dosync (ref-set max-count count)))
        (max-size!
         [size]
         (dosync (ref-set max-size size)))
        (locate!
         [sig]
         (dosync
          (let [match (first #(= sig (signature %)) @cache)]
            (when match
              (acquire! match))
            match)))
        (add!
         [data]
         (dosync
          (let [max-count @max-count
                max-size @max-size]
            (when (or
                   (and (pos? max-size) (>= (total-size) max-size))
                   (and (pos? max-count) (>= (total-count) max-count)))
              (let [[available not-available] (partition available? @cache)]
                (doseq [d available]
                  (destroy! d))
                (ref-set cache not-available)))
            (alter cache #(conj % data)))))))))


