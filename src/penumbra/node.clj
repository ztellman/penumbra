;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.node
  (:use [clojure.walk]))

(defprotocol NodeProtocol
  (update-state [n f])
  (update-nodes [n f])
  (update-callbacks [n f])
  (update-region [n f])
  (update- [n event args]))

(deftype Node [state callbacks region nodes]
  clojure.lang.IPersistentMap
  NodeProtocol
  (update-state [this f] (update-in this [:state] f))
  (update-nodes [this f] (update-in this [:nodes] f))
  (update-callbacks [this f] (update-in this [:callbacks] f))
  (update-region [this f] (update-in this [:region] f))
  (update- [this event args] (apply (callbacks event) (list* this args))))

(defn update [node event & args]
  (update- node event args))
