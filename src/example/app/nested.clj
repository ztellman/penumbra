;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.app.nested
  (:use [penumbra opengl text])
  (:require [penumbra.app :as app]))
 
(declare callbacks)
(declare start)
 
(defn key-press [key state]
  (cond
   (= key " ") (do
                 (start (update-in state [:count] inc))
                 (app/repaint!))
   (= key :escape) (app/stop!)
   :else nil))
 
(defn display [_ state]
  (write-to-screen (str "nested " (:count state) " deep!") 0 0))
 
(def callbacks {:key-press key-press :display display})
 
(defn start
  ([] (start {:count 1}))
  ([state] (app/start callbacks state)))