;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.app.switch
  (:use [penumbra opengl text])
  (:require [penumbra.app :as app]))

(declare apps)

(defn switch [a]
  (loop [app (apps a)]
    (when app
      (recur (-> app app/start deref :goto apps)))))

;; Controller

(defn controller-init [state]
  (switch :first))

;; First app

(defn first-init [state]
  (assoc state :goto nil))

(defn first-key-press [key state]
  (when (= key :escape)
    (app/stop!)
    (assoc state :goto :second)))

(defn first-display [_ state]
  (write-to-screen "first app" 0 0))

;; Second app

(defn second-init [state]
  (assoc state :goto nil))

(defn second-key-press [key state]
  (when (= key :escape)
    (app/stop!)
    (assoc state :goto :first)))

(defn second-display [_ state]
  (write-to-screen "second app" 0 0))

;;

(def apps {:controller (app/create {:init controller-init} {})
           :first (app/create {:init first-init, :key-press first-key-press, :display first-display} {})
           :second (app/create {:init second-init, :key-press second-key-press :display second-display} {})})

(defn start []
  (app/start (:controller apps)))