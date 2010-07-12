;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.text
  (:use [penumbra opengl])
  (:require [penumbra [app :as app] [text :as text]]))

(defn display [_ _]
  (text/with-font (text/font "Inconsolata" :size 50)
    (text/write-to-screen "hello world" 0 0)
    (text/write-to-screen "hello world" 0 100)))

(defn start []
  (app/start {:display display} {}))
