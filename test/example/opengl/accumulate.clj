;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.accumulate
  (:use [penumbra.opengl])
  (:require [penumbra.app :as app]
            [penumbra.data :as data]))

(defn init [state]
  (app/title! "Accumulate")
  (enable :texture-2d)
  (point-size 5)
  state)

(defn reshape [[x y w h] state]
  (ortho-view 0 w h 0 -1 1)
  (when-let [t (:tex state)]
    (data/destroy! t))
  (let [t (create-byte-texture w h)]
    (render-to-texture t (clear))
    (assoc state
      :tex t)))

(defn mouse-drag [_ [x y] button state]
  (assoc state
    :point [x y]))

(defn display [_ state]
  (render-to-texture (:tex state)
     (when-let [p (:point state)]
      (with-disabled :texture-2d
        (draw-points (apply vertex p)))))
  (blit (:tex state)))

(defn start []
  (app/start {:display display :mouse-drag mouse-drag :reshape reshape :init init} {}))