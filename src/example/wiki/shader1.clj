;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.wiki.shader1
  (:use [penumbra.opengl])
  (:require [penumbra.app :as app]))

(defn init [state]
  (def program (create-program
                :declarations '[(uniform float4 tint)]
                :vertex '(<- :position (* :model-view-projection-matrix :vertex))
                :fragment '(<- :frag-color tint)))
  state)

(defn reshape [[x y w h] state]
  (frustum-view 45 (/ w h) 0.1 10)
  state)

(defn display [_ state]
  (translate -0.5 -0.5 -5)
  (with-program program
    (uniform :tint 1. 1. 0. 1.)
    (draw-triangles
     (vertex 0 0 0)
     (vertex 0 1 0)
     (vertex 1 1 0))))

(app/start {:init init, :reshape reshape, :display display} {})