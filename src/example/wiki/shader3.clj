;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.wiki.shader3
  (:use [penumbra opengl compute])
  (:require [penumbra.app :as app]))

(defn init [state]
  
  (defpipeline program
    :attributes {alpha float}
    :vertex {pixel-alpha alpha
             :position (* :model-view-projection-matrix :vertex)}
    :fragment (float4 tint pixel-alpha))

  (enable :blend)
  (blend-func :src-alpha :one-minus-src-alpha)
  
  state)

(defn reshape [[x y w h] state]
  (frustum-view 45 (/ w h) 0.1 10)
  state)

(defn display [_ state]
  (translate -0.5 -0.5 -5)
  (blit!
   (with-pipeline program [{:tint [1. 1. 0.]} (app/size)]
     (clear)
     (draw-triangles
      (attribute :alpha 0.) (vertex 0 0)
      (attribute :alpha 0.5) (vertex 0 1)
      (attribute :alpha 1.) (vertex 1 1)))))

(defn start [] (app/start {:init init, :reshape reshape, :display display} {}))