;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.shadow
  (:use [penumbra opengl compute])
  (:require [penumbra.app :as app]))

(defn init [state]

  (defpipeline depth
    :vertex {:position (* :model-view-projection-matrix :vertex)
             pos (float3 :vertex)}
    :fragment (color (/ (+ 2 (.z pos)) 5)))

  (enable :depth-test)
  
  (assoc state
    :scene (create-display-list
            (teapot)
            (push-matrix
             (scale 5 1 5)
             (draw-quads
              (vertex 1 -1 -1) (vertex -1 -1 -1)
              (vertex -1 -1 1) (vertex 1 -1 1))))))

(defn reshape [[x y w h] state]
  (frustum-view 45 (/ w h) 0.1 20)
  state)

(defn mouse-drag [[dx dy] _ button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn display [_ state]
  (translate 0 0 -5)
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (let [draw-scene (:scene state)]
    (blit!
     (with-pipeline depth [(app/size)]
       (clear)
       (draw-scene)))))

(defn start []
  (app/start {:init init, :reshape reshape, :mouse-drag mouse-drag, :display display}
             {:rot-x 0, :rot-y 0}))
