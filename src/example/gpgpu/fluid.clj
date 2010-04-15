;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.gpgpu.fluid
  (:use [penumbra opengl compute])
  (:require [penumbra.app :as app]
            [penumbra.text :as text]
            [penumbra.data :as data]))

(defn init [state]

  (app/title! "Fluid")
  (app/vsync! false)

  (color 1 1 1)
  (point-size 10)
  
  (defmap reset
    (float3 0 0 0))
  
  (defmap diffuse
    (let [sum (+ %
                 (% (mod (+ :coord [1 0]) :dim))
                 (% (mod (+ :coord [-1 0]) :dim))
                 (% (mod (+ :coord [0 1]) :dim))
                 (% (mod (+ :coord [0 -1]) :dim)))]
      (max
       (float3 0)
       (- (/ (float3 sum)
             (float3 5))
          (float3 dampening)))))

  state)

(defn reshape [[x y w h] state]

  (when (:tex state)
    (data/destroy! (:tex state)))
  
  (assoc state
    :tex (reset [w h])))

(defn mouse-move [_ [x y] state]
  (render-to-texture
   (:tex state)
   (with-projection (ortho-view 0 1 0 1 -1 1)
     (draw-points
      (apply vertex (map / [x y] (app/size))))))
  state)

(defn update [_ state]
  (assoc state
    :tex (diffuse {:dampening 0.01} (:tex state))))

(defn display [[dt t] state]
  (blit (:tex state))
  (text/write-to-screen (str (int (/ 1 dt)) "fps") 0 0)
  (app/repaint!))

(defn start []
  (app/start {:display display, :update update, :mouse-move mouse-move, :reshape reshape, :init init}
             {}))