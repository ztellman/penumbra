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
            [penumbra.opengl.texture :as tex]
            [penumbra.data :as data]))

(defn overdraw! [tex [x y] col]
  (render-to-texture tex
   (with-projection (ortho-view 0 1 1 0 -1 1)
     (apply color col)
     (draw-points
      (apply vertex (map / [x y] (app/size)))))))

(defn init [state]

  (app/title! "Fluid")
  (app/vsync! false)

  (color 1 1 1)
  (point-size 10)
  
  (defmap reset-density
    (float3 0 0 0))

  (defmap reset-velocity
    (float3 0.5 0.5 0.5))

  (defmap diffuse
    (let [neighbors
          (+ (% (mod (+ :coord [1 0]) :dim))
             (% (mod (+ :coord [-1 0]) :dim))
             (% (mod (+ :coord [0 1]) :dim))
             (% (mod (+ :coord [0 -1]) :dim)))
          diffusion
          (* diff (.x :dim) (.y :dim) dt)]
      (max
       (float3 0)
       (- (/ (+ % (* diffusion neighbors)) (+ 1 (* 4 diffusion)))
          (float3 (* loss dt))))))

  (defmap advect
    (let [offset (* 10 (- (.xy %2) [0.5 0.5]))]
      (%1 (mod (+ :coord (* diff dt offset)) :dim))))

  state)

(defn reshape [[x y w h] state]

  (when (:density state)
    (data/destroy! (:density state))
    (data/destroy! (:velocity state)))
  
  (let [density (reset-density [w h])
        velocity (reset-velocity [w h])]
    (assoc state
      :density density 
      :velocity velocity)))

(defn mouse-move [[dx dy] [x y] state]
  (point-size 20)
  (overdraw! (:velocity state) [x y] (map #(+ 0.5 (/ % 10)) [(- dx) dy 0]))
  state)

(defn mouse-drag [_ [x y] button state]
  (point-size 50)
  (overdraw! (:density state) [x y] [1 1 1])
  state)

(defn update [[dt t] state]
  '(let [[w h] (tex/dim (:density state))]
    (overdraw! (:density state) [(/ w 2) h] [1 1 1])
    (overdraw! (:velocity state) [(/ w 2) h] [0.5 0 0]))
  (let [density (:density state)
        density (diffuse {:diff 100.0 :dt dt :loss 0.1} (:density state))
        density (advect {:diff 100.0 :dt dt} density [(:velocity state)])
        velocity (:velocity state)
        velocity (diffuse {:diff 100.0 :dt dt :loss 0.0} velocity)
        velocity (advect {:diff 100.0 :dt dt} velocity velocity)
        ]
    (assoc state
      :density density
      :velocity velocity)))

(defn display [[dt t] state]
  (blit (:density state))
  (text/write-to-screen (str (int (/ 1 dt)) "fps") 0 0)
  (app/repaint!))

(defn start []
  (app/start {:display display, :update update, :mouse-move mouse-move, :mouse-drag mouse-drag, :reshape reshape, :init init}
             {}))