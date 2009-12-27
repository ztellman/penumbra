;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns examples.marble
  (:use [penumbra opengl geometry])
  (:require [penumbra.app :as app])
  (:require [penumbra.window :as window])
  (:use [penumbra.glsl.effects]))

;;;;;;;;;;;;;;;;;

(def declarations
  '((varying float noise)
    (varying float4 pos)
    (varying float4 intensity)))

(def vertex-shader
  '((import (penumbra.glsl.effects lighting))
    (let [pos         :vertex
          noise       (* 1.5 (noise1 pos))
          intensity   (lighting 0 (normalize (* :normal-matrix :normal)))]
      (set! :position (* :model-view-projection-matrix :vertex)))))

(def fragment-shader
  '(let [marble       (-> pos .x (+ noise) (* 3.0) sin abs)
         marble-color (float4 0.8 0.7 0.7 1.0)
         vein-color   (float4 0.2 0.15 0.1 1.0)
         mixed-color  (mix vein-color marble-color (pow marble 0.75))]
     (<- :frag-color (* intensity mixed-color))))

;;Sphere

(defn sphere-vertices
  [lod]
  (for [theta (range 0 361 (/ 360 lod))]
    (for [phi (range -90 91 (/ 180 (/ lod 2)))]
      (cartesian [theta phi 1]))))

(defn sphere-geometry [lod]
  (create-display-list
   (doseq [arcs (partition 2 1 (sphere-vertices lod))]
    (draw-quad-strip
     (doseq [[a b] (map list (first arcs) (second arcs))]
       (apply normal a) (apply vertex a)
       (apply normal b) (apply vertex b))))))

;;

(defn reshape [[x y w h] state]
  (frustum-view 60. (/ w (double h)) 0.1 10.)
  (load-identity)
  (translate 0 0 -3)
  (light 0
    :position [1 1 1 0])
  (material :front-and-back
    :ambient-and-diffuse [1 1 1 1]
    :specular            [0.5 0.4 0.4 1]
    :shininess           64)
  state)

(defn init [state]
  (enable :depth-test)
  (enable :lighting)
  (enable :light0)
  (window/vsync true)
  (assoc state
    :sphere (sphere-geometry 100)
    :program (create-program declarations vertex-shader fragment-shader)))

(defn mouse-drag [[dx dy] _ button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn display [[delta time] state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (with-program (:program state)
    (call-display-list (:sphere state))))

(defn start []
  (app/start
   {:reshape reshape, :display display, :init init, :mouse-drag mouse-drag}
   {:rot-x 0, :rot-y 0, :program nil}))

