;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.marble
  (:use [penumbra opengl geometry compute]
        [penumbra.opengl core teapot])
  (:require [penumbra.app :as app]
            [penumbra.text :as text]
            [penumbra.data :as data]))

;;;;;;;;;;;;;;;;;

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

  (defpipeline marble
    :vertex {position (float3 :vertex)
             noise    (noise1 position)    
             normal   (normalize (float3 (* :normal-matrix :normal)))
             :position    (* :model-view-projection-matrix :vertex)}
    :fragment (let [marble (-> position .x (+ noise) (* 3) sin abs)
                    mixed (mix [0.2 0.15 0.1 1] [0.8 0.7 0.7 1] (pow marble 0.75))]
                (* mixed (lighting (int 0) normal)))) 
  
  (app/title! "Marble")
  (enable :depth-test)
  (assoc state
    :teapot (create-display-list (teapot 20 1))))

(defn mouse-drag [[dx dy] _ button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn display [[delta time] state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (color 1 0 0)
  (blit!
   (with-pipeline marble [(app/size)]
     (clear)
     '(text/write-to-screen (format "%d fps" (int (/ 1 delta))) 0 0)  
     ((:teapot state))))
  '(app/repaint!))

(defn start []
  (app/start
   {:reshape reshape, :display display, :init init, :mouse-drag mouse-drag}
   {:rot-x 0, :rot-y 0, :program nil}))

