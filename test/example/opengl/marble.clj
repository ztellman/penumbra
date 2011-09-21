;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.marble
  (:use [penumbra opengl compute]
        [penumbra.opengl core]
        [cantor])
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

(defn reset-random-texture
  ([state]
     (reset-random-texture 32 1 state))
  ([dim scale state]
     (when-let [tex (:tex state)]
       (data/destroy! tex))
     (assoc state
       :tex (wrap
             (take (* dim dim dim) (repeatedly #(float (- (rand (* 2 scale)) scale))))
             [dim dim dim]
             :target :texture-3d
             :texture-wrap-s :repeat
             :texture-wrap-t :repeat
             :texture-wrap-r :repeat
             :texture-min-filter :linear
             :texture-mag-filter :linear))))

(defn init [state]

  (defpipeline marble

    :vertex {position  (float3 :vertex)
             normal    (normalize (float3 (* :normal-matrix :normal)))
             :position (* :model-view-projection-matrix :vertex)}

    :fragment (let [noise 0
                    scale 0.5
                    pos (* position (float3 (/ 1 (.x (dim %)))))]
                (dotimes [i octaves]
                  (+= noise (* (% pos) scale))
                  (*= scale 0.5)
                  (*= pos (float3 2)))
                (let [marble (-> position .x (+ noise) (* 2) sin abs)
                      mixed (mix [0.2 0.15 0.1 1] [0.8 0.7 0.7 1] (pow marble 0.75))]
                  (* mixed (lighting 0 normal)))))
  
  (app/title! "Marble")
  (enable :depth-test)

  (reset-random-texture
    (assoc state
      :teapot (create-display-list (teapot))
      :octaves 6.0)))

(defn mouse-drag [[dx dy] _ button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn key-press [key state]
  (condp = key
    ;;"a" (update-in state [:octaves] inc)
    ;;"s" (update-in state [:octaves] #(max 0 (dec %)))
    " " (reset-random-texture state)
    state))

(defn display [[delta time] state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (color 1 0 0)
  (blit!
    (with-pipeline marble [{:octaves (:octaves state)} (app/size) [(:tex state)]]
     (clear)
     ;;(text/write-to-screen (str (int (/ 1 delta)) "fps") 0 0)  
     ((:teapot state)))))

(defn start []
  (app/start
   {:reshape reshape, :display display, :init init, :mouse-drag mouse-drag, :key-press key-press}
   {:rot-x 0, :rot-y 0}))

