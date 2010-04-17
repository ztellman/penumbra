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

(def dim [300 300])

(defmacro def-fluid-map [name & body]
  `(defmap ~name
     (let [~'scale  ~'(float3 (/ [0.5 0.5] :dim) 1)
           ~'left   ~'(% (mod (+ :coord [-1 0]) :dim))
           ~'right  ~'(% (mod (+ :coord [1 0]) :dim))
           ~'top    ~'(% (mod (+ :coord [0 1]) :dim))
           ~'bottom ~'(% (mod (+ :coord [0 -1]) :dim))]
      ~@body)))

(defn init [state]

  (app/title! "Fluid")
  (app/vsync! false)

  (defmap reset-density
    [0 0 0])

  (defmap reset-velocity
    [0.5 0.5 0.5])

  (defmap denormalize-velocity
    (* 10 (- % [0.5 0.5 0.5])))

  (defmap normalize-velocity
    (+ (/ % 10) [0.5 0.5 0.5]))

  (defmap advect
    (let [offset (.xy %2) 
          scaled-offset (* diff dt offset)]
      (%1 (mod (- :coord scaled-offset) :dim))))

  (def-fluid-map diffuse
    (let [diffusion (* diff (.x :dim) (.y :dim) dt)]
      (/ (+ % (* diffusion (+ left right top bottom))) (+ 1 (* 4 diffusion)))))

  (def-fluid-map jacobi
    (/ (+ left right top bottom (* (float3 alpha) %2)) (float3 beta)))

  (def-fluid-map divergence
    (* scale (float3 (+ (.x (- right left)) (.y (- bottom top))))))

  (def-fluid-map gradient
    (float3 (- %2 (* (float3 (.x (- right left)) (.y (- bottom top)) 0) scale))))

  state)

(defn overdraw! [tex [x y] col]
  (render-to-texture
   tex
   (with-projection (ortho-view 0 1 1 0 -1 1)
     (apply color col)
     (draw-points
      (apply vertex (map / [x y] (app/size)))))))

(defn reshape [[x y w h] state]

  (when (:density state)
    (data/destroy! (:density state))
    (data/destroy! (:velocity state)))
  
  (let [density (reset-density dim)
        velocity (reset-velocity dim)]
    
    (point-size 200)
    (overdraw! density (map #(/ % 2) (app/size)) [1 0.5 0])
    ;;(overdraw! velocity (map #(/ % 2) (app/size)) [0.5 0.5 0.5])
    
    (assoc state
      :density density 
      :velocity velocity)))

(defn mouse-move [[dx dy] [x y] state]
  (point-size 10)
  (overdraw! (:velocity state) [x y] (map #(+ (/ % 10) 0.5) [dx (- dy) 0 1]))
  state)

(defn update [[dt t] state]
  
  (when (app/button-pressed? :left)
    (point-size 20)
    (overdraw! (:density state) (app/mouse-location) [1 0.5 0]))
  
  (let [velocity  (denormalize-velocity (:velocity state))
        velocity  (diffuse {:diff 1.0 :dt dt} velocity)
        velocity  (advect {:diff 5.0 :dt dt} velocity velocity)
        div       (divergence [velocity])
        pressure  (loop [i 0 p (reset-density dim)]
                    (if (> i 20)
                      (do
                        (data/release! div)
                        p)
                      (recur (inc i) (jacobi {:alpha (* dt dt) :beta 4.0} p [div]))))
        velocity  (gradient pressure velocity)
        density   (:density state)
        density   (diffuse {:diff 1.0 :dt dt} density)
        density   (advect {:diff 5.0 :dt dt} density [velocity])]
    (assoc state
      :density density
      :velocity (normalize-velocity velocity))))

(defn key-press [key state]
  (when (= key " ")
    (update-in state [:view-density] not)))

(defn display [[dt t] state]
  (blit ((if (:view-density state)
           :density
           :velocity)
         state))
  ;;(text/write-to-screen (str (int (/ 1 dt)) "fps") 0 0)
  (app/repaint!))

(defn start []
  (app/start {:display display, :update update, :mouse-move mouse-move, :key-press key-press, :reshape reshape, :init init}
             {:view-density true}))