;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.gpgpu.fluid
  (:use [penumbra opengl compute]
        [penumbra.opengl core]
        [cantor])
  (:require [penumbra.app :as app]
            [penumbra.text :as text]
            [penumbra.opengl.texture :as tex]
            [penumbra.data :as data]))

(def dim [400 300])

(defn textured-quad []
  (draw-quads
   (texture 0 0) (vertex -1 -1)
   (texture 1 0) (vertex 1 -1)
   (texture 1 1) (vertex 1 1)
   (texture 0 1) (vertex -1 1)))

(defn init-particles []
  (def particle-tex
       (let [[w h] [128 128]
             dim (vec2 w h)
             tex (create-byte-texture w h)]
      (data/overwrite!
       tex
       (apply concat
              (for [x (range w) y (range h)]
                (let [pos (div (vec2 x y) dim)
                      i (Math/exp (* 16 (- (length-squared (sub pos (vec2 0.5 0.5))))))]
                  [1 1 1 i]))))
      tex))
  (def particle-quad
    (create-display-list (textured-quad))))

(defn overdraw! [tex locs size [r g b]]
  (render-to-texture
   tex
   (with-enabled [:blend :texture-2d]
     (with-texture particle-tex
       (with-projection (ortho-view 0 1 1 0 -1 1)
         (color r g b (/ 1 (count locs)))
         (doseq [loc locs]
           (push-matrix
            (translate (div loc (apply vec2 (app/size))))
            (scale size size)
            (particle-quad))))))))

(defmacro def-fluid-map [name & body]
  `(defmap ~name
     (let [~'scale  ~'(/ 1 (.x :dim))
           ~'left   ~'(% (+ :coord [-1 0]))
           ~'right  ~'(% (+ :coord [1 0]))
           ~'top    ~'(% (+ :coord [0 1]))
           ~'bottom ~'(% (+ :coord [0 -1]))]
      ~@body)))

(defn init [state]

  (app/title! "Fluid")
  (app/vsync! true)

  (init-particles)
  (blend-func :src-alpha :one-minus-src-alpha)

  (defmap reset-density
    [0 0 0])

  (defmap reset-velocity
    [0 0 0])

  (defmap denormalize-velocity
    (- % [0.5 0.5 0.5]))

  (defmap normalize-velocity
    (+ % [0.5 0.5 0.5]))

  (defmap advect
    (let [offset (.xy %2)
          coord (- (floor :coord) (* diff dt offset (.x :dim))) 
          c0 (floor coord)
          c1 (+ c0 [1 1])
          t (- coord c0)
          t0 (% c0)
          t1 (% (float2 (.x c0) (.y c1)))
          t2 (% (float2 (.x c1) (.y c0)))
          t3 (% c1)]
      (mix
       (mix t0 t1 (.y t))
       (mix t2 t3 (.y t))
       (.x t))))

  (def-fluid-map diffuse
    (let [diffusion (* diff dt (.x :dim))
          val (/ (+ % (* diffusion (+ left right top bottom))) (+ 1 (* 4 diffusion)))]
      (* val (- 1 (* dt decay)))))

  (def-fluid-map jacobi
    (/ (+ left right top bottom (* (float3 alpha) %2)) (float3 (+ alpha beta))))

  (def-fluid-map divergence
    (* 0.5 scale (float3 (+ (.x (- right left)) (.y (- top bottom))))))

  (def-fluid-map gradient
    (let [val (float3 (.x (- right left)) (.x (- top bottom)) 0)]
      (+ %2 (* 0.5 (/ val scale)))))

  (defmap boundary-conditions
    (let [val %]
      (when (= 0 (floor (.x :coord)))
        (<- (.x val) (- (.x (% (+ :coord [1 0]))))))
      (when (= (- (.x :dim) 1) (floor (.x :coord)))
        (<- (.x val) (- (.x (% (+ :coord [-1 0]))))))
      (when (= 0 (floor (.y :coord)))
        (<- (.y val) (- (.y (% (+ :coord [0 1]))))))
      (when (= (- (.y :dim) 1) (floor (.y :coord)))
        (<- (.y val) (- (.y (% (+ :coord [0 -1]))))))
      
      val))

  state)


(defn reshape [[x y w h] state]

  (when (:density state)
    (data/destroy! (:density state))
    (data/destroy! (:velocity state)))
  
  (let [density (reset-density dim)
        velocity (reset-velocity dim)]
    
    (assoc state
      :density density 
      :velocity (normalize-velocity velocity))))

(defn mouse-move [[dx dy] [x y] state]
  (when (:velocity state)
    (let [finish (vec2 x y)
          start (sub finish (vec2 dx dy))
          steps (int (length (sub finish start)))
          locs (map #(lerp start finish (/ % steps)) (range steps))]
      (overdraw! (:velocity state) locs 0.03 (map #(+ (/ % 10) 0.5) (concat (map / (map #(* % 100) [dx (- dy)]) dim) [0 1])))
      (overdraw! (:density state) locs 0.03 [1 1 1])))

  state)

(defn pressure [velocity dt]
  (let [div (divergence [velocity])
        p  (loop [i 0 p (reset-density dim)]
             (if (> i 10)
               (do
                 (data/release! div)
                 p)
               (recur
                (inc i)
                (jacobi {:alpha 1.0 :beta 3.0} p [div]))))
        p (diffuse {:diff 0.1 :dt dt :decay 0.002} p)]
    p))

(defn project [velocity dt]
  (let [p (pressure velocity dt)
        velocity (gradient p velocity)
        velocity (boundary-conditions velocity)]
    velocity))

(defn update [[dt t] state]
  
  (let [dt dt]
    (let [velocity  (denormalize-velocity (:velocity state))
          velocity  (diffuse {:diff 1.0 :dt dt :decay 0.1} velocity)
          velocity  (project velocity dt)
          velocity  (boundary-conditions (advect {:diff 0.5 :dt dt} velocity velocity))
          ;;velocity  (project velocity dt)
          density   (:density state)
          density   (diffuse {:diff 1.0 :dt dt :decay 0.5} density)
          density   (advect {:diff 1.0 :dt dt} density [velocity])
          ]
      (assoc state
        :density density
        :velocity (normalize-velocity velocity))))
  )

(defn key-press [key state]
  (when (= key " ")
    (update-in state [:view-density] not)))

(defn display [[dt t] state]
  (blit ((if (:view-density state)
           :density
           :velocity)
          state))
  
  ;;(println (data/stats *texture-pool*))
  (text/write-to-screen (str (int (/ 1 dt)) "fps") 0 0)
  (app/repaint!))

(defn start []
  (app/start {:display display, :update update, :mouse-move mouse-move, :key-press key-press, :reshape reshape, :init init}
             {:view-density true}))