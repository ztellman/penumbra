;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns examples.mandelbrot
  (:use [penumbra opengl compute window])
  (:use [penumbra.opengl.texture]))

(defn init [state]

  (defmap initialize-fractal
    (float3 (mix upper-left lower-right (/ :coord :dim)) 0))

  (defmap iterate-fractal
    (let [val %
          z (.xy val)
          c (mix upper-left lower-right (/ :coord :dim))
          iterations (.z val)]
      (? (< 4.0 (dot z z))
         val
         (float3
          (float2 (- (pow (.x z) 2.0) (pow (.y z) 2.0)) (* 2.0 (.x z) (.y z))) ;;next location on complex plane
          (+ 1.0 iterations)))))

  (defmap color-fractal
    (let [val %
          z (.xy val)
          iterations (.z val)]
      (? (< 4.0 (dot z z))
         (float3 (- 1.0 (/ iterations (float max-iterations))))
         (float3 0.0 0.0 0.0))))


  (enable :texture-rectangle)
    
  state)

(defn reshape [[x y w h] state]
  (let [aspect (/ (float w) h)
        height (if (> 1 aspect) (/ 1.0 aspect) 1)
        aspect (max 1 aspect)]
    (ortho-view (- aspect) (- height) aspect height -1 1)
    state))

(defn display [_ state]
  (let [tex (ref nil)]
    (with-frame-buffer [1 1]
      (let [upper-left [-2.0 1.0]
            lower-right [1.0 -1.0]
            size [300 300]
            coll (iterate
                    #(iterate-fractal {:upper-left upper-left :lower-right lower-right} [%])
                    (initialize-fractal {:upper-left upper-left :lower-right lower-right} size))
            result (color-fractal {:max-iterations 30} [(nth coll 30)])]
        (dosync (ref-set tex result))))
    (let [[w h] (:dim @tex)]
      (with-program nil
        (bind-texture @tex)
        (draw-quads
         (texture 0 0) (vertex -1 -1 0)
         (texture w 0) (vertex 1 -1 0)
         (texture w h) (vertex 1 1 0)
         (texture 0 h) (vertex -1 1 0))))
    (release! @tex)))

(start {:init init, :reshape reshape, :display display} {})

