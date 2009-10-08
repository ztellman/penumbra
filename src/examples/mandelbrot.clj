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
          (+ c (float2 (- (* (.x z) (.x z)) (* (.y z) (.y z))) (* 2.0 (.x z) (.y z)))) ;;next location on complex plane
          (+ 1.0 iterations)))))

  (defmap color-fractal
    (let [val %
          z (.xy val)
          n (.z val)
          value (/ n (float max-iterations))]
      (? (< 4.0 (dot z z))
         (color3 value value (mix 0.2 1.0 value))
         (color3 0.0 0.0 0.0))))

  (enable :texture-rectangle)
    
  state)

(defn reset-fractal [state]
  (if (:data state)
    (release! (:data state)))
  (if (:image state)
    (release! (:image state)))
  (assoc state
    :iterations 0
    :image nil
    :data nil))

(defn get-radius [state]
  (let [[w h] (:dim state)]
    (map #(/ % (:zoom state)) [(* (/ (float w) h) 1.5) 1])))

(defn update-bounds [state]
  (let [[w h]  (:dim state)
        center (:offset state)
        ul     (map - center (get-radius state))
        lr     (map + center (get-radius state))]
    (assoc (reset-fractal state)
      :upper-left ul
      :lower-right lr)))

(defn mouse-click [[x y] state]
  (let [ul    (:upper-left state)
        lr    (:lower-right state)
        coord (map / [x y] (:dim state))]
    (update-bounds
      (assoc state
        :zoom (* 2 (:zoom state))
        :offset (map + ul (map * coord (map - lr ul)))))))

(defn reshape [[x y w h] state]
  (ortho-view 0 0 1 1 -1 1)
  (update-bounds
    (assoc state
      :dim [w h])))

(defn update [_ state]
  (if (< (:iterations state) (* 30 (inc (Math/log (:zoom state)))))
    (with-frame-buffer
      (let [ul    (:upper-left state)
            lr    (:lower-right state)
            iters (inc (:iterations state))
            data  (or
                    (:data state)
                    (initialize-fractal {:upper-left ul :lower-right lr} (:dim state)))
            next  (iterate-fractal {:upper-left ul :lower-right lr} [data])
            image (color-fractal {:max-iterations 30} [[next]])]
        (bind-program nil)
        (if (:image state)
          (release! (:image state)))
        (assoc state
          :iterations iters
          :data next
          :image image)))
    state))

(defn display [_ state]
  (let [[w h] (:dim state)]
    (bind-texture (:image state))
    (draw-quads
      (texture 0 0) (vertex 0 0 0)
      (texture w 0) (vertex 1 0 0)
      (texture w h) (vertex 1 1 0)
      (texture 0 h) (vertex 0 1 0)))
  (repaint))

(start
  {:init init, :reshape reshape, :update update, :display display, :mouse-click mouse-click}
  (reset-fractal {:upper-left [-2.0 1.0] :lower-right [1.0 -1.0] :zoom 1 :offset [-0.5 0]}))

