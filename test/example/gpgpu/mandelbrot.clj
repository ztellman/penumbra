;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.gpgpu.mandelbrot
  (:use [penumbra opengl compute])
  (:require [penumbra.app :as app]
            [penumbra.data :as data]))

(defn init [state]

  (app/title! "Mandelbrot Viewer")

  (defmap initialize-fractal
    (float3 (mix upper-left lower-right (/ :coord :dim)) 0))

  (defmap iterate-fractal
    (let [val %
          c (mix upper-left lower-right (/ :coord :dim))]   
      (dotimes [i num-iterations]
        (let [z (.xy val)
              iterations (.z val)]
          (<- val
            (if (< 4 (dot z z))
               val
               (float3
                (+ c
                   (float2
                    (- (* (.x z) (.x z)) (* (.y z) (.y z)))
                    (* 2 (.x z) (.y z))))
                (+ 1 iterations))))))
      val))

  (defmap color-fractal
    (let [val %
          z (.xy val)
          n (.z val)
          escape (-> n (- (-> z length log2 log2)) (/ (float max-iterations)))]
      (if (< 4 (dot z z))
         (color3 (mix [0 0 1] [1 1 1] escape))
         (color3 0 0 0))))

  state)

(defn reset-fractal [state]
  (when (:data state)
    (data/release! (:data state)))
  (when (:image state)
    (data/release! (:image state)))
  (assoc state
    :iterations 0
    :image nil
    :data nil))

(defn update-bounds [state]
  (let [[w h]  (:dim state)
        center (:offset state)
        radius (map #(/ % (:zoom state)) [(/ (float w) h) 1])
        ul     (map - center radius)
        lr     (map + center radius)]
    (assoc (reset-fractal state)
      :upper-left ul
      :lower-right lr)))

(defn mouse-down [[x y] button state]
  (let [ul    (:upper-left state)
        lr    (:lower-right state)
        [nx ny] (map / [x y] (:dim state))]
    (update-bounds
      (assoc state
        :zoom (max 1
                   (* (:zoom state)
                      (if (= button :left) 2 0.5)))
        :offset (map + ul (map * [nx (- 1 ny)] (map - lr ul)))))))

(defn reshape [[x y w h] state]
  (ortho-view 0 1 1 0 -1 1)
  (update-bounds
    (assoc state
      :dim [w h])))

(defn key-press [key state]
  (cond
   (= key :escape) (app/pause!)
   :else state))

(def iterations-per-frame 60)

(defn update [_ state]
  (let [max-iterations (* 20 (Math/pow (:zoom state) 0.5))]
    (if (< (:iterations state) max-iterations)
      (with-frame-buffer
        (let [ul      (:upper-left state)
              lr      (:lower-right state)
              iters   (+ (:iterations state) iterations-per-frame)
              data    (or
                       (:data state)
                       (initialize-fractal {:upper-left ul :lower-right lr} (:dim state)))
              next    (iterate-fractal {:upper-left ul :lower-right lr :num-iterations iterations-per-frame} data)
              image   (color-fractal {:max-iterations max-iterations} [next])]
          (assoc state
            :repaint true
            :iterations iters
            :data next
            :image image)))
      (assoc state
        :repaint false))))

(defn display [_ state]
  (when (:repaint state)
    (app/repaint!))
  (blit! (:image state)))

(defn start []
  (app/start
   {:init init, :reshape reshape, :update update, :display display, :mouse-down mouse-down, :key-press key-press}
   (reset-fractal {:upper-left [-2.0 1.0] :lower-right [1.0 -1.0] :zoom 1 :offset [-0.5 0]})))

