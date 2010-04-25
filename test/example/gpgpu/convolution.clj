;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.gpgpu.convolution
  (:use [penumbra opengl compute])
  (:require [penumbra.app :as app]))

(defn draw-rect [x y w h]
  (with-disabled :texture-rectangle
    (draw-quads
     (vertex x y)
     (vertex (+ x w) y)
     (vertex (+ x w) (+ y h))
     (vertex x (+ y h)))))

(defn reset-image [tex]
  (render-to-texture tex
    (clear)                 
    (with-projection (ortho-view 0 2 0 2 -1 1)
      (dotimes [_ 100]
        (apply color (take 3 (repeatedly rand)))
        (apply draw-rect (take 4 (repeatedly rand))))))
  (app/repaint!)
  tex)

(defn init [state]

  (app/title! "Convolution")

  (defmap detect-edges
    (let [a (float4 0.0)
          b (float4 0.0)]
      (convolve %2
        (+= a (* %2 %1)))
      (convolve %3
        (+= b (* %3 %1)))
      (sqrt (+ (* a a) (* b b)))))

  (def filter-1
    (wrap (map float
               [-1 0 1
                -2 0 2
                -1 0 1])))

  (def filter-2
    (wrap (map float
               [1 2 1
                0 0 0
                -1 -2 -1])))

  (enable :texture-rectangle)
  (assoc state
    :tex (reset-image (create-byte-texture :texture-rectangle 512 512))))

(defn key-press [key state]
  (let [tex (:tex state)]
    (cond
     (= key " ")
     (assoc state
       :tex (reset-image tex))
     (= key :return)
     (assoc state
       :tex (detect-edges tex [filter-1] [filter-2])
       )
     :else
     state)))

(defn display [_ state]
  (blit (:tex state)))

(defn start []
  (app/start
   {:display display, :key-press key-press, :init init}
   {}))
