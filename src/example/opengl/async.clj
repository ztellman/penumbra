;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.async
  (:use [penumbra.opengl])
  (:require [penumbra.app :as app]))

(defn xor [a b] (or (and a (not b)) (and (not a) b)))

(defn draw [tex]
  (draw-to-subsampled-texture!
   tex
   (fn [[x y] _]
     (let [x (+ x (int (* 5 (app/now))))]
       (if (xor (even? (bit-shift-right x 4)) (even? (bit-shift-right y 4)))
         [1 0 0 1]
         [0 0 0 1])))))

(defn init [state]
  (enable :texture-2d)
  (app/periodic-update 10 #(draw (:tex %)))
  (assoc state
    :tex (create-byte-texture 128 128)))

(defn display [_ state]
  (blit (:tex state))
  (app/repaint!))

(defn start []
  (app/start {:display display, :init init} {}))