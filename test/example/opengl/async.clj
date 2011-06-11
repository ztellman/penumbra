;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.async
  (:use [penumbra.opengl])
  (:require [penumbra.app :as app]
            [penumbra.data :as data]
            [penumbra.opengl.texture :as tex]))

(def dim [64 64])

(defn xor [a b] (or (and a (not b)) (and (not a) b)))

(defn draw [tex]
  (data/overwrite! tex
    (apply concat
      (let [offset (int (* 5 (app/now)))]
	(for [x (range offset (+ offset (first dim))) y (range (second dim))]
	  (if (xor (even? (bit-shift-right x 3)) (even? (bit-shift-right y 3)))
	    [1 0 0 1]
	    [0 0 0 1]))))))

(defn swap [state]
  (let [[a b] (:textures state)]
    (draw b)
    (assoc state
      :textures [b a])))

(defn gen-tex [dim]
  (create-texture
   :dim dim
   :target :texture-2d
   :tex-min-filter :nearest
   :tex-max-filter :nearest))

(defn init [state]
  (app/title! "Async")
  (enable :texture-2d)
  (let [a (gen-tex dim)
        b (gen-tex dim)]
    (draw a)
    (draw b)
    (app/periodic-update! 2 (fn [_] (app/enqueue! swap)))
    (assoc state
      :textures [a b])))

(defn display [_ state]
  (blit (first (:textures state)))
  (app/repaint!))

(defn start []
  (app/start {:display display, :init init} {}))
