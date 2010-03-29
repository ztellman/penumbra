;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.context
  (:use [penumbra.opengl core])
  (:use [penumbra opengl])
  (:require [penumbra.opengl [texture :as texture]])
  (:import [org.lwjgl.opengl Display]))

;;;

(defn draw* []
  (with-projection (ortho-view -1 1 1 -1 -1 1)
    (push-matrix
     (load-identity)
     (gl-active-texture :texture0)
     (color 1 1 1)
     (draw-quads
      (texture 0 1) (vertex -1 -1 0)
      (texture 1 1) (vertex 1 -1 0)
      (texture 1 0) (vertex 1 1 0)
      (texture 0 0) (vertex -1 1 0)))))

(defn draw-frame-buffer
  ([w h]
     (draw-frame-buffer 0 0 w h))
  ([x y w h]
     (with-viewport [x y w h]
       (call-display-list (force *display-list*)))))

(defn current []
  (if-not *texture-pool*
    nil
    {:display-list *display-list*
     :texture-pool *texture-pool*
     :font-cache *font-cache*}))

(defn destroy []
  (texture/destroy-textures (:textures @*texture-pool*)))

(defmacro with-context [context & body]
  `(binding [*display-list* (or *display-list* (:display-list ~context) (delay (create-display-list (draw*))))
             *texture-pool* (or *texture-pool* (:texture-pool ~context) (atom (texture/create-texture-pool)))
             *font-cache* (or *font-cache* (:font-cache ~context) (atom {}))]
     ~@body))

