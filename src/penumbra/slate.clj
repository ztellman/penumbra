;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.slate
  (:use [clojure.contrib.def :only [defmacro- defn-memo defvar-]])
  (:use [clojure.contrib.pprint])
  (:use [clojure.contrib.seq-utils :only [separate]])
  (:use [penumbra opengl])
  (:use [penumbra.opengl core])
  (:use [penumbra.opengl.texture :only [destroy-textures create-texture-pool]])
  (:import [org.lwjgl.opengl Pbuffer PixelFormat]))

;;;

(defstruct slate-struct
  :pixel-buffer
  :frame-buffer
  :texture-pool
  :display-list
  :base-context?)

(defvar- *slate* nil)

(defn- draw* []
  (with-projection (ortho-view 0 1 1 0 -1 1)
    (gl-active-texture :texture0)
    (push-matrix
      (load-identity)
      (draw-quads
       (texture 0 1) (vertex 0 0 0)
       (texture 1 1) (vertex 1 0 0)
       (texture 1 0) (vertex 1 1 0)
       (texture 0 0) (vertex 0 1 0)))))

(defn draw
  ([w h]
    (draw 0 0 w h))
  ([x y w h]
    (with-viewport [x y w h]
      (if *slate*
        (call-display-list @(:display-list *slate*))
        (draw*)))))

(defn create
  ([]
     (create nil))
  ([parent]
     (let [drawable (when-let [drawable (:drawable parent)]
                      (drawable))
           pixel-buffer (Pbuffer. 1 1 (-> (PixelFormat.)) drawable)]
       (.makeCurrent pixel-buffer)
       (let [frame-buffer (gen-frame-buffer)]
         (bind-frame-buffer frame-buffer)
         (struct-map slate-struct
           :drawable (constantly pixel-buffer)
           :pixel-buffer pixel-buffer
           :frame-buffer frame-buffer
           :texture-pool (or (:texture-pool parent) (create-texture-pool))
           :display-list (or (:display-list parent) (create-display-list (draw*)))
           :base-context? (nil? parent))))))

(defn destroy
  ([]
     (destroy *slate*))
  ([slate]
     (when (:base-context? slate)
       (destroy-frame-buffer (:frame-buffer slate))
       (destroy-textures (-> slate :texture-pool :textures)))
     (.destroy (:pixel-buffer slate))))

(defmacro with-slate [& body]
  `(let [slate# (create)]
     (try
      ~@body
      (finally
       (destroy slate#)))))

;;;




