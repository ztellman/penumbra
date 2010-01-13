;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.slate
  (:use [clojure.contrib.def :only [defmacro- defn-memo defvar-]]
        [clojure.contrib.pprint]
        [clojure.contrib.seq-utils :only [separate]]
        [clojure.contrib.core :only [-?>]]
        [penumbra opengl]
        [penumbra.opengl core]
        [penumbra.app core])
  (:require [penumbra.opengl.texture :as texture])
  (:import [org.lwjgl.opengl Pbuffer PixelFormat]))

;;;

(defstruct slate-struct
  :pixel-buffer
  :frame-buffer
  :texture-pool
  :base-context?)

(defvar- *slate* nil)

(defn draw* []
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
       (call-display-list (force *frame-buffer-display-list*)))))

(defn create
  ([]
     (create nil))
  ([parent]
     (let [drawable (when-let [drawable-fn (-?> *app* :window :drawable)]
                      (drawable-fn))
           pixel-buffer (Pbuffer. 1 1 (-> (PixelFormat.)) drawable)]
       (.makeCurrent pixel-buffer)
       (let [frame-buffer (gen-frame-buffer)]
         (bind-frame-buffer frame-buffer)
         (struct-map slate-struct
           :drawable (constantly pixel-buffer)
           :pixel-buffer pixel-buffer
           :frame-buffer frame-buffer
           :texture-pool (or (:texture-pool parent) (atom (texture/create-texture-pool)))
           :base-context? (nil? parent))))))

(defn destroy
  ([]
     (destroy *slate*))
  ([slate]
     (when (:base-context? slate)
       (destroy-frame-buffer (:frame-buffer slate))
       (texture/destroy-textures (-> slate :texture-pool deref :textures)))
     (.destroy (:pixel-buffer slate))))

(defmacro with-slate [& body]
  `(let [slate# (create)]
     (binding [*slate* slate#
               *frame-buffer-display-list* (delay (create-display-list (draw*)))
               *texture-pool* (:texture-pool slate#)]
       (try
        ~@body
        (finally
         (destroy slate#))))))

;;;




