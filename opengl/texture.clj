;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.texture
  (:use [clojure.contrib.def :only (defmacro-)])
  (:use [penumbra.opengl.core])
  (:import (java.nio ByteBuffer))
  (:import (com.sun.opengl.util BufferUtil))
  (:import (com.sun.opengl.util.texture TextureIO))
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;

(gl-import glTexCoord1d gl-tex-1)
(gl-import glTexCoord2d gl-tex-2)
(gl-import glBindTexture gl-bind-texture)
(gl-import glGenTextures gl-gen-textures)
(gl-import glTexParameteri tex-parameter)
(gl-import glTexEnvf tex-env)
(gl-import glPixelStorei gl-pixel-store)

(gl-import glTexImage2D gl-tex-image-2d)
(gl-import glTexSubImage2D gl-tex-sub-image-2d)
(gl-import glCopyTexSubImage2D gl-copy-tex-sub-image-2d)
(gl-import glGetTexParameteriv gl-get-tex-parameter)
(glu-import gluBuild2DMipmaps glu-build-2d-mipmaps)

;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tex-struct :width :height :id :type)

(defn texture [u v] (gl-tex-2 u v))

(defn bind-texture [t]
 (gl-bind-texture :texture-2d (:id t)))

(defn gen-texture []
  (let [a (int-array 1)]
    (gl-gen-textures 1 a 0)
    (nth a 0)))

(defmacro get-tex-parameter [dim param]
  `(let [ary# (int-array 1)]
    (gl-get-tex-parameter ~dim ~param ary# 0)
    (get-name (nth ary# 0))))

;;;;;;;;;;;;;;;;;;;;;;;

(defn create-texture
  "Creates a new byte texture"
  ([width height]
  (let [id (gen-texture)]
    (gl-bind-texture :texture-2d id)
    (gl-tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte (ByteBuffer/allocate (* width height 4)))
    (tex-parameter :texture-2d :texture-wrap-s :clamp)
    (tex-parameter :texture-2d :texture-wrap-t :clamp)
    (tex-parameter :texture-2d :texture-min-filter :nearest)
    (tex-parameter :texture-2d :texture-mag-filter :nearest)
    (struct-map tex-struct :width width :height height :id id :type :byte))))

(defn load-texture-from-file [filename]
  (let [tex (TextureIO/newTexture (File. filename) false)]
    (struct-map tex-struct :width (.getWidth tex) :height (.getHeight tex) :id (.getTextureObject tex) :type :byte)))

;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro- to-byte [num] `(byte (* 255 (double ~num)))) ;this is a macro for performance reasons

(defn populate-2d-texture [w h fun]
  (let [#^ByteBuffer buf (ByteBuffer/allocate (* w h 4))]
    (dotimes [j w]
      (dotimes [i h]
        (let [[r g b a] (fun [i j] [(/ i (double w)) (/ j (double h))])]
          (doto buf
            (.put (to-byte r))
            (.put (to-byte g))
            (.put (to-byte b))
            (.put (to-byte a))))))
    (.rewind buf)
    buf))

(defn draw-to-subsampled-texture
  "Takes a function that returns normalized RGBA values for all texel coordinates, and applies it to the texture"
  [tex fun]
  (bind-texture tex)
  (tex-parameter :texture-2d :texture-wrap-s :clamp)
  (tex-parameter :texture-2d :texture-wrap-t :clamp)
  (tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  (tex-parameter :texture-2d :texture-mag-filter :linear)
  (glu-build-2d-mipmaps :texture-2d :rgba (:width tex) (:height tex) :rgba :unsigned-byte (populate-2d-texture (:width tex) (:height tex) fun)))

(defn draw-to-texture
  [tex fun]
  (bind-texture tex)
  (tex-parameter :texture-2d :texture-wrap-s :clamp)
  (tex-parameter :texture-2d :texture-wrap-t :clamp)
  (tex-parameter :texture-2d :texture-min-filter :linear)
  (tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl-tex-sub-image-2d :texture-2d 0 0 0 (:width tex) (:height tex) :rgba :unsigned-byte (populate-2d-texture (:width tex) (:height tex) fun)))

(defmacro render-to-texture
  "Renders a scene to a texture."
  [tex & body]
  `(do
    (clear)
    (with-viewport [0 0 (:width ~tex) (:height ~tex)]
      (push-matrix
        ~@body)                              
      (bind-texture ~tex)
      (gl-copy-tex-sub-image-2d :texture-2d 0 0 0 0 0 (:width ~tex) (:height ~tex))
    (clear))))

