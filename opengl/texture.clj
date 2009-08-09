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
  (:import (java.nio ByteBuffer FloatBuffer))
  (:import (com.sun.opengl.util BufferUtil))
  (:import (com.sun.opengl.util.texture TextureIO))
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;

(gl-import glTexCoord1d gl-tex-1)
(gl-import glTexCoord2d gl-tex-2)
(gl-import glTexCoord3d gl-tex-3)

(gl-import glBindTexture gl-bind-texture)
(gl-import glGenTextures gl-gen-textures)
(gl-import glTexParameteri tex-parameter)
(gl-import glTexEnvf tex-env)
(gl-import glPixelStorei gl-pixel-store)

(gl-import glGetTexParameteriv gl-get-tex-parameter)

(gl-import glTexImage1D gl-tex-image-1d)
(gl-import glTexSubImage1D gl-tex-sub-image-1d)

(gl-import glTexImage2D gl-tex-image-2d)
(gl-import glTexSubImage2D gl-tex-sub-image-2d)
(gl-import glCopyTexSubImage2D gl-copy-tex-sub-image-2d)
(glu-import gluBuild2DMipmaps glu-build-2d-mipmaps)

(gl-import glTexImage3D gl-tex-image-3d)
(gl-import glTexSubImage3D gl-tex-sub-image-3d)

;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tex-struct :width :height :depth :id :type :tuple :persistant :attach-point)

(defn dimensions [texture]
  (count (filter #(not= 1 %) [(:width texture) (:height texture) (:depth texture)])))

(defn texture
  ([u] (gl-tex-1 u))
  ([u v] (gl-tex-2 u v))
  ([u v w] (gl-tex-3 u v w)))

(defn bind-texture [t]
  (let [dims (dimensions t)]
   (cond
    (= 1 dims) (gl-bind-texture :texture-1d (:id t))
    (= 2 dims) (gl-bind-texture :texture-2d (:id t))
    (= 3 dims) (gl-bind-texture :texture-3d (:id t)))))

(defn gen-texture []
  (let [a (int-array 1)]
    (gl-gen-textures 1 a 0)
    (nth a 0)))

(defmacro get-tex-parameter [dim param]
  `(let [ary# (int-array 1)]
    (gl-get-tex-parameter ~dim ~param ary# 0)
    (get-name (nth ary# 0))))

;;;;;;;;;;;;;;;;;;;;;;;

(defn init-texture-1d []
  (tex-parameter :texture-1d :texture-wrap-s :clamp)
  (tex-parameter :texture-1d :texture-min-filter :linear)
  (tex-parameter :texture-1d :texture-mag-filter :linear))

(defn init-texture-2d []
  (tex-parameter :texture-2d :texture-wrap-s :clamp)
  (tex-parameter :texture-2d :texture-wrap-t :clamp)
  (tex-parameter :texture-2d :texture-min-filter :linear)
  (tex-parameter :texture-2d :texture-mag-filter :linear))

(defn init-texture-3d []
  (tex-parameter :texture-3d :texture-wrap-s :clamp)
  (tex-parameter :texture-3d :texture-wrap-t :clamp)
  (tex-parameter :texture-3d :texture-wrap-r :clamp)
  (tex-parameter :texture-3d :texture-min-filter :linear)
  (tex-parameter :texture-3d :texture-mag-filter :linear))

(defn create-texture
  ([w]
     (let [id (gen-texture)]
       (gl-bind-texture :texture-1d id)
       (gl-tex-image-1d :texture-1d 0 :rgba w 0 :rgba :unsigned-byte (ByteBuffer/allocate (* w 4)))
       (init-texture-1d)
       (struct-map tex-struct :width w :height 1 :depth 1 :id id :type :unsigned-byte :tuple 4 :persistent true)))
  ([w h]
     (let [id (gen-texture)]
       (gl-bind-texture :texture-2d id)
       (gl-tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte (ByteBuffer/allocate (* w h 4)))
       (init-texture-2d)
       (struct-map tex-struct :width w :height h :depth 1 :id id :type :unsigned-byte :tuple 4 :persistent true)))
  ([w h d]
     (let [id (gen-texture)]
       (gl-bind-texture :texture-3d id)
       (gl-tex-image-3d :texture-3d 0 :rgba w h d 0 :rgba :unsigned-byte (ByteBuffer/allocate (* w h d 4)))
       (init-texture-3d)
       (struct-map tex-struct :width w :height h :depth d :id id :type :unsigned-byte :tuple 4 :persistent true))))

(def rgba (translate-keyword :rgba))

(defn- texture-from-texture-io [tex]
  (struct-map tex-struct
    :width (.getWidth tex)
    :height (.getHeight tex)
    :id (.getTextureObject tex)
    :type :unsigned-byte
    :tuple 4))

(defn load-texture-from-file [filename subsample]
  (let [data (TextureIO/newTextureData (File. filename) rgba rgba subsample "")
        tex  (TextureIO/newTexture data)]
    (texture-from-texture-io tex)))

(defn load-texture-from-image [image subsample]
  (let [data (TextureIO/newTextureData image rgba rgba subsample "")
        tex  (TextureIO/newTexture data)]
    (texture-from-texture-io tex)))

;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro to-byte [num] `(byte (* 255 (double ~num)))) ;this is a macro for performance reasons

(defn put [#^ByteBuffer buf [r g b a]]
  (doto buf
    (.put (to-byte r))
    (.put (to-byte g))
    (.put (to-byte b))
    (.put (to-byte a))))

(defn populate-buffer [fun texture]
  (let [dim (dimensions texture)
        w (:width texture)
        h (:height texture)
        d (:depth texture)
        #^ByteBuffer buf (ByteBuffer/allocate (* 4 w h d))]
    (cond
      (= 1 dim)
      (dotimes [x w]
        (put buf (fun [x] [(/ x (double h))])))
      (= 2 dim)
      (dotimes [x w] (dotimes [y h]
        (put buf (fun [x y] [(/ x (double w)) (/ y (double h))]))))
      (= 3 dim)
      (dotimes [x w] (dotimes [y h] (dotimes [z d]
        (put buf (fun [x y z] [(/ x (double w)) (/ y (double h)) (/ z (double d))]))))))
    (.rewind buf)
    buf))

(defn draw-to-subsampled-texture
  [tex fun]
  (bind-texture tex)
  (tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  (let [buf (populate-buffer fun tex)]
    (glu-build-2d-mipmaps :texture-2d :rgba (:width tex) (:height tex) :rgba :unsigned-byte buf)))

(defn draw-to-texture
  [tex fun]
  (bind-texture tex)
  (let [dims (dimensions tex)
        buf (populate-buffer fun tex)]
    (cond
      (= 1 dims)
      (gl-tex-sub-image-1d :texture-1d 0 0 (:width tex) :rgba :unsigned-byte buf)
      (= 2 dims)
      (gl-tex-sub-image-2d :texture-2d 0 0 0 (:width tex) (:height tex) :rgba :unsigned-byte buf)
      :else
      (gl-tex-sub-image-3d :texture-3d 0 0 0 0 (:width tex) (:height tex) (:depth tex) :rgba :unsigned-byte buf))))

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

