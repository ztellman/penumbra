;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.texture)

(use 'penumbra.opengl.core 'penumbra.opengl.geometry 'penumbra.opengl.view 'clojure.contrib.def)

(set! *warn-on-reflection* true)

(import '(java.nio ByteBuffer)
        '(com.sun.opengl.util BufferUtil)
        '(com.sun.opengl.util.texture TextureIO)
        '(java.io File))

;;;;;;;;;;;;;;;;;;;;;;;

(gl-import glTexCoord1d gl-tex-1)
(gl-import glTexCoord2d gl-tex-2)
(gl-import glBindTexture gl-bind-texture)
(gl-import glGenTextures gl-gen-textures)
(gl-import glTexParameteri tex-parameter)
(gl-import glTexEnvf tex-env)
(gl-import glPixelStorei gl-pixel-store)

(gl-import glTexImage1D gl-tex-image-1d)
(gl-import glTexImage2D gl-tex-image-2d)

(gl-import glTexSubImage1D gl-tex-sub-image-1d)
(gl-import glTexSubImage2D gl-tex-sub-image-2d)

(gl-import glCopyTexSubImage1D gl-copy-tex-sub-image-1d)
(gl-import glCopyTexSubImage2D gl-copy-tex-sub-image-2d)

(gl-import glGetTexParameteriv gl-get-tex-parameter)

(glu-import gluBuild2DMipmaps glu-build-2d-mipmaps)
(glu-import gluBuild1DMipmaps glu-build-1d-mipmaps)

;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tex-struct :width :height :id :type)

(defn texture
  ([u] (gl-tex-1 u))
  ([u v] (gl-tex-2 u v)))

(defn bind-texture [t]
  (if (= 1 (:height t))
    (gl-bind-texture :texture-1d (:id t))
    (gl-bind-texture :texture-2d (:id t))))

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
  ([size]
    (let [id (gen-texture)]
      (gl-bind-texture :texture-1d id)
      (gl-tex-image-1d :texture-1d 0 :rgba size 0 :rgba :unsigned-byte (ByteBuffer/allocate (* size 4)))
      (tex-parameter :texture-1d :texture-wrap-s :clamp)                          ;set up target texture
      (tex-parameter :texture-1d :texture-min-filter :linear)
      (tex-parameter :texture-1d :texture-mag-filter :linear)
      (struct-map tex-struct :width size :height 1 :id id :type :byte)))
  ([width height]
    (let [id (gen-texture)]
      (gl-bind-texture :texture-2d id)
      (gl-tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte (ByteBuffer/allocate (* width height 4)))
      (tex-parameter :texture-2d :texture-wrap-s :clamp)                          ;set up target texture
      (tex-parameter :texture-2d :texture-wrap-t :clamp)
      (tex-parameter :texture-2d :texture-min-filter :nearest)
      (tex-parameter :texture-2d :texture-mag-filter :nearest)
      (struct-map tex-struct :width width :height height :id id :type :byte))))

(defn load-texture-from-file [filename]
  (let [tex (TextureIO/newTexture (File. filename) false)]
    (struct-map tex-struct :width (.getWidth tex) :height (.getHeight tex) :id (.getTextureObject tex) :type :byte)))

(defn create-float-texture ;this will be used in the future for GPGPU
  [size]
  (let [id (gen-texture)]
    (gl-tex-image-1d :texture-1d 0 :rgba size 0 :rgba :float (ByteBuffer/allocate (* size 4)))
    (struct-map tex-struct :width size :height 1 :id id :contents :type :float)))

;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro- to-byte [num] `(byte (* 255 (double ~num)))) ;this is a macro for performance reasons

(defn populate-1d-texture [size fun]
  (let [buf (ByteBuffer/allocate (* size 4))]
    (dotimes [idx size]
      (let [[r g b a] (fun idx (/ idx (double size)))]
         (doto buf
           (.put (to-byte r))
           (.put (to-byte g))
           (.put (to-byte b))
           (.put (to-byte a)))))
    (.rewind buf)
    buf))

(defn populate-2d-texture [w h fun]
  (let [buf (ByteBuffer/allocate (* w h 4))]
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
  (tex-parameter :texture-2d :texture-mag-filter :linear-mipmap-linear)
  (if (= (:height tex) 1)
    (glu-build-1d-mipmaps :texture-1d :rgba (:width tex) :rgba :unsigned-byte (populate-1d-texture (:width tex) fun))
    (glu-build-2d-mipmaps :texture-2d :rgba (:width tex) (:height tex) :rgba :unsigned-byte (populate-2d-texture (:width tex) (:height tex) fun))))

(defn draw-to-texture
  [tex fun]
  (bind-texture tex)
  (tex-parameter :texture-2d :texture-wrap-s :clamp)
  (tex-parameter :texture-2d :texture-wrap-t :clamp)
  (tex-parameter :texture-2d :texture-min-filter :linear)
  (tex-parameter :texture-2d :texture-mag-filter :linear)
  (if (= (:height tex) 1)
    (gl-tex-sub-image-1d :texture-1d 0 0 (:width tex) :rgba :unsigned-byte (populate-1d-texture (:width tex) fun))
    (gl-tex-sub-image-2d :texture-2d 0 0 0 (:width tex) (:height tex) :rgba :unsigned-byte (populate-2d-texture (:width tex) (:height tex) fun))))
  
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


