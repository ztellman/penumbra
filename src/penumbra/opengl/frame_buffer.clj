;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.frame-buffer
  (:use [penumbra.opengl core]
        [penumbra.opengl.shader :only [uniform-location]]
        [clojure.contrib.def :only [defn-memo]]
        [clojure.contrib.seq :only [indexed]])
  (require [penumbra.opengl.texture :as tex])
  (:import [org.lwjgl BufferUtils]))

;;Render Buffers

(gl-import- glGenRenderbuffersEXT gl-gen-render-buffers)
(gl-import- glBindRenderbufferEXT gl-bind-render-buffer)
(gl-import- glRenderbufferStorageEXT gl-render-buffer-storage)
(gl-import- glFramebufferRenderbufferEXT gl-frame-buffer-render-buffer)
(gl-import- glDeleteRenderbuffersEXT gl-delete-render-buffers)

(defn gen-render-buffer
  "Creates a render buffer."
  []
  (let [buf (BufferUtils/createIntBuffer 1)]
    (gl-gen-render-buffers buf)
    (.get buf 0)))

(defn delete-render-buffer
  [rb]
  (gl-delete-render-buffers (-> (BufferUtils/createIntBuffer 1) (.put (int-array [rb])) .rewind)))

(defn bind-render-buffer
  "Binds a render buffer."
  [rb]
  (gl-bind-render-buffer :renderbuffer rb))

(defn attach-depth-buffer
  "Attaches a depth buffer to the currently bound render buffer."
  ([dim]
     (attach-depth-buffer (gen-render-buffer) dim))
  ([rb dim]
     (let [dim (vec dim)]
       (bind-render-buffer rb)
       (gl-render-buffer-storage :renderbuffer :depth-component24 (dim 0) (dim 1))
       (gl-frame-buffer-render-buffer :framebuffer :depth-attachment :renderbuffer rb))))

(defmacro with-depth-buffer [dim & body]
  `(let [rb# (gen-render-buffer)]
     (attach-depth-buffer rb# ~dim)
     (try
      ~@body
      (finally
       (delete-render-buffer rb#)))))

;;Frame Buffers

(gl-import- glGenFramebuffersEXT gl-gen-frame-buffers)
(gl-import- glBindFramebufferEXT gl-bind-frame-buffer)
(gl-import- glCheckFramebufferStatusEXT gl-check-frame-buffer-status)
(gl-import- glDeleteFramebuffersEXT gl-delete-frame-buffers)
(gl-import- glFramebufferTextureEXT gl-frame-buffer-texture)
(gl-import- glFramebufferTexture2DEXT gl-frame-buffer-texture-2d)
(gl-import- glFramebufferTexture3DEXT gl-frame-buffer-texture-3d)
(gl-import- glFramebufferTextureLayerEXT gl-frame-buffer-texture-layer)
(gl-import- glDrawBuffers gl-draw-buffers)
(gl-import- glDrawBuffer draw-buffer)
(gl-import- glReadBuffer gl-read-buffer)
(gl-import- glBindTexture gl-bind-texture)
(gl-import- glUniform1i gl-uniform-1i)
(gl-import glReadPixels gl-read-pixels)
(gl-import glGetTexImage gl-get-tex-image)
(gl-import glActiveTexture gl-active-texture)

(defn gen-frame-buffer
  "Creates a single frame buffer object."
  []
  (let [buf (BufferUtils/createIntBuffer 1)]
    (gl-gen-frame-buffers buf)
    (.get buf 0)))

(defn destroy-frame-buffer
  "Destroys a single frame buffer object."
  [fb]
  (gl-delete-frame-buffers (-> (BufferUtils/createIntBuffer 1) (.put (int-array [fb])) .rewind)))

(defn bind-frame-buffer
  "Binds a frame buffer object."
  [fb]
  (gl-bind-frame-buffer :framebuffer fb))

(defn frame-buffer-ok?
  "Checks whether the current frame buffer object has valid attachments."
  []
  (let [status (gl-check-frame-buffer-status :framebuffer)]
    (when-not (= status (enum :framebuffer-complete))
      (throw (Exception. (str "Invalid framebuffer: " (enum-name status)))))
    true))

(defn-memo texture-lookup
  "Given n, returns integer value of GL_TEXTURE(n)"
  [point]
  (enum (keyword (str "texture" point))))

(defn-memo attachment-lookup [point]
  (enum (keyword (str "color-attachment" point))))

(defn attach
  "Attaches a texture to point n."
  [tex point]
  (let [p (attachment-lookup point)]
    (if (nil? tex)
      (gl-frame-buffer-texture-2d :framebuffer p :texture-rectangle 0 0)
      (condp = (count (tex/dim tex))
        2 (gl-frame-buffer-texture-2d :framebuffer p (tex/target tex) (tex/id tex) 0)
        3 (if (nil? *z-offset*)
            (if *layered-target?*
              (gl-frame-buffer-texture :framebuffer p (tex/id tex) 0)
              (throw (Exception. "A layered render target must have a z-offset defined.")))
            (condp = (tex/target tex)
              (enum :texture-3d)
              (do
                (gl-frame-buffer-texture-3d :framebuffer p (tex/target tex) (tex/id tex) 0 *z-offset*))
              (enum :texture-2d-array)
              (do
                (gl-frame-buffer-texture-2d :framebuffer p (tex/target tex) (tex/id tex) 0)
                (gl-frame-buffer-texture-layer :framebuffer p (tex/id tex) 0 *z-offset*))))))))

(defn bind-read
  "Binds a texture as a uniform variable to point n."
  [variable tex point]
  (let [loc (uniform-location variable)]
    (gl-active-texture (texture-lookup point))
    (gl-bind-texture (tex/target tex) (tex/id tex))
    (gl-uniform-1i loc point)))

(defn bind-write
  "Defines which textures will be written to, where textures are defined by their attachmennt points."
  [start end]
  (let [buffers (int-array (map attachment-lookup (range start end)))]
    (gl-draw-buffers (-> (BufferUtils/createIntBuffer (count buffers)) (.put buffers) .rewind))))

(defn attach-textures
  "Attaches read and write textures, where read textures are a hash with names as keys, and write textures are a standard seq."
  [read write]
  (let [read-textures (map last (partition 2 read))]
    (doseq [[idx tex] (indexed write)]
      (attach tex idx))
    (when-not (empty? write)
      (doseq [idx (range (count write) 8)]
        (attach nil idx))) ;;shouldn't this be unnecessary?
    (doseq [[idx [vr tex]] (indexed (partition 2 read))]
      (bind-read vr tex idx))
    (when-not (empty? write)
      (bind-write 0 (count write)))))

(defn with-frame-buffer
  "Renders anything within the inner scope to a frame buffer."
  [f]
  (if *frame-buffer*
    (f)
    (let [fb (gen-frame-buffer)]
      (binding [*frame-buffer* fb]
        (bind-frame-buffer fb)
        (try
         (f)
         (finally
          (bind-frame-buffer 0)
          (destroy-frame-buffer fb)))))))

(defn with-saved-frame-buffer
  [fb f]
  (binding [*frame-buffer* fb]
    (bind-frame-buffer fb)
    (try
     (f)
     (finally
      (bind-frame-buffer 0)))))
