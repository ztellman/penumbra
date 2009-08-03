;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.compute.core
  (:use [penumbra.opengl core geometry texture])
  (:import (java.nio FloatBuffer)))

(gl-import glGenFramebuffersEXT gl-gen-frame-buffers)
(gl-import glBindFramebufferEXT gl-bind-frame-buffer)
(gl-import glFramebufferTexture2DEXT gl-frame-buffer-texture-2d)
(gl-import glDrawBuffers gl-draw-buffers)
(gl-import glReadBuffer gl-read-buffer)
(gl-import glReadPixels gl-read-pixels)

(defn attachment [point]
  (translate-keyword (keyword (str "color-attachment" point "-ext"))))

(defn attach [tex point]
  (let [p (attachment point)]
    (gl-frame-buffer-texture-2d :framebuffer-ext p :texture-rectangle-arb 0 0) 
    (gl-frame-buffer-texture-2d :framebuffer-ext p :texture-rectangle-arb (:id tex) 0)))

(defn gen-frame-buffer []
  (let [a (int-array 1)]
    (gl-gen-frame-buffers 1 a 0)
    (nth a 0)))

(defn bind-frame-buffer [fb]
  (gl-bind-frame-buffer :framebuffer-ext fb))

(defn set-draw-buffers [bufs]
  (gl-draw-buffers (count bufs) (int-array (map attachment bufs))))

(def type-size
  {:luminance 1, :rgba 4})

(defn type-array [type size]
  (cond
    (= :float type) (float-array size)
    (= :int type)   (int-array size)))

(defn read-data
  ([tex] (read-data (* (:width tex) (:height tex))))
  ([tex size]
    (gl-read-buffer tex)
    (let [w (min size (:width tex))
          h (* w (-> size (/ w) Math/ceil int))
          diff (- size (* w h))
          a (type-array (:type tex) (* w h (type-size (:pixel tex))))]
      (gl-read-pixels 0 0 w h (:pixel tex) (:type tex) (FloatBuffer/wrap a))
      (if (zero? diff)
        (seq a)
        (let [tmp (type-array (:type tex) (count a))]
          (System/arraycopy a 0 tmp 0 (count a))
          (seq tmp))))))

(defn draw-quad [w h]
  (draw-quads
    (texture 0 0) (vertex 0 0)
    (texture w 0) (vertex 1 0)
    (texture w h) (vertex 1 1)
    (texture 0 h) (vertex 0 1)))

