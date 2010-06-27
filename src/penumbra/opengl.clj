;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Zachary Tellman"}
  penumbra.opengl
  (:use [penumbra.opengl core]
        [clojure.contrib.def :only (defn-memo defmacro- defvar)])
  (:require [penumbra.opengl.texture :as tex]
            [penumbra.data :as data]
            [penumbra.opengl.frame-buffer :as fb]
            [penumbra.opengl.shader :as shader]
            [penumbra.opengl.effects :as fx]
            [penumbra.glsl.core :as glsl]
            [penumbra.opengl.geometry :as geometry]
            [penumbra.opengl.teapot :as t])
  (:import (org.lwjgl BufferUtils)
           (java.io File ByteArrayOutputStream ByteArrayInputStream)
           (javax.imageio ImageIO)
           (org.newdawn.slick.opengl InternalTextureLoader Texture TextureImpl)))

;;;

(defmacro- import-fn [sym]
  (let [m (meta (eval sym))
        m (meta (intern (:ns m) (:name m)))
        n (:name m)
        arglists (:arglists m)
        doc (:doc m)]
    (list `def (with-meta n {:doc doc :arglists (list 'quote arglists)}) (eval sym))))

;;;

(gl-import+ glEnable enable)
(gl-import+ glDisable disable)
(gl-import+ glIsEnabled enabled?)
(gl-import+ glGetString get-string)

(gl-import glMatrixMode gl-matrix-mode)
(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)
(gl-import glLoadIdentity gl-load-identity-matrix)

;;;

(defmacro with-enabled
  "Enables the param(s) within the inner scope.  Will subsequently disable field if and only if it was previously disabled."
  [param-or-param-seq & body]
  `(let [e# (filter
              (fn [a#] (not (enabled? (enum a#))))
              (if (not (sequential? ~param-or-param-seq)) [~param-or-param-seq] ~param-or-param-seq))]
     (doseq [b# e#]
       (enable (enum b#)))
     (try ~@body
      (finally
        (doall (map (fn [c#] (disable (enum c#))) e#))))))

(defmacro with-disabled
  "Disables the param(s) within the inner scope.  Will subsequently enable field if and only if it was previously enabled."
  [e & body]
  `(let [e# (filter
              (fn [a#] (enabled? (enum a#)))
              (if (not (sequential? ~e)) [~e] ~e))]
     (doseq [b# e#]
       (disable (enum b#)))
     (try ~@body
      (finally
       (doall (map (fn [c#] (enable (enum c#))) e#))))))

;;;

(defn get-extensions
  "Returns a set of strings representing all supported extensions."
  []
  (set (.split (get-string :extensions) " ")))

(defn-memo get-version
  "Gets the version of OpenGL that's supported."
  []
  (Float/parseFloat (re-find #"\d+\.\d+" (get-string :version))))

(defn-memo frame-buffer-supported? []
  (contains? (get-extensions) "GL_EXT_framebuffer_object"))

(defn shaders-supported? []
  (>= (get-version) 2.0))

;;;

(gl-import- glClear gl-clear)
(gl-import+ glClearColor clear-color)
(gl-import+ glDepthFunc depth-test)

(defn clear
  "Clears the depth and color buffers."
  ([]
     (gl-clear :depth-buffer-bit)
     (gl-clear :color-buffer-bit))
  ([r g b]
    (clear r g b 1))
  ([r g b a]
    (clear-color r g b a)
    (clear)))

(gl-import- glViewport gl-viewport)

(defn viewport
  "Sets the current render window."
  ([w h] (viewport 0 0 w h))
  ([x y w h]
     (reset! *view* [x y w h])
     (gl-viewport x y w h)))

(defmacro with-viewport
  "Sets the render window within the inner scope.

   'rect' is of the form [x y w h]."
  [rect & body]
  `(let [old-view# @*view*]
    (apply viewport ~rect)
    (try
      ~@body
      (finally
        (apply viewport old-view#)))))

(gl-import- glOrtho gl-ortho)
(gl-import- gluPerspective glu-perspective)

(defmacro with-projection
  "Sets the projection matrix within the inner scope.  'projection' must actively set the projection matrix, i.e. call (ortho-view ...)"
  [projection & body]
  `(do
    (gl-matrix-mode :projection) (gl-push-matrix) ~projection (gl-matrix-mode :modelview)
    ~@body
    (gl-matrix-mode :projection) (gl-pop-matrix) (gl-matrix-mode :modelview)))

(defn ortho-view
  "Create orthographic view, where distant objects don't get smaller."
  [left right bottom top near far]
  (gl-matrix-mode :projection)
  (gl-load-identity-matrix)
  (gl-ortho left right bottom top near far)
  (gl-matrix-mode :modelview))

(defn frustum-view
  "Create a standard perspective view."
  [fovy aspect near far]
  (gl-matrix-mode :projection)
  (gl-load-identity-matrix)
  (glu-perspective (double fovy) (double aspect) (double near) (double far))
  (gl-matrix-mode :modelview))

;;Geometry

(import-fn geometry/vertex)
(import-fn geometry/normal)
(import-fn geometry/texture)
(import-fn geometry/attribute)
(import-fn geometry/rotate)
(import-fn geometry/scale)
(import-fn geometry/color)
(import-fn geometry/translate)
(import-fn geometry/load-identity)
(import-fn geometry/declare-attributes)

(defmacro push-matrix [& body]
  `(geometry/with-transform- *renderer* (fn [] ~@body)))

(geometry/defn-draw :quads)
(geometry/defn-draw :line-strip)
(geometry/defn-draw :lines)
(geometry/defn-draw :triangle-strip)
(geometry/defn-draw :triangle-fan)
(geometry/defn-draw :quad-strip)
(geometry/defn-draw :triangles)
(geometry/defn-draw :polygon)
(geometry/defn-draw :line-loop)
(geometry/defn-draw :points)

;;Display Lists

(gl-import glCallList gl-call-list)
(gl-import glGenLists gl-gen-lists)
(gl-import glNewList gl-new-list)
(gl-import glEndList gl-end-list)
(gl-import- glDeleteLists gl-delete-lists)
(gl-import- glIsList gl-is-list)

(defn- display-list-id [display-list]
  (:display-list (meta display-list)))

(defn display-list?
  "Returns true if display-list is a valid display list within the render context."
  [display-list]
  (if (nil? display-list)
    false
    (gl-is-list (display-list-id display-list))))

(defn delete-display-list
  "Deletes display-list"
  [display-list]
  (gl-delete-lists (display-list-id display-list) 1))

(defn call-display-list
  "Executes a display list, which is generated using (get-display-list ...)"
  [display-list]
  (display-list))

(defmacro create-display-list
  "Bounds inner scope in glNewList() ... glEndList(), and returns the display list value."
  [& body]
  `(let [list# (gl-gen-lists 1)]
     (gl-new-list list# :compile)
     ~@body
     (gl-end-list)
     (with-meta
       #(gl-call-list list#)
       {:display-list list#})))

;;Effects

(gl-import+ glCullFace cull-face)
(gl-import+ glLineWidth line-width)
(gl-import+ glPointSize point-size)
(gl-import+ glHint hint)
(gl-import+ glBlendFunc blend-func)
(gl-import+ glShadeModel shade-model)

(import-fn fx/render-mode)
(import-fn fx/color)
(import-fn fx/material)
(import-fn fx/fog)
(import-fn fx/light)

(defmacro with-render-mode [mode & body]
  `(fx/with-render-mode ~mode (fn [] ~@body)))

;;Shader

(def uniform shader/uniform)

(defn- filter-translate [decl [type source]]
  [type
   (if (not= :vertex type)
     (glsl/translate-shader (remove #(= 'attribute (first %)) decl) source)
     (glsl/translate-shader decl source))])

(defn- literal-translate [[type source]]
  [type (glsl/translate-shader source)])

(defn create-program
  "Creates a program.  Possible params are :extensions, :declarations, :fragment, :vertex, :geometry."
  [& params]
  (let [params (apply hash-map params)
        decl (:declarations params)
        tuple #(list % (% params))
        extend (fn [[k v]] [k (str (:extensions params) "\n" v)])
        sources (->> [:vertex :fragment :geometry]
                     (map tuple)
                     (apply concat)
                     (apply hash-map)
                     (filter second)
                     (map #(if (:literal params)
                             (literal-translate %)
                             (filter-translate decl %)))
                     (map extend)
                     (apply concat)
                     )]
    (apply shader/compile-source sources)))

(defmacro with-program [program & body]
  `(shader/with-program ~program (fn [] ~@body)))

(defmacro try-with-program
  "Calls with-program only if shaders are supported."
  [program & body]
  `(let [inner# (fn [] ~@body)]
     (if (shaders-supported?)
       (with-program ~program
         (inner#))
       (inner#))))

;;Frame Buffers

(defmacro with-frame-buffer [& body]
  `(fb/with-frame-buffer (fn [] ~@body)))

;;Texture

(gl-import+ glTexEnvf tex-env)
(gl-import+ glTexParameteri tex-parameter)
(gl-import+ glIsTexture valid-texture-id?)

(import-fn tex/bind-texture)
(import-fn tex/texture)
(import-fn tex/create-texture)
(import-fn tex/build-mip-map)

(defmacro with-texture [tex & body]
  `(tex/with-texture ~tex (fn [] ~@body)))

(defmacro with-texture-transform [transform & body]
  `(tex/with-texture-transform (fn [] ~transform) (fn [] ~@body)))

(defn create-byte-texture
  "Creates a texture with pixel format :unsigned-byte.
   Valid targets include [:texture-rectangle :texture-2d]."
  ([w h]
     (create-byte-texture :texture-2d w h))
  ([target w h]
     (create-texture
      :target target
      :dim [w h]
      :internal-format :rgba
      :pixel-format :rgba
      :internal-type :unsigned-byte)))

(defn load-texture-from-image
  "Creates a texture from a BufferedImage.
   If subsample is set to true, the texture will have mip-maps.
   Possible filters include [:nearest :linear], default is :linear."
  ([image]
     (load-texture-from-image image false))
  ([image subsample]
     (load-texture-from-image image subsample :linear))
  ([image subsample filter]
     (let [output-stream (ByteArrayOutputStream.)]
       (ImageIO/write image "png" output-stream)
       (.clear (InternalTextureLoader/get))
       (let [input-stream (ByteArrayInputStream. (.toByteArray output-stream))
             texture (-> (InternalTextureLoader/get)
                         (.getTexture
                          input-stream
                          "png"
                          false
                          (enum filter)))]
         (let [tex (tex/texture-from-texture-object texture filter)]
           (when subsample
             (tex/build-mip-map tex))
           tex)))))

(defn load-texture-from-file
  "Loads a texture from an image file."
  ([path]
     (load-texture-from-file path false))
  ([path subsample]
     (load-texture-from-file path subsample :linear))
  ([path subsample filter]
     (load-texture-from-image (ImageIO/read (File. path)) subsample filter)))

(defn blit
  "Blits texture to the screen.  Ignores current projection matrix, but honors current transform matrix."
  ([tex]
     (blit tex [0 0 1 1]))
  ([tex [x y w h]]
     (when tex
       (let [target (-> tex data/params :target)
             [tw th] (if (= :texture-rectangle target)
                     (tex/dim tex)
                     [1 1])]
         (with-enabled target
           (with-texture tex
             (try-with-program nil
               (push-matrix
                (load-identity)
                (with-projection (ortho-view 0 1 1 0 -1 1)
                  (push-matrix
                   (color 1 1 1)
                   (draw-quads
                    (texture 0 0)  (vertex x (+ y h))
                    (texture tw 0) (vertex (+ x w) (+ y h))
                    (texture tw th)  (vertex (+ x w) y)
                    (texture 0 th)   (vertex x y))))))))))))

(defn blit!
  "Same as blit, but releases texture after rendering."
  ([tex]
     (blit! tex [0 0 1 1]))
  ([tex region]
     (blit tex region)
     (data/release! tex)))

(defn render-to-texture-
  [tex f]
  {:skip-wiki true}
  (let [z-offset (if (sequential? tex) (last tex) nil)
        tex (if (sequential? tex) (first tex) tex)
        [w h] (tex/dim tex)]
    (with-frame-buffer [w h]
      (binding [*render-target* tex
                *z-offset* z-offset]
        (with-viewport [0 0 w h]
          (fb/attach-textures [] [tex])
          (push-matrix
           (fb/with-depth-buffer [w h]
             (f))))))))

(defmacro render-to-texture
  "Renders a scene defined in the inner scope to 'tex'."
  [tex & body]
  `(render-to-texture- ~tex (fn [] ~@body)))

(defmacro render-to-layered-texture
  [tex & body]
  `(binding [*layered-texture?* true]
     (render-to-texture tex ~@body)))

;;

(defn teapot
  ([] (teapot 20))
  ([detail] (push-matrix (t/teapot detail 1))))

