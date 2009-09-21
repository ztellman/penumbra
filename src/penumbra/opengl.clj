;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl
  (:use [clojure.contrib.def :only (defn-memo)])
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:use [penumbra.opengl core geometry shader texture])
  (:use [penumbra.glsl.core])
  (:import (javax.media.opengl GL2))
  (:import (javax.media.opengl.glu.gl2 GLUgl2))
  (:import (com.sun.opengl.util.gl2 GLUT))
  (:import (java.lang.reflect Field))
  (:import (java.awt Font))
  (:import (com.sun.opengl.util.awt TextRenderer))
  (:import (com.sun.opengl.util.texture TextureIO))
  (:import (java.io File)))

(defmacro bind-gl [#^javax.media.opengl.GLAutoDrawable drawable & body]
  `(binding [*gl* (.. ~drawable getGL getGL2)]
    ~@body))

;;;;;;;;;;;;;;;;;;;;;;

(gl-import glEnable enable)
(gl-import glDisable disable)
(gl-import glGetIntegerv get-integer)

(gl-import glMatrixMode gl-matrix-mode)
(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)
(gl-import glLoadIdentity gl-load-identity-matrix)

;;;;;;;;;;;;;;;;;;;;;;

(def *view-bounds* (ref [0 0 0 0]))

(gl-import glClear gl-clear)
(gl-import glClearColor clear-color)
(gl-import glDepthFunc depth-test)

(defn clear
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
  ([w h] (viewport 0 0 w h))
  ([x y w h]
    (dosync (ref-set *view-bounds* [x y w h]))
    (gl-viewport x y w h)))

(defmacro with-viewport [[x y w h] & body]
  `(let [[x# y# w# h#] @*view-bounds*]
    (gl-viewport ~x ~y ~w ~h)
    (try
      ~@body
      (finally
        (gl-viewport x# y# w# h#)))))

(gl-import- glOrtho gl-ortho)
(glu-import- gluPerspective glu-perspective)

(defmacro with-projection [projection & body]
  `(do
    (gl-matrix-mode :projection) (gl-push-matrix) ~projection (gl-matrix-mode :modelview)
    ~@body
    (gl-matrix-mode :projection) (gl-pop-matrix) (gl-matrix-mode :modelview)))

(defn ortho-view
  "Create orthographic view, where distant objects don't get smaller."
  [left top right bottom near far]
  (gl-matrix-mode :projection)
  (gl-load-identity-matrix)
  (gl-ortho left right bottom top near far)
  (gl-matrix-mode :modelview))

(defn frustum-view [fovx aspect near far]
  "Create a standard perspective view."
  (gl-matrix-mode :projection)
  (gl-load-identity-matrix)
  (glu-perspective (double fovx) (double aspect) (double near) (double far))
  (gl-matrix-mode :modelview))

;;;;;;;;;;;;;;;;;;;;;;;;;
;Geometry

(defmacro push-matrix [& body]
  `(binding [*transform-matrix* (if *inside-begin-end* (atom @*transform-matrix*) *transform-matrix*)]
    (if (not *inside-begin-end*) (gl-push-matrix))
      (try
        ~@body
        (finally
          (if (not *inside-begin-end*) (gl-pop-matrix))))))

(gl-facade-import glVertex3d vertex)
(gl-facade-import glNormal3d normal)
(gl-facade-import glRotated rotate)
(gl-facade-import glTranslated translate)
(gl-facade-import glScaled scale)
(gl-facade-import glLoadIdentity load-identity)

(defn- undo-translation [matrix] (vec (concat (subvec matrix 0 12) [0 0 0 0]))) ;we don't want to translate normals

(facade-transform vertex identity)
(facade-transform normal undo-translation)

(facade-multiply rotate penumbra.opengl.geometry/rotation-matrix)
(facade-multiply scale penumbra.opengl.geometry/scaling-matrix)
(facade-multiply translate penumbra.opengl.geometry/translation-matrix)

;This should really be the inverse matrix of the current model-view matrix
;Right now, it only resets the intra-primitive transformations
(facade-multiply load-identity penumbra.opengl.geometry/identity-matrix #(%2))

(defn-draw :quads)
(defn-draw :line-strip)
(defn-draw :lines)
(defn-draw :triangle-strip)
(defn-draw :triangle-fan)
(defn-draw :quad-strip)
(defn-draw :triangles)
(defn-draw :polygon)
(defn-draw :line-loop)
(defn-draw :points)

(glut-import- glutSolidTeapot glut-solid-teapot)
(defn teapot [] (glut-solid-teapot 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Texture

(gl-import glTexEnvf tex-env)
(gl-import glActiveTexture gl-active-texture)

(defn texture
  ([u] (gl-tex-coord-1 u))
  ([u v] (gl-tex-coord-2 u v))
  ([u v w] (gl-tex-coord-3 u v w)))

(defn bind-texture [t]
  (gl-bind-texture (enum (:target t)) (:id t)))

(defn destroy-texture [tex]
  (gl-delete-textures 1 (int-array (:id tex)) 0))

(defn create-color-texture [w h]
  (create-texture :texture-2d [w h] :rgba :rgba :unsigned-byte 4))

(defn load-texture-from-file [filename subsample]
  (let [rgba (enum :rgba)
        data (TextureIO/newTextureData (File. filename) rgba rgba subsample "")
        tex  (TextureIO/newTexture data)]
    (texture-from-texture-io tex)))

(defn load-texture-from-image [image subsample]
  (let [rgba (enum :rgba)
        data (TextureIO/newTextureData image rgba rgba subsample "")
        tex  (TextureIO/newTexture data)]
    (texture-from-texture-io tex)))

(defn draw-to-subsampled-texture
  [tex fun]
  (bind-texture tex)
  (tex-parameter (enum (:target tex)) :texture-min-filter :linear-mipmap-linear)
  (let [buf (populate-buffer fun tex)
        dim (vec (:dim tex))]
    (glu-build-2d-mipmaps
      (enum (:target tex))
      (enum (:internal-format tex))
      (dim 0) (dim 1)
      (enum (:pixel-format tex))
      (enum (:internal-type tex))
      buf)))

(defn draw-to-texture
  [tex fun]
  (bind-texture tex)
  (let [target  (enum (:target tex))
        p-f     (enum (:pixel-format tex))
        i-t     (enum (:internal-type tex))
        dim     (vec (:dim tex))
        buf     (populate-buffer fun tex)]
    (condp = (count (filter #(not= 1 %) dim))
      1 (gl-tex-sub-image-1d target 0 0 (dim 0) p-f i-t buf)
      2 (gl-tex-sub-image-2d target 0 0 0 (dim 0) (dim 1) p-f i-t buf)
      3 (gl-tex-sub-image-3d target 0 0 0 0 (dim 0) (dim 1) (dim 2) p-f i-t buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Display Lists

(gl-import- glCallList gl-call-list)
(gl-import- glGenLists gl-gen-lists)
(gl-import- glNewList gl-new-list)
(gl-import- glEndList gl-end-list)
(gl-import- glDeleteLists gl-delete-lists)
(gl-import- glIsList gl-is-list)

(defn is-display-list [display-list]
  (and
    (not (nil? display-list))
    (gl-is-list display-list)))

(defn delete-display-list [display-list]
  (gl-delete-lists display-list 1))

(defn call-display-list [display-list]
  (gl-call-list display-list))

(defmacro get-display-list [& body]
  `(let [list# (gl-gen-lists 1)]
    (gl-new-list list# :compile)
    ~@body
    (gl-end-list)
    list#))

(defmacro set-display-list
  "Points list-atom to a new list, and deletes the list it was previous pointing to."
  [list-atom & body]
  `(let [list# (get-display-list ~@body)]
    (if (is-display-list (deref ~list-atom)) (delete-display-list (deref ~list-atom)))
    (reset! ~list-atom list#)))

;;;;;;;;;;;;;;;;;;;;;;;
;Effects

(gl-import glColor3d color)
(gl-import glCullFace cull-face)
(gl-import glLineWidth line-width)

(gl-import- glPolygonMode gl-polygon-mode)
(gl-import- glLightfv set-light-array)
(gl-import- glLightf set-light)
(gl-import- glMaterialfv set-material-array)
(gl-import- glMaterialf set-material)
(gl-import- glFogfv set-fog)
(gl-import- glShadeModel shade-model)

(gl-import- glHint hint)

(defn light [num & params]
  (let [light-num (enum (keyword (str "light" num)))]
    (doseq [[property value] (partition 2 params)]
      (let [property (enum property)]
        (if (sequential? value)
          (set-light-array light-num property (float-array (count value) value) 0)
          (set-light light-num property value))))))

(defn material [side & params]
  (let [side (enum side)]
    (doseq [[property value] (partition 2 params)]
      (let [property (enum property)]
        (if (sequential? value)
          (set-material-array side property (float-array (count value) value) 0)
          (set-material side property value))))))

(defn fog [& params]
  (doseq [[property value] (partition 2 params)]
    (let [value (if (sequential? value) value [value])]
      (set-fog
        (enum property)
        (float-array (count value) (map #(if (keyword? %) (enum %) %) value))
        0))))

(defn draw-solid [] (gl-polygon-mode :front-and-back :fill))
(defn draw-wireframe [] (gl-polygon-mode :front-and-back :line))
(defn draw-point-cloud [] (gl-polygon-mode :front-and-back :point))

;;;;;;;;;;;;;;;;;;;;;;
;Shader

(defn create-program*
  [extensions vertex fragment]
  (let [vertex-source   (translate-shader vertex)
        fragment-source (translate-shader fragment)]
    (create-program-from-source
     (str extensions "\n" vertex-source)
     (str extensions "\n" fragment-source))))

(defn create-program
  "Creates a program from s-exprssions.  Declarations are specified first, and shared between both shaders."
  ([declarations vertex fragment]
     (create-program "" declarations vertex fragment))
  ([extensions declarations vertex fragment]
     (let [vertex-source   (translate-shader declarations vertex)
           fragment-source (translate-shader (filter #(not= 'attribute (first %)) declarations) fragment)]
       (create-program-from-source
        (str extensions "\n" vertex-source)
        (str extensions "\n" fragment-source)))))

(gl-import- glUseProgram gl-use-program)
(gl-import- glGetUniformLocation gl-get-uniform-location)

(defn bind-program
  [program]
  (gl-use-program (if (nil? program) 0 (:program program))))

(defmacro with-program [program & body]
  `(binding [*program* (:program ~program)]
    (bind-program ~program)
    ~@body))

(defn- int? [p]
  (let [cls (class p)]
    (if (or (= cls Integer) (= cls Integer/TYPE)) true false)))

(defn uniform [variable & args]
  (let [loc     (gl-get-uniform-location *program* (name variable))
        is-int  (int? (first args))
        args    (vec (map (if is-int int float) args))]
    (condp = (count args)
      1 (if is-int  (uniform-1i loc (args 0))
                    (uniform-1f loc (args 0)))
      2 (if is-int  (uniform-2i loc (args 0) (args 1))
                    (uniform-2f loc (args 0) (args 1)))
      3 (if is-int  (uniform-3i loc (args 0) (args 1) (args 2))
                    (uniform-3f loc (args 0) (args 1) (args 2)))
      4 (if is-int  (uniform-4i loc (args 0) (args 1) (args 2) (args 3))
                    (uniform-4f loc (args 0) (args 1) (args 2) (args 3))))))

;;;;;;;;;;;;;;;;
;Render Buffers
(gl-import- glGenRenderbuffers gl-gen-render-buffers)
(gl-import- glBindRenderbuffer gl-bind-render-buffer)
(gl-import- glRenderbufferStorage gl-render-buffer-storage)
(gl-import- glFramebufferRenderbuffer gl-frame-buffer-render-buffer)

(defn gen-render-buffer []
  (let [a (int-array 1)]
    (gl-gen-render-buffers 1 a 0)
    (nth a 0)))

(defn bind-render-buffer [rb]
  (gl-bind-render-buffer :renderbuffer rb))

(defn attach-depth-buffer [dim]
  (let [depth-buffer (gen-render-buffer)
        dim (vec dim)]
    (bind-render-buffer depth-buffer)
    (gl-render-buffer-storage :renderbuffer :depth-component24 (dim 0) (dim 1))
    (gl-frame-buffer-render-buffer :framebuffer :depth-attachment :renderbuffer depth-buffer)))

;;;;;;;;;;;;;;;;
;Frame Buffers

(gl-import- glGenFramebuffers gl-gen-frame-buffers)
(gl-import- glBindFramebuffer gl-bind-frame-buffer)
(gl-import- glCheckFramebufferStatus gl-check-frame-buffer-status)
(gl-import- glDeleteFramebuffers gl-delete-frame-buffers)
(gl-import- glFramebufferTexture2D gl-frame-buffer-texture-2d)
(gl-import- glDrawBuffers gl-draw-buffers)
(gl-import- glDrawBuffer draw-buffer)
(gl-import- glReadBuffer gl-read-buffer)
(gl-import- glReadPixels gl-read-pixels)

(defn gen-frame-buffer []
  (let [a (int-array 1)]
    (gl-gen-frame-buffers 1 a 0)
    (nth a 0)))

(defn get-frame-buffer []
  (let [ary (int-array 1)]
    (get-integer :framebuffer-binding ary 0)
    (first (seq ary))))

(defn destroy-frame-buffer [fb]
  (let [a (int-array [fb])]
    (gl-delete-frame-buffers 1 a 0)))

(defn bind-frame-buffer [fb]
  (gl-bind-frame-buffer :framebuffer fb))


(defn frame-buffer-ok? []
  (= (gl-check-frame-buffer-status :framebuffer) (enum :framebuffer-complete)))

(defn frame-buffer-status []
  (enum-name (gl-check-frame-buffer-status :framebuffer)))

(defn attachment [point]
  (enum (keyword (str "color-attachment" point))))

(defn attach [tex point]
  (let [p (attachment point)]
    (if (nil? tex)
      (gl-frame-buffer-texture-2d :framebuffer p :texture-rectangle 0 0)
      (do
        (gl-frame-buffer-texture-2d :framebuffer p (enum (:target tex)) (:id tex) 0)
        (attach! tex point)))))

(defn bind-read [variable tex point]
  (let [loc (gl-get-uniform-location *program* (.replace (name variable) \- \_))]
    (gl-active-texture (enum (keyword (str "texture" point))))
    (gl-bind-texture (enum (:target tex)) (:id tex))
    (uniform-1i loc point)))

(defn bind-write [start end]
  (gl-draw-buffers (- end start) (int-array (map attachment (range start end))) 0))

(defn attach-textures [read write]
  (let [read-textures (map #(last %) (partition 2 read))]
    (doseq [[idx tex] (indexed write)]
      (attach tex idx))
    (doseq [idx (range (count write) 8)]
      (attach nil idx))
    (doseq [[idx [vr tex]] (indexed (partition 2 read))]
      (bind-read vr tex idx))
    (if (not (empty? write))
      (bind-write 0 (count write)))))

(defmacro with-frame-buffer
  [& body]
    `(let [fb# (gen-frame-buffer)]
      (bind-frame-buffer fb#)
      (try
        ~@body
        (finally
          (destroy-frame-buffer fb#)))))

;;;;;;;;;;;;;;;;;;;;;;

(defmacro render-to-texture
  "Renders a scene to a texture."
  [tex & body]
  `(do
    (let [[w# h#] (:dim ~tex)]
      (with-frame-buffer [w# h#]
        (with-viewport [0 0 w# h#]
          (attach-depth-buffer [w# h#])
          (attach-textures [] [~tex])
          (clear)
          (push-matrix
            ~@body))))))

;;;;;;;;;;;;;;;;;;;;;;

(def #^TextRenderer *text* (TextRenderer. (Font. "Tahoma" java.awt.Font/PLAIN 20) true true))

(defn write-to-screen
  "writes string at normalized coordinates (x,y)"
  [string x y]
  (let [[_ _ w h] @*view-bounds*
        text-height (.. *text* (getBounds string) getHeight)]
    (.beginRendering *text* w h)
    (.draw *text* string (int (* x w)) (int (* y (- h text-height))))
    (.endRendering *text*)))
