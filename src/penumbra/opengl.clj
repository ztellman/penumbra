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
  (:use [penumbra.glsl core])
  (:use [penumbra.geometry])
  (:import (java.lang.reflect Field))
  (:import (java.awt Font))
  (:import (java.nio ByteBuffer IntBuffer FloatBuffer))
  (:import (java.io File)))

;;;

(gl-import glEnable enable)
(gl-import glDisable disable)
(gl-import glIsEnabled enabled?)
(gl-import glGetInteger gl-get-integer)
(gl-import glGetString get-string)

(gl-import glMatrixMode gl-matrix-mode)
(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)
(gl-import glLoadIdentity gl-load-identity-matrix)

;;;

(defn get-integer
  "Calls glGetInteger."
  [param]
  (let [ary (int-array 1)]
    (gl-get-integer (enum param) (IntBuffer/wrap ary))
    (first ary)))

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

;;;

(gl-import glClear gl-clear)
(gl-import glClearColor clear-color)
(gl-import glDepthFunc depth-test)
(gl-import glFlush gl-flush)
(gl-import glFinish gl-finish)

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
  "Sets the render window within the inner scope."
  [[x y w h] & body]
  `(let [[x# y# w# h#] @*view*]
    (viewport ~x ~y ~w ~h)
    (try
      ~@body
      (finally
        (viewport x# y# w# h#)))))

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

(defn frustum-view [fovx aspect near far]
  "Create a standard perspective view."
  (gl-matrix-mode :projection)
  (gl-load-identity-matrix)
  (glu-perspective (double fovx) (double aspect) (double near) (double far))
  (gl-matrix-mode :modelview))

;;Geometry

(defmacro push-matrix [& body]
  `(binding [*transform-matrix* (if *inside-begin-end* (atom @*transform-matrix*) *transform-matrix*)]
    (if (not *inside-begin-end*) (gl-push-matrix))
      (try
        ~@body
        (finally
          (if (not *inside-begin-end*) (gl-pop-matrix))))))

(gl-facade-import glVertex3d gl-vertex)
(gl-facade-import glNormal3d gl-normal)
(gl-facade-import glRotatef gl-rotate)
(gl-facade-import glTranslated gl-translate)
(gl-facade-import glScaled gl-scale)
(gl-facade-import glLoadIdentity load-identity)

(defn- undo-translation [matrix] (vec (concat (subvec matrix 0 12) [0 0 0 0]))) ;;we don't want to translate normals

(facade-transform gl-vertex identity)
(facade-transform gl-normal undo-translation)

(facade-multiply gl-rotate penumbra.geometry/rotation-matrix)
(facade-multiply gl-scale penumbra.geometry/scaling-matrix)
(facade-multiply gl-translate penumbra.geometry/translation-matrix)

;; This should really be the inverse matrix of the current model-view matrix
;; Right now, it only resets the intra-primitive transformations
(facade-multiply load-identity penumbra.geometry/identity-matrix #(%2))

(defn vertex
  "Calls glVertex3d.
   [x y] -> [x y 0].
   [x y z w] -> [x y z]."
  ([x y] (gl-vertex x y 0))
  ([x y z] (gl-vertex x y z))
  ([x y z w] (gl-vertex x y z)))

(defn translate
  "Calls glTranslated.
   [x y] -> [x y 0]."
  ([x y] (gl-translate x y 0))
  ([x y z] (gl-translate x y z)))

(defn scale
  "Calls glScaled.
   [x y] -> [x y 1]."
  ([x y] (gl-scale x y 1))
  ([x y z] (gl-scale x y z)))

(defn normal
  "Calls glNormal3d."
  [x y z]
  (gl-normal x y z))

(defn rotate
  "Calls glRotated.  Rotates by 'angle' degrees, about the axis defined by [x y z]"
  [angle x y z]
  (gl-rotate angle x y z))

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

;;Display Lists

(gl-import- glCallList gl-call-list)
(gl-import- glGenLists gl-gen-lists)
(gl-import- glNewList gl-new-list)
(gl-import- glEndList gl-end-list)
(gl-import- glDeleteLists gl-delete-lists)
(gl-import- glIsList gl-is-list)

(defn display-list?
  [display-list]
  (if (nil? display-list)
    false
    (gl-is-list display-list)))

(defn delete-display-list [display-list]
  (gl-delete-lists display-list 1))

(defn call-display-list
  "Executes a display list, which is generated using (get-display-list ...)"
  [display-list]
  (gl-call-list display-list))

(defmacro get-display-list
  "Bounds inner scope in glNewList() ... glEndList(), and returns the display list value."
  [& body]
  `(let [list# (gl-gen-lists 1)]
    (gl-new-list list# :compile)
    ~@body
    (gl-end-list)
    list#))

(defmacro set-display-list
  "Points list-atom to a new list, and deletes the list it was previous pointing to."
  [list-atom & body]
  `(let [list# (get-display-list ~@body)]
    (if (display-list? (deref ~list-atom)) (delete-display-list (deref ~list-atom)))
    (reset! ~list-atom list#)))

;;Effects

(gl-import glColor3d color-3)
(gl-import glColor4d color-4)
(gl-import glCullFace cull-face)
(gl-import glLineWidth line-width)
(gl-import glPointSize point-size)

(gl-import- glPolygonMode gl-polygon-mode)
(gl-import- glLight set-light-array)
(gl-import- glLightf set-light)
(gl-import- glMaterial set-material-array)
(gl-import- glMaterialf set-material)
(gl-import- glFog set-fog-array)
(gl-import- glFogf set-fog)
(gl-import- glShadeModel shade-model)

(gl-import- glHint hint)
(gl-import- glBlendFunc blend-func)

(defn enable-high-quality-rendering []
  (hint :point-smooth-hint :nicest)
  (hint :line-smooth-hint :nicest)
  (hint :polygon-smooth-hint :nicest)
  (hint :fog-hint :nicest)
  (hint :perspective-correction-hint :nicest)
  (enable :point-smooth)
  (enable :line-smooth)
  (enable :polygon-smooth)
  (blend-func :src-alpha-saturate :one))

(defn color
  "Calls glColor.  Values are normalized between 0 and 1."
  ([r g b] (color-3 r g b))
  ([r g b a] (color-4 r g b a)))

(defn light
  "Sets values for light 'num'.  Example:
   (light 0
     :position [1 1 1 0])"
  [num & params]
  (let [light-num (enum (keyword (str "light" num)))]
    (doseq [[property value] (partition 2 params)]
      (let [property (enum property)]
        (if (sequential? value)
          (set-light-array light-num property (FloatBuffer/wrap (float-array (count value) value)))
          (set-light light-num property value))))))

(defn material
  "Sets material values for 'side'.  Example:
   (material :front-and-back
     :ambient-and-diffuse [1 0.25 0.25 1])"
  [side & params]
  (let [side (enum side)]
    (doseq [[property value] (partition 2 params)]
      (let [property (enum property)]
        (if (sequential? value)
          (set-material-array side property (FloatBuffer/wrap (float-array (count value) value)))
          (set-material side property value))))))

(defn fog
  "Sets values for fog.  Example:
    (fog
     :fog-start 0
     :fog-end 10
     :fog-color [0 0 0 0])"
  [& params]
  (doseq [[property value] (partition 2 params)]
    (if (sequential? value)
      (set-fog-array (enum property) (FloatBuffer/wrap (float-array (count value) (map #(or (enum %) %) value))))
      (set-fog (enum property) (if (keyword? value) (enum value) value)))))

(defn render-mode
  "Sets current render-mode.  Valid modes are [:solid :wireframe :point-cloud]."
  [mode]
  (condp = mode
    :solid (gl-polygon-mode :front-and-back :fill)
    :wireframe (gl-polygon-mode :front-and-back :line)
    :point-cloud (gl-polygon-mode :front-and-back :point)))

(defmacro with-render-mode
  "Sets render-mode within inner scope."
  [mode & body]
  `(let [mode# (get-integer :polygon-mode)]
     (render-mode ~mode)
     (try
      ~@body
      (finally
       (gl-polygon-mode :front-and-back mode#)))))

;;Shader

(defn create-literal-program
  "Translate a program without wrapping it in (defn main [] ...)"
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
  "Calls glUseProgram."
  [program]
  (gl-use-program (if (nil? program) 0 (:program program))))

(defmacro with-program
  "Binds program within inner-scope."
  [program & body]
  `(let [prev-program# *program*]
     (try
       (binding [*program* (:program ~program), *uniforms* (:uniforms ~program)]
         (bind-program ~program)
         ~@body)
       (finally
         (if (and prev-program# (not= prev-program# ~program))
           (bind-program prev-program#))))))

(defn- int? [p]
  (let [cls (class p)]
    (if (or (= cls Integer) (= cls Integer/TYPE)) true false)))

(defn get-uniform-location [variable]
  (if-let [location (@*uniforms* variable)]
    location
    (let [loc (gl-get-uniform-location *program* (.replace (name variable) \- \_))]
      (dosync (alter *uniforms* #(assoc % variable loc)))
      loc)))

(defn uniform [variable & args]
  (let [loc     (get-uniform-location variable)
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

;;Render Buffers

(gl-import- glGenRenderbuffersEXT gl-gen-render-buffers)
(gl-import- glBindRenderbufferEXT gl-bind-render-buffer)
(gl-import- glRenderbufferStorageEXT gl-render-buffer-storage)
(gl-import- glFramebufferRenderbufferEXT gl-frame-buffer-render-buffer)

(defn gen-render-buffer []
  (let [a (int-array 1)]
    (gl-gen-render-buffers (IntBuffer/wrap a))
    (first a)))

(defn bind-render-buffer [rb]
  (gl-bind-render-buffer :renderbuffer rb))

(defn attach-depth-buffer [dim]
  (let [depth-buffer (gen-render-buffer)
        dim (vec dim)]
    (bind-render-buffer depth-buffer)
    (gl-render-buffer-storage :renderbuffer :depth-component24 (dim 0) (dim 1))
    (gl-frame-buffer-render-buffer :framebuffer :depth-attachment :renderbuffer depth-buffer)))

;;Frame Buffers

(gl-import- glGenFramebuffersEXT gl-gen-frame-buffers)
(gl-import- glBindFramebufferEXT gl-bind-frame-buffer)
(gl-import- glCheckFramebufferStatusEXT gl-check-frame-buffer-status)
(gl-import- glDeleteFramebuffersEXT gl-delete-frame-buffers)
(gl-import- glFramebufferTexture2DEXT gl-frame-buffer-texture-2d)
(gl-import- glDrawBuffers gl-draw-buffers)
(gl-import- glDrawBuffer draw-buffer)
(gl-import- glReadBuffer gl-read-buffer)
(gl-import glReadPixels gl-read-pixels)
(gl-import glGetTexImage gl-get-tex-image)
(gl-import glActiveTexture gl-active-texture)

(defn gen-frame-buffer []
  (let [a (int-array 1)]
    (gl-gen-frame-buffers (IntBuffer/wrap a))
    (first a)))

(defn get-frame-buffer []
  (get-integer :framebuffer-binding))

(defn destroy-frame-buffer [fb]
  (let [a (int-array [fb])]
    (gl-delete-frame-buffers (IntBuffer/wrap a))))

(defn bind-frame-buffer [fb]
  (gl-bind-frame-buffer :framebuffer fb))

(defn frame-buffer-ok? []
  (= (gl-check-frame-buffer-status :framebuffer) (enum :framebuffer-complete)))

(defn frame-buffer-status []
  (enum-name (gl-check-frame-buffer-status :framebuffer)))

(defn-memo texture-lookup [point]
  (enum (keyword (str "texture" point))))

(defn attach [tex point]
  (let [p (attachment-lookup point)]
    (if (nil? tex)
      (gl-frame-buffer-texture-2d :framebuffer p :texture-rectangle 0 0)
      (do
        (gl-frame-buffer-texture-2d :framebuffer p (enum (:target tex)) (:id tex) 0)
        (attach! tex point)))))

(defn bind-read [variable tex point]
  (let [loc (get-uniform-location variable)]
    (gl-active-texture (texture-lookup point))
    (gl-bind-texture (enum (:target tex)) (:id tex))
    (uniform-1i loc point)))

(defn bind-write [start end]
  (let [buffers (int-array (map attachment-lookup (range start end)))]
    (gl-draw-buffers (IntBuffer/wrap buffers))))

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
       (bind-frame-buffer 0)
       (destroy-frame-buffer fb#)))))

;;Texture

(gl-import glTexEnvf tex-env)
(gl-import glTexParameteri tex-parameter)

(defn texture
  ([u] (gl-tex-coord-1 u))
  ([u v] (gl-tex-coord-2 u v))
  ([u v w] (gl-tex-coord-3 u v w)))

(defn bind-texture [t]
  (when t (gl-bind-texture (enum (:target t)) (:id t))))

(defmacro with-texture [t & body]
  `(do
     (bind-texture ~t)
     ~@body))

(defn destroy-texture [tex]
  (gl-delete-textures (IntBuffer/wrap (int-array (:id tex)))))

(defn create-byte-texture
  ([w h]
     (create-byte-texture :texture-2d w h))
  ([target w h]
     (create-texture target [w h] :rgba :rgba :unsigned-byte 4)))

(defn convert-texture [target tex]
  (let [target (enum target)
        [w h] (:dim tex)
        tex* (create-texture
              target
              [w h]
              (:internal-format tex)
              (:pixel-format tex)
              (:internal-type tex)
              (:tuple tex))]
    (bind-texture tex*)
    (gl-copy-tex-sub-image-2d target 0 0 0 0 0 w h)
    tex*))

(defn convert-texture! [target tex]
  (let [converted-texture (convert-texture target tex)]
    (release! tex)
    converted-texture))

'(defn load-texture-from-file
  ([filename subsample]
     (load-texture-from-file filename subsample :linear))
  ([filename subsample filter]
     (let [rgba (enum :rgba)
            data (TextureIO/newTextureData (File. filename) rgba rgba subsample "")
           tex  (TextureIO/newTexture data)]
       (texture-from-texture-io tex filter))))

'(defn load-texture-from-image
  ([image subsample]
     (load-texture-from-image image subsample :linear))
  ([image subsample filter]
     (let [rgba (enum :rgba)
           data (TextureIO/newTextureData image rgba rgba subsample "")
           tex  (TextureIO/newTexture data)]
       (texture-from-texture-io tex filter))))

(defn copy-to-texture
  [tex ary]
  (bind-texture tex)
  (let [target  (enum (:target tex))
        p-f     (enum (:pixel-format tex))
        i-t     (enum (:internal-type tex))
        dim     (vec (:dim tex))
        buf     (ByteBuffer/wrap ary)]
    (condp = (count (filter #(not= 1 %) dim))
      1 (gl-tex-sub-image-1d target 0 0 (dim 0) p-f i-t buf)
      2 (gl-tex-sub-image-2d target 0 0 0 (dim 0) (dim 1) p-f i-t buf)
      3 (gl-tex-sub-image-3d target 0 0 0 0 (dim 0) (dim 1) (dim 2) p-f i-t buf))))

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

(defn blit [tex]
  (when tex
    (let [[w h]
          (if (= :texture-rectangle (:target tex))
            (:dim tex)
            [1 1])]
      (push-matrix
        (with-texture tex
          (color 1 1 1)
          (with-projection (ortho-view 0 1 1 0 -1 1)
            (with-program nil
              (draw-quads
               (texture 0 0) (vertex 0 0 0)
               (texture w 0) (vertex 1 0 0)
               (texture w h) (vertex 1 1 0)
               (texture 0 h) (vertex 0 1 0)))))))))

(defn blit! [tex]
  (blit tex)
  (release! tex))

(defmacro render-to-texture
  "Renders a scene defined in the inner scope to 'tex'."
  [tex & body]
  `(let [[w# h#] (:dim ~tex)]
     (with-frame-buffer [w# h#]
       (with-viewport [0 0 w# h#]
         (attach-depth-buffer [w# h#])
         (attach-textures [] [~tex])
         (clear)
         (push-matrix
           ~@body)))))

;;;;;;;;;;;;;;;;;;;;;;


(defn write-to-screen
  "writes string at normalized coordinates (x,y)"
  [string x y]
  )