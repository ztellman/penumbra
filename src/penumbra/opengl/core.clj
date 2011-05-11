;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.core
  (:use [clojure.contrib.def :only (defn-memo defmacro- defvar defvar-)])
  (:import (org.lwjgl.opengl GL11 GL12 GL13 GL14 GL15 GL20 GL30 GL31 GL32
                             ARBDrawBuffers
                             ARBFramebufferObject
                             ARBTextureFloat
                             ARBHalfFloatPixel
                             APPLEFloatPixels
                             ATITextureFloat
                             NVFloatBuffer
                             EXTTransformFeedback
                             ARBTextureRectangle
                             EXTTextureRectangle
                             EXTFramebufferObject
                             EXTGeometryShader4))
  (:import (org.lwjgl.util.glu GLU))
  (:import (java.lang.reflect Field Method))
  (:import (org.lwjgl BufferUtils))
  (:import (penumbra PenumbraSystem Natives)))

;;;
(Natives/extractNativeLibs (PenumbraSystem/getPlatform) "LWGL")


(defvar *primitive-type* nil
  "What type of primitive is being rendered?")

(defvar *check-errors* true
  "Causes errors in glGetError to throw an exception.  This creates minimal CPU overhead (~3%), and is almost always worth having enabled.")

(defvar *view* (atom [0 0 0 0])
  "Pixel boundaries of render window.  Parameters represent [x y width height].")

;;;

(defvar *program* nil
  "The current program bound by with-program")

(defvar *uniforms* nil
  "Cached integer locations for uniforms (bound on a per-program basis)")

(defvar *attributes* nil
  "Cached integer locations for attributes (bound on a per-program basis)")

;;;

(defvar *texture-pool* nil
  "A list of all allocated textures.  Unused textures can be overwritten, thus avoiding allocation.")

;;;

(defvar *renderer* nil)

(defvar *display-list* nil
  "Display list for framebuffer/blit rendering.")

(defvar *frame-buffer* nil
  "The currently bound frame buffer")

(defvar *read-format* nil
  "A function which returns the proper read format for a sequence type and tuple.")

(defvar *render-to-screen?* false
  "Whether the current renderer only targets the screen.")

(defvar *render-target* nil
  "The texture which is the main render target (GL_COLOR_ATTACHMENT0)")

(defvar *layered-target?* false
  "Is the render target a layered texture?")

(defvar *z-offset* nil
  "2-D slice of 3-D texture to render into.")

;;;

(defvar *font-cache* nil
  "Where all the fonts are kept")

(defvar *font* nil
  "Current font")

;;;

(defvar- containers [
                     APPLEFloatPixels
                     ARBDrawBuffers
                     ARBTextureFloat
                     ARBHalfFloatPixel
                     ARBFramebufferObject
                     EXTFramebufferObject
                     NVFloatBuffer
                     ATITextureFloat
                     EXTTextureRectangle
                     ARBTextureRectangle
                     EXTTransformFeedback
                     EXTGeometryShader4
                     GL20 GL15 GL14 GL13 GL12 GL11 GL30 GL31 GL32 GLU])

(defn- get-fields [#^Class static-class]
  (. static-class getFields))

(defn- get-methods [#^Class static-class]
  (. static-class getMethods))

(defn- contains-field? [#^Class static-class field]
  (first
   (filter
    #{ (name field) }
    (map #(.getName #^Field %) (get-fields static-class)))))

(defn- contains-method? [static-class method]
  (first
   (filter
    #{ (name method) }
    (map #(.getName #^Method %) (get-methods static-class)))))

(defn- field-container [field]
  (first (filter #(contains-field? % field) containers)))

(defn- method-container [method]
  (first (filter #(contains-method? % method) containers)))

(defn- get-gl-method [method]
  (let [method-name (name method)]
    (first (filter #(= method-name (.getName #^Method %)) (mapcat get-methods containers)))))

(defn-memo enum-name
  "Takes the numeric value of a gl constant (i.e. GL_LINEAR), and gives the name"
  [enum-value]
  (if (= 0 enum-value)
    "NONE"
    (.getName
     #^Field (some
              #(if (= enum-value (.get #^Field % nil)) % nil)
              (mapcat get-fields containers)))))     

(defn check-error
  ([]
     (check-error ""))
  ([name]
     (let [error (GL11/glGetError)]
       (if (not (zero? error))
         (throw (Exception. (str "OpenGL error: " name " " (enum-name error))))))))

(defn-memo enum [k]
  (when (keyword? k)
    (let [gl (str "GL_" (.. (name k) (replace \- \_) (toUpperCase)))
          sym (symbol gl)
          container (field-container sym)]
      (when (nil? container)
        (throw (Exception. (str "Cannot locate enumeration " k))))
      (eval `(. ~(field-container sym) ~sym)))))

(defn- get-parameters [method]
  (map
   #(keyword (.getCanonicalName #^Class %))
   (.getParameterTypes #^Method (get-gl-method method))))

(defn- get-doc-string [method]
  (str "Wrapper for " method "."))

(defmacro gl-import
  [import-from import-as]
  (let [method-name (str import-from)
        container (method-container import-from)]
    (when (nil? container)
      (throw (Exception. (str "Cannot locate method " import-from))))
    (let [doc-string (get-doc-string import-from)
          arg-list (vec (get-parameters import-from))
          doc-skip (if (contains? (meta import-as) :skip-wiki)
                     (:skip-wiki (meta import-as))
                     true)]
      `(defmacro ~import-as
         ~doc-string
         {:skip-wiki ~doc-skip
          :arglists (list ~arg-list)}
         [& args#]
         `(do
            (let [~'value# (. ~'~container ~'~import-from ~@(map (fn [x#] (or (enum x#) x#)) args#))]
              (when (and *check-errors* (not *primitive-type*))
                (check-error ~'~method-name))
              ~'value#))))))

(defmacro gl-import-
  "Private version of gl-import"
  [import-from import-as]
  (list `gl-import import-from (with-meta import-as (assoc (meta import-as) :private true))))

(defmacro gl-import+
  "Documented version of gl-import"
  [import-from import-as]
  (list `gl-import import-from (with-meta import-as (assoc (meta import-as) :skip-wiki nil))))

;;;

(gl-import- glGetInteger gl-get-integer)

(defn get-integer
  "Calls glGetInteger."
  [param]
  (let [buf (BufferUtils/createIntBuffer 16)]
    (gl-get-integer (enum param) buf)
    (.get buf 0)))
