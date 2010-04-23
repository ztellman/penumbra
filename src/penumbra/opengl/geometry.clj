;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.geometry
  (:use [clojure.contrib.def :only (defmacro- defvar)]
        [penumbra.geometry]
        [penumbra.opengl.core])
  (:require [penumbra.opengl.effects :as fx])
  (:import [penumbra.geometry Vec2 Vec3]
           [org.lwjgl BufferUtils]))

;;;

(defprotocol Renderer
  (vertex- [r x y z])
  (texture- [r u] [r u v] [r u v w])
  (normal- [r x y z])
  (attribute- [r attrib values])
  (color- [rnd r g b a])
  (translate- [r x y z])
  (scale- [r x y z])
  (rotate- [r angle x y z])
  (load-identity- [r])
  (transform-matrix- [r])
  (with-transform- [r f]))

(defn vertex
  ([v]
     (cond
      (instance? Vec2 v) (vertex (.x #^Vec2 v) (.y #^Vec2 v))
      (instance? Vec3 v) (vertex (.x #^Vec3 v) (.y #^Vec3 v) (.z #^Vec3 v))
      :else (apply vertex v)))
  ([x y] (vertex x y 0))
  ([x y z] (vertex- *renderer* x y z))
  ([x y z w] (vertex x y z)))

(defn texture
  ([v] (cond
        (instance? Vec2 v) (texture (.x #^Vec2 v) (.y #^Vec2 v))
        (instance? Vec3 v) (texture (.x #^Vec3 v) (.y #^Vec3 v) (.z #^Vec3 v))
        (number? v) (texture- *renderer* v)
        :else (apply texture v)))
  ([u v] (texture- *renderer* u v))
  ([u v w] (texture- *renderer* u v w)))

(defn normal
  ([v] (cond
        (instance? Vec2 v) (normal (.x #^Vec2 v) (.y #^Vec2 v))
        (instance? Vec3 v) (normal (.x #^Vec3 v) (.y #^Vec3 v) (.z #^Vec3 v))
        :else (apply normal v)))
  ([x y z] (normal- *renderer* x y z)))

(defn translate
  ([v] (cond
        (instance? Vec2 v) (translate (.x #^Vec2 v) (.y #^Vec2 v))
        (instance? Vec3 v) (translate (.x #^Vec3 v) (.y #^Vec3 v) (.z #^Vec3 v))
        :else (apply translate v)))
  ([x y] (translate x y 0))
  ([x y z] (translate- *renderer* x y z)))

(defn scale
  ([v] (cond
        (instance? Vec2 v) (scale (.x #^Vec2 v) (.y #^Vec2 v))
        (instance? Vec3 v) (scale (.x #^Vec3 v) (.y #^Vec3 v) (.z #^Vec3 v))
        :else (apply scale v)))
  ([x y] (scale x y 1))
  ([x y z] (scale- *renderer* x y z)))

(defn color
  ([c] (cond
        (instance? Vec2 c) (color (.x #^Vec2 c) (.y #^Vec2 c))
        (instance? Vec3 c) (color (.x #^Vec3 c) (.y #^Vec3 c) (.z #^Vec3 c))
        :else (apply color c)))
  ([r g b] (color r g b 1))
  ([r g b a] (color- *renderer* r g b a)))

(defn attribute [attrib & values]
  (attribute- *renderer* attrib values))

(defn rotate [angle x y z]
  (rotate- *renderer* angle x y z))

(defn load-identity []
  (load-identity- *renderer*))

;;;

(gl-import- glVertex3d gl-vertex)
(gl-import- glNormal3d gl-normal)
(gl-import- glTexCoord1d gl-tex-1)
(gl-import- glTexCoord2d gl-tex-2)
(gl-import- glTexCoord3d gl-tex-3)
(gl-import- glRotatef gl-rotate)
(gl-import- glTranslated gl-translate)
(gl-import- glScaled gl-scale)
(gl-import- glPushMatrix gl-push-matrix)
(gl-import- glPopMatrix gl-pop-matrix)
(gl-import- glLoadIdentity gl-load-identity)

(gl-import- glVertexAttrib1f attribute-1f)
(gl-import- glVertexAttrib2f attribute-2f)
(gl-import- glVertexAttrib3f attribute-3f)
(gl-import- glVertexAttrib4f attribute-4f)

(gl-import glGetAttribLocation gl-get-attrib-location)

(defn- attribute-location [variable]
  (if-let [location (@*attributes* variable)]
    location
    (let [ary (.getBytes (str (.replace (name variable) \- \_) "\0"))
          attribute-buf (-> (BufferUtils/createByteBuffer (count ary)) (.put ary) .rewind)
          loc (gl-get-attrib-location (:program *program*) attribute-buf)]
      (println "attribute-location" variable loc)
      (dosync (alter *attributes* #(assoc % variable loc)))
      loc)))

(defn- set-attrib [variable args]
  (let [loc     (attribute-location variable)
        args    (vec (map float args))]
    (condp = (count args)
      1 (attribute-1f loc (args 0))
      2 (attribute-2f loc (args 0) (args 1))
      3 (attribute-3f loc (args 0) (args 1) (args 2))
      4 (attribute-4f loc (args 0) (args 1) (args 2) (args 3)))))

(def basic-renderer
  (reify 
    Renderer
    (vertex- [_ x y z] (gl-vertex x y z))
    (texture- [_ u] (gl-tex-1 u))
    (texture- [_ u v] (gl-tex-2 u v))
    (texture- [_ u v w] (gl-tex-3 u v w))
    (color- [_ r g b a] (fx/color r g b a))
    (attribute- [_ attrib values] (set-attrib attrib values))
    (normal- [_ x y z] (gl-normal x y z))
    (scale- [_ x y z] (gl-scale x y z))
    (translate- [_ x y z] (gl-translate x y z))
    (rotate- [_ angle x y z] (gl-rotate angle x y z))
    (load-identity- [_] (gl-load-identity))
    (transform-matrix- [_] nil)
    (with-transform- [_ f]
      (gl-push-matrix)
      (try
       (f)
       (finally
        (gl-pop-matrix))))))

;;;

(defvar *outer-renderer* nil)

(defvar *intra-primitive-transform* false
  "Have we encountered an intra-primitive (i.e. *inside-begin-end* is true) transformation")

(defvar *transform-matrix* nil
  "The transform matrix for intra-primtive transforms")

(def intra-primitive-renderer
  (reify
    Renderer
    (vertex- [_ x y z]
      (if (and *intra-primitive-transform* @*intra-primitive-transform*)
        (let [v (transform @*transform-matrix* (vec3 x y z))]
          (vertex- *outer-renderer* (.x #^Vec3 v) (.y #^Vec3 v) (.z #^Vec3 v)))
        (vertex- *outer-renderer* x y z)))
    (normal- [_ x y z]
      (if (and *intra-primitive-transform* @*intra-primitive-transform*)
        (let [v (transform (normal-matrix @*transform-matrix*) (vec3 x y z))]
          (normal- *outer-renderer* (.x #^Vec3 v) (.y #^Vec3 v) (.z #^Vec3 v)))
        (normal- *outer-renderer* x y z)))
    (texture- [_ u]
      (texture- *outer-renderer* u))
    (texture- [_ u v]
      (texture- *outer-renderer* u v))
    (texture- [_ u v w]
      (texture- *outer-renderer* u v w))
    (color- [_ r g b a]
      (color- *outer-renderer* r g b a))
    (attribute- [_ attrib values]
      (attribute- *outer-renderer* attrib values))
    (scale- [_ x y z]
      (dosync
       (alter *transform-matrix* #(transform-matrix % (scaling-matrix x y z)))
       (ref-set *intra-primitive-transform* true)))
    (translate- [_ x y z]
      (dosync
       (alter *transform-matrix* #(transform-matrix % (translation-matrix x y z)))
       (ref-set *intra-primitive-transform* true)))
    (rotate- [_ angle x y z]
      (dosync
       (alter *transform-matrix* #(transform-matrix % (rotation-matrix angle x y z)))
       (ref-set *intra-primitive-transform* true)))
    (load-identity- [_]
      (dosync
       (ref-set *transform-matrix* (identity-matrix))
       (ref-set *intra-primitive-transform* false)))
    (transform-matrix- [_] @*transform-matrix*)
    (with-transform- [_ f]
      (binding [*transform-matrix* (ref @*transform-matrix*)
                *intra-primitive-transform* (ref @*intra-primitive-transform*)]
        (f)))))

;;;

(gl-import glBegin gl-begin)
(gl-import glEnd gl-end)

(defmacro defn-draw
  "Creates a macro called draw-'type' which redirects vertex and transform calls through appropriate facades."
  [primitive-type]
  (let [doc-string (str "Wraps body in glBegin(GL_" (.toUpperCase (name primitive-type)) ") ... glEnd().\n  "
                        "Transform calls (rotate, translate, etc.) are allowed within this scope, but will force an intermediate transform step.")]
    `(defmacro ~(symbol (str "draw-" (name primitive-type)))
       ~doc-string
      [& body#]
      `(binding [*primitive-type* ~'~primitive-type
                 *transform-matrix* (ref (identity-matrix))
                 *intra-primitive-transform* (ref false)
                 *outer-renderer* *renderer*
                 *renderer* intra-primitive-renderer
                 ]
         (gl-begin ~'~(enum primitive-type))
         ~@body#
         (gl-end)))))

;;;