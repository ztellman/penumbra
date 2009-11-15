;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.geometry
  (:use [clojure.contrib.def :only (defmacro-)])
  (:use [penumbra.geometry])
  (:use [penumbra.opengl.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- prepend [text sym] (symbol (format "%s-%s" text (name sym))))

(defmacro gl-facade-import
  "Takes an OpenGL function and turns it into two macros:
    - a macro which behaves differently if we're inside a glBegin/End clause (glVertex3d -> vertex)
    - a macro which directly calls the OpenGL function (glVertex3d -> gl-vertex)"
  [import-from import-as]
  (let [facade-fn (prepend "facade" import-as)
        direct-fn (prepend "direct" import-as)]
    `(do
      (defmacro ~import-as [& a#]
        `(if *inside-begin-end*
          (~'~facade-fn ~@a#)
          (. *gl* ~'~import-from ~@a#)))
      (defmacro ~direct-fn [& b#]
        `(. *gl* ~'~import-from ~@b#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro facade-transform
  "Forwards the transformed vector from fn to the OpenGL function fn represents."
  [fn transform-fn]
  (let [facade-fn (prepend "facade" fn)
        direct-fn (prepend "direct" fn)]
    `(defn ~facade-fn [x# y# z#]
      (let [[xp# yp# zp# wp#]
            (if @*intra-primitive-transform*
              (apply-matrix (~transform-fn @*transform-matrix*) [x# y# z# 1])
              [x# y# z# 1])]
        (~direct-fn xp# yp# zp#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-transform
  [matrix transform-fn]
  (reset! *intra-primitive-transform* true)
  (swap! *transform-matrix* transform-fn matrix))

(defmacro facade-multiply
  "Applies a transform to *transform-matrix* rather than the OpenGL modelview matrix."
  ([fn matrix-fn] `(facade-multiply ~fn ~matrix-fn mult-matrix))
  ([fn matrix-fn transform-fn]
  (let [facade-fn (prepend "facade" fn)]
    `(defmacro ~facade-fn [& args#]
      `(apply-transform (~'~matrix-fn ~@args#) ~'~transform-fn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gl-import- glBegin gl-begin)
(gl-import- glEnd gl-end)

(defmacro defn-draw
  "Creates a macro called draw-'type' which redirects vertex and transform calls through appropriate facades."
  [primitive-type]
  `(defmacro ~(symbol (str "draw-" (name primitive-type))) [& body#]
    `(binding [*inside-begin-end* true
               *transform-matrix* (atom (identity-matrix))]
      (gl-begin ~'~(enum-macro primitive-type))
      ~@body#
      (gl-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;