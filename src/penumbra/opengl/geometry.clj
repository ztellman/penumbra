;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.geometry
  (:use [clojure.contrib.def :only (defmacro-)])
  (:use [penumbra.opengl.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn identity-matrix []
  [1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 0 1])

(defn translation-matrix [x y z]
  [1 0 0 x
   0 1 0 y
   0 0 1 z
   0 0 0 1])                                              

(defn scaling-matrix [x y z]
  [x 0 0 0
   0 y 0 0
   0 0 z 0
   0 0 0 1])

(defn rotation-matrix [theta x y z]
  (let [s (Math/sin (* Math/PI (/ theta 180)))
        c (Math/cos (* Math/PI (/ theta 180)))
        t (- 1 c)]
  [(+ c (* t x x))        (- (* t x y) (* s z))   (+ (* t x z) (* s y))   0
   (+ (* t x y) (* s z))  (+ (* t y y) c)         (- (* t y z) (* s x))   0
   (- (* t x z) (* s y))  (+ (* t y z) (* s x))   (+ (* t z z) c)         0
   0                      0                       0                       1]))

(defn index [m i j] (m (+ i (* j 4))))

(defn mult-matrix [a b]
  (let [indices (for [i (range 4) j (range 4)] [i j])
        traverse (fn [[i j]] (apply + (map #(* (index a % i) (index b j %)) (range 4))))]
    (vec (map traverse indices))))

(defn apply-matrix [m v]
  (let [traverse-fn (fn [i] #(* (v %) (index m % i)))]
    (map #(apply + (map (traverse-fn %) (range 4))) (range 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- prepend [text sym] (symbol (format "%s-%s" text (name sym))))

(defmacro gl-facade-import
  "Takes an OpenGL function and turns it into two macros:
    - a macro which behaves differently if we're inside a glBegin/End clause (glVertex3d -> vertex)
    - a macro which directly calls the OpenGL function (glVertex3d -> gl-vertex)"
  [import-from import-as]
  (let [facade-fn (prepend "facade" import-as)
        direct-fn (prepend "gl" import-as)]
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
        direct-fn (prepend "gl" fn)]
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
