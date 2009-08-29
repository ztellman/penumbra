;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.core
  (:use [penumbra.translate.core])
  (:use [penumbra.translate.c :only (c-transformer c-generator c-parser)])
  (:use [clojure.contrib.def :only (defvar-)])
  (:use [clojure.contrib.pprint]))

;;;

(defn- parse-keyword
  "Turns :model-view-matrix into gl_ModelViewMatrix."
  [k]
  (symbol
    (str
      "gl_"
      (apply str
        (map
          #(str (.. % (substring 0 1) toUpperCase) (. % substring 1 (count %)))
          (seq (.split (name k) "-")))))))

(def type-map
  (apply hash-map
    '(float float
      float2 vec2
      float3 vec3
      float4 vec4
      int int
      int2 ivec2
      int3 ivec3
      int4 ivec4
      color3 float3
      color4 float4)))
;;;

(defmulti glsl-transformer
  (fn [_] nil)
  :default :none)

(defmulti glsl-generator
  #(if (seq? %) (first %) nil)
  :default :none)

(defmulti glsl-parser
  #(if (seq? %) (first %) nil)
  :default :none)

(defmethod glsl-transformer :none [expr]
  (cond
    (contains? type-map expr) (expr type-map)
    :else                     (c-transformer expr)))

(defmethod glsl-generator :none [expr]
 (c-generator expr))

(defmethod glsl-parser :none [expr]
  (cond
    (keyword? expr) (parse-keyword expr)
    :else           (c-parser expr)))

(defn translate-declarations [decl]
  (if (empty? decl)
    ""
    (parse-lines (map #(list 'declare %) decl) ";")))

(defn translate-shader
  ([exprs] (translate-shader '() exprs))
  ([decl exprs]
    (binding [*transformer* glsl-transformer, *generator* glsl-generator, *parser* #(try-parse % glsl-parser)]
      (let [exprs (tree-map exprs #(if (keyword? %) (parse-keyword %) %))]
      (str
        (translate-declarations decl)
        (translate-expr
          (concat
            '(defn void main [])
            (list 'do exprs))))))))