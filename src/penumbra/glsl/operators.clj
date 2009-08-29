;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.operators
  (:use [penumbra opengl slate])
  (:use [penumbra.opengl.texture :only (create-texture release!)])
  (:use [penumbra.opengl.framebuffer :only (pixel-format write-format)])
  (:use [penumbra.translate.core :only (tree-map)])
  (:use [clojure.set :only (map-invert)])
  (:use [clojure.contrib.seq-utils :only (indexed flatten)])
  (:use [clojure.contrib.def :only (defvar-)])
  (:require [penumbra.glsl.core :as glsl])
  (:use [clojure.contrib.pprint]))

;;;;;;;;;;;;;;;;;;

(def fixed-transform
  '((set! --coord (-> :multi-tex-coord0 .xy))
    (set! :position (* :model-view-projection-matrix :vertex))))

(defn- separate-operator [expr]
  (cond
    (vector? expr)            ['() expr]
    (vector? (last expr))     [(take (dec (count expr)) expr) (last expr)]
    (-> expr first seq? not)  ['() (list expr)]
    :else                     [(take (dec (count expr)) expr) (list (last expr))]))

(defn- transform-return-expr
  "Tranforms the final expression into one or more assignments to gl_FragData[n]"
  [expr]
  (let [[body assn] (separate-operator expr)
        assn        (list* 'do
                      (map
                        (fn [[idx e]] (list 'set! (list '-> :frag-data (list 'nth idx)) e))
                        (indexed assn)))]
    (concat body (list assn))))

(defn create-operator
  ([source] (create-operator '() source))
  ([uniforms body]
    (create-program
      "#extension GL_ARB_texture_rectangle : enable"
      (concat
        '((varying vec2 --coord)
          (uniform vec2 --dim))
        (map #(cons 'uniform %) uniforms))
      fixed-transform
      (transform-return-expr body))))

;;;;;;;;;;;;;;;;;;

(defvar- types (set '(color3 color4 float float2 float3 float4 int int2 int3 int4)))

(defvar- type-map
  (apply hash-map
    '(color3  [:unsigned-byte 3]
      color4  [:unsigned-byte 4]
      float   [:float 1]
      float2  [:float 2]
      float3  [:float 3]
      float4  [:float 4]
      int     [:int 1]
      int2    [:int 2]
      int3    [:int 3]
      int4    [:int 4])))

(defvar- swizzle { 1 '.x, 2 '.xy, 3 '.xyz, 4 '.xyzw })

(defn- apply-transforms [tree funs]
  (reduce #(tree-map %1 %2) tree funs))

(defn- typeof [x]
  (:tag ^x))

(defn- filter-symbols [predicate tree]
  (let [leaves (filter #(not (seq? %)) (flatten tree))]
    (distinct (filter predicate leaves))))

(defn- element? [s]
  (and (symbol? s) (.startsWith (name s) "%")))

(defn- element-index [param]
  (if (= '% param)
    0
    (dec (Integer/parseInt (.substring (name param) 1)))))

(defn- replace-with [from to]
  #(if (= from %) to %))

;;;;;;;;;;;;;;;;;;;;;

(defn- prepend-index [expr]
  (let [index '((set! (float --index) (-> --coord .y (* (.x --dim)) (+ (.x --coord)))))]
    (if (contains? (set (flatten expr)) :index)
      (concat
        index
        (apply-transforms expr [(replace-with :index '--index)]))
      expr)))

(defn- validate-elements [expr]
  (let [elements (filter-symbols element? expr)]
    (doseq [e (distinct (filter typeof elements))]
      (let [typs (distinct (map typeof (filter #(= e %) elements)))]
        (if (empty? typs)
          (throw (Exception. (str e " needs to have an explicitly defined type."))))
        (if (< 1 (count typs))
          (throw (Exception. (str "Ambiguous type for " e ", can't choose between " typs))))))))

(defn- validate-params [expr]
  (let [params (filter-symbols #(and (not (element? %)) (typeof %)) expr)]
    (doseq [e (distinct (filter typeof params))]
      (let [typs (distinct (map typeof (filter #(= e %) params)))]
        (if (< 1 (count typs))
          (throw (Exception. (str "Ambiguous type for " e ", can't choose between " typs))))))))

(defn- validate-results [expr]
  (let [[_ result] (separate-operator expr)]
    (doseq [x result]
      (if (and (nil? (typeof x)) (or (not (sequential? x)) (not (contains? types (first x)))))
        (throw (Exception. (str x " must have an explicit type.")))))))

(defn- rename-element [i]
  (symbol (str "-tex" i)))

(defn- transform-element [e]
  (let [[_ tuple] (type-map (typeof e))]
    (list (swizzle tuple)
      (list 'texture2DRect (rename-element (element-index e)) '--coord))))

(defn- rename-param [p]
  (symbol (str "-" (name p))))

(defn- process-gmap [expr]
  (validate-elements expr)
  (validate-params expr)
  (validate-results expr)
  (let [expr        expr ;(if (seq? (first expr)) expr (list expr))
        elements    (distinct (filter-symbols #(and (element? %) (typeof %)) expr))
        params      (distinct (filter-symbols #(and (not (element? %)) (typeof %)) expr))
        transforms  (flatten
                      (list
                        prepend-index
                        (replace-with :coord '--coord)
                        (map #(replace-with % (transform-element %)) elements)
                        (map #(replace-with % (rename-param %)) params)))
        expr        (apply-transforms expr transforms)
        decl        (concat
                      (if (empty? elements)
                        '()
                        (map
                          #(list 'sampler2DRect (rename-element (element-index %)))
                          elements))
                      (map
                        #(list (glsl/type-map (typeof %)) (rename-param %))
                        params))]
    {:declarations decl
     :body expr
     :elements (zipmap elements (map typeof elements))
     :params (zipmap params (map typeof params))
     :result (map #(or (typeof %) (first %)) (second (separate-operator expr)))}))

(defn- create-write-texture [typecast dim]
  (let [[type tuple]  (type-map typecast)
        i-f           (write-format type tuple)
        p-f           (pixel-format tuple)]
    (if (nil? i-f) (throw (Exception. (str "Cannot write to texture of type " typecast))))
    (create-texture :texture-rectangle dim (first i-f) p-f type tuple)))

(defn create-gmap [expr]
  (let [info    (process-gmap expr)
        program (create-operator (:declarations info) (:body info))]
    (fn this
      ([size] (this {} [] (rectangle size)))
      ([params elements-or-size]
        (if (number? elements-or-size)
          (this params [] (rectangle elements-or-size))
          (this params elements-or-size (:dim (first elements-or-size)))))
      ([params elements dim]
        ;Check number of elements
        (if (not= (count elements) (count (:elements info)))
          (throw (Exception. (str "Expected " (count (:elements info)) " elements, was given " (count elements) "."))))
        ;Check dimensions of elements
        (if (and
              (not (empty? elements))
              (apply not= (list* dim (map :dim elements))))
          (throw (Exception. (str "All data must be of the same dimension.  Given dimensions are " (map :dim elements)))))
        (with-program program
          (let [targets (map
                          (fn [[typ dim]] (create-write-texture typ dim))
                          (map (fn [x] [x dim]) (:result info)))]
            (doseq [[n v] params]
              (apply uniform (list*
                               (keyword (.replace (str "-" (name n)) \- \_))
                               (if (sequential? v) v (list v)))))
            (uniform :__dim (float (first dim)) (float (second dim)))
            (attach-textures
              (interleave (map rename-element (range (count elements))) elements)
              targets)
            (draw)
            (doseq [e elements]
              (release! e))
            (if (= 1 (count targets)) (first targets) targets)))))))





