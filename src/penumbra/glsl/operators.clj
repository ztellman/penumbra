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
  (:use [penumbra.translate.core :only (tree-map realize seq-wrap)])
  (:use [clojure.set :only (map-invert)])
  (:use [clojure.contrib.seq-utils :only (indexed flatten)])
  (:use [clojure.contrib.def :only (defvar-)])
  (:use [clojure.contrib.pprint])
  (:require [penumbra.glsl.core :as glsl])
  (:require [penumbra.glsl.data :as data])
  (:require [clojure.zip :as zip]))

;;;;;;;;;;;;;;;;;;

(defvar- types (set '(color3 color4 float float2 float3 float4 int int2 int3 int4)))

(defn- typecast-expr [type expr]
  (condp = type
    'float    (list 'float4 expr)
    'float2   (list 'float4 expr 1.0 1.0)
    'float3   (list 'float4 expr 1.0)
    'float4   expr
    'color3   (list 'float4 expr 1.0)
    'color4   expr
    'int      (list 'float4 expr)
    'int2     (list 'float4 expr 1.0 1.0)
    'int3     (list 'float4 expr 1.0)
    'int4     expr))

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

(def fixed-transform
  '((set! --coord (-> :multi-tex-coord0 .xy (* --dim)))
    (set! :position (* :model-view-projection-matrix :vertex))))

(defn- result?
  "This assumes you only traverse down the last element of the tree"
  [expr]
  (or
    (vector? expr)
    (not (sequential? expr))
    (and
      (-> expr first seq? not)
      (-> expr glsl/transformer first (not= 'do)))))

(defn- results [expr]
  (if (result? expr)
    (if (vector? expr) expr (list expr))
    (results (last expr))))

(defn- transform-results [expr fun]
  (loop [z (zip/seq-zip expr)]
    (if (result? (zip/node z))
      (zip/root (zip/replace z (fun (results expr))))
      (recur (-> z zip/down zip/rightmost)))))

(defn- transform-operator-results
  "Tranforms the final expression into one or more assignments to gl_FragData[n]"
  [expr]
  (transform-results
    expr
    (fn [results]
      (list* 'do
        (realize
          (map
            (fn [[idx e]]
              (list 'set!
                (list '-> :frag-data (list 'nth idx))
                (typecast-expr (or (typeof e) (first e)) e)))
            (indexed results)))))))

(defn create-operator
  ([source] (create-operator '() source))
  ([uniforms body]
    (create-program
      "#extension GL_ARB_texture_rectangle : enable"
      (concat
        '((varying vec2 --coord)
          (uniform vec2 --dim))
        (realize (map #(cons 'uniform %) uniforms)))
      fixed-transform
      (transform-operator-results body))))

;;;;;;;;;;;;;;;;;;;

(defn- prepend-index [expr]
  (println "prepend-index" expr)
  (let [index
        '((set!
          (float --index)
          (-> --coord .y floor (* (.x --dim)) (+ (-> --coord .x floor)))))]
    (if (contains? (set (flatten expr)) :index)
      (concat
        index
        (apply-transforms expr [(replace-with :index '--index)]))
      expr)))

(defn- validate-elements
  "Make sure that there is one and only one type for each element"
  [expr]
  (let [elements (filter-symbols element? expr)]
    (doseq [e (distinct (filter typeof elements))]
      (let [typs (distinct (map typeof (filter #(= e %) elements)))]
        (if (empty? typs)
          (throw (Exception. (str e " needs to have an explicitly defined type."))))
        (if (< 1 (count typs))
          (throw (Exception. (str "Ambiguous type for " e ", can't choose between " typs))))))))

(defn- validate-params
  "Make sure there isn't more than one type for each parameter"
  [expr]
  (let [params (filter-symbols #(and (not (element? %)) (typeof %)) expr)]
    (doseq [e (distinct (filter typeof params))]
      (let [typs (distinct (map typeof (filter #(= e %) params)))]
        (if (< 1 (count typs))
          (throw (Exception. (str "Ambiguous type for " e ", can't choose between " typs))))))))

(defn- validate-results
  "Make sure there is a type for each return value"
  [expr]
  (doseq [x (results expr)]
    (if (and (nil? (typeof x)) (or (not (sequential? x)) (not (contains? types (first x)))))
      (throw (Exception. (str x " must have an explicit type."))))))

(defn- rename-element [i]
  (symbol (str "-tex" i)))

(defn- transform-element [e]
  (let [[_ tuple] (type-map (typeof e))]
    (with-meta
      (list (swizzle tuple)
        (list 'texture2DRect (rename-element (element-index e)) '--coord))
      ^e)))

(defn- rename-param [p]
  p)

(defn- process-map
  "Transforms the body, and pulls out all the relevant information."
  [expr]
  (validate-elements expr)
  (validate-params expr)
  (validate-results expr)
  (let [expr        (if (seq? (first expr)) expr (list expr))
        elements    (distinct (filter-symbols #(and (element? %) (typeof %)) expr))
        params      (distinct (filter-symbols #(and (not (element? %)) (typeof %)) (transform-results expr (fn [x] '()))))
        transforms  (flatten
                      (list
                        (replace-with :coord '--coord)
                        (map #(replace-with % (transform-element %)) elements)
                        (map #(replace-with % (rename-param %)) params)))
        expr        (prepend-index (apply-transforms expr transforms))
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
     :results (map #(or (typeof %) (first %)) (results expr))}))

(defn- create-write-texture [typecast dim]
  (let [[type tuple]  (type-map typecast)
        i-f           (write-format type tuple)
        p-f           (pixel-format tuple)]
    (if (nil? i-f) (throw (Exception. (str "Cannot write to texture of type " typecast))))
    (create-texture :texture-rectangle dim (first i-f) p-f type tuple)))

(defn create-map [expr]
  (let [info    (process-map expr)
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
                          (map (fn [x] [x dim]) (:results info)))]
            (doseq [[n v] params]
              (apply uniform (list*
                               (keyword (name n))
                               (seq-wrap v))))
            (apply uniform (list* :__dim (map float dim)))
            (attach-textures
              (interleave (map rename-element (range (count elements))) elements)
              targets)
            (apply draw dim)
            (doseq [e (distinct elements)]
              (release! e))
            (if (= 1 (count targets)) (first targets) targets)))))))

;;;;;;;;;;;;;;;;;;

(defn process-reduce [expr]
  (let [expr          (if (= 1 (count expr)) (first expr) expr)
        result        (first (results expr))
        apply-reduce  (fn [offset]
                        (apply-transforms expr
                          [(replace-with '%1 'a)
                           (replace-with '%2
                             (list
                               (swizzle (second (type-map (typeof result))))
                               (list 'texture2DRect '--data
                                 (list '+ 'coord
                                   (list 'float2 (first offset) (second offset))))))]))]
    {:type
      (typeof result)
     :declarations
      '((sampler2DRect --data)
        (vec2 --bounds))
     :body
      (list
        'let
          (vector
            '(float2 coord) '(* (floor --coord) 2.0)
            '(bool x) '(<= (.x --bounds) (.x coord))
            '(bool y) '(<= (.y --bounds) (.y coord))
            (list (typeof result) 'a) (list (swizzle (second (type-map (typeof result)))) '(texture2DRect --data coord))
            'a (list 'if 'x 'a (apply-reduce [1 0]))
            'a (list 'if 'y 'a (apply-reduce [0 1]))
            'a (list 'if '(or x y) 'a (apply-reduce [1 1])))
        (list (typeof result) 'a))}))

(defn create-reduce [expr]
  (let [info (process-reduce expr)
        program (create-operator (:declarations info) (:body info))]
    (fn [input*]
      (let [dim* (:dim input*)]
        (loop [dim dim*, input input*]
          (if (= [1 1] dim)
            (do
              (let [result (data/unwrap-first input)]
                (release! input)
                (seq result)))
            (let [half-dim  (map #(Math/ceil (/ % 2.0)) dim)
                  target    (data/mimic-texture input half-dim)
                  [w h]     half-dim
                  bounds    (map #(* 2 (Math/floor (/ % 2.0))) dim)]
              (with-program program
                (apply uniform (list* :__bounds bounds))
                (apply uniform (list* :__dim half-dim))
                (attach-textures [:__data input] [target])
                (draw 0 0 w h)
                (release! input)
                (recur half-dim target)))))))))






