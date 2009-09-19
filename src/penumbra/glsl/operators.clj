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
  (:use [penumbra.glsl core data])
  (:use [penumbra.translate.core])
  (:use [clojure.set :only (difference)])
  (:use [clojure.contrib (seq-utils :only (separate indexed flatten)) (def :only (defvar-)) pprint])
  (:require [clojure.zip :as zip]))

;;;;;;;;;;;;;;;;;;

(defvar- types (set '(color3 color4 float float2 float3 float4 int int2 int3 int4)))

(defn- typecast [expr]
  (condp = (:tag ^expr)
    'float   (list 'float4 expr)
    'float2  (list 'float4 expr 1.0 1.0)
    'float3  (list 'float4 expr 1.0)
    'float4  expr
    'color   (list 'float4 expr)
    'color2  (list 'float4 expr 1.0 1.0)
    'color3  (list 'float4 expr 1.0)
    'color4  expr
    'int     (list 'float4 expr)
    'int2    (list 'float4 expr 1 1)
    'int3    (list 'float4 expr 1)
    'int4    expr))

(defvar- type-tuple
  (apply hash-map
   '(color 1, color2 2, color3 3, color4 4
     float 1, float2 2, float3 3, float4 4
     int 1, int2 2, int3 3, int4 4)))

(defvar- type-format
  (apply hash-map
    '(color1 :unsigned-byte
      color2 :unsigned-byte
      color3 :unsigned-byte
      color4 :unsigned-byte
      float :float       
      float2 :float
      float3 :float
      float4 :float
      int :int
      int2 :int
      int3 :int
      int4 :int)))

(defvar- swizzle { 1 '.x, 2 '.xy, 3 '.xyz, 4 '.xyzw })

(defn- apply-transforms [tree funs]
  (reduce #(tree-map %1 %2) tree funs))

(defn- element? [s]
  (and (symbol? s) (.startsWith (name s) "%")))

(defn- element-index [param]
  (if (= '% param)
    0
    (dec (Integer/parseInt (.substring (name param) 1)))))

(defn- replace-with [from to]
  #(if (= from %) to))

;;;;;;;;;;;;;;;;;;;;;

(def fixed-transform
  '((<- --coord (-> :multi-tex-coord0 .xy (* --dim)))
    (<- :position (* :model-view-projection-matrix :vertex))))

(defn- prepend-index [expr]
  (let [index
        '((<-
          --index
          (-> --coord .y floor (* (.x --dim)) (+ (-> --coord .x floor)))))]
    (if (contains? (set (flatten expr)) :index)
      (concat
        index
        (apply-transforms expr [(replace-with :index '--index)]))
      expr)))

(defn wrap-and-prepend [expr]
  (list
   '(do
      (declare (varying #^float2 --coord))
      (declare (uniform #^float2 --dim)))
   (list 'defn 'void 'main [] (prepend-index expr))))             

(defn- result?
  "This assumes you only traverse down the last element of the tree"
  [expr]
  (or
    (vector? expr)
    (not (sequential? expr))
    (and
      (-> expr first sequential? not)
      (-> expr transformer first (not= 'do)))))

(defn- results [expr]
  (if (result? expr)
    (if (vector? expr) expr (list expr))
    (results (last expr))))

(defn- transform-results [expr fun]
  (loop [z (zip/seq-zip expr)]
    (if (result? (zip/node z))
      (zip/root (zip/replace z (fun (results expr))))
      (recur (-> z zip/down zip/rightmost)))))

(defn create-operator
  ([body]
     (create-program*
      "#extension GL_ARB_texture_rectangle : enable"
      (wrap-and-prepend fixed-transform)
      body)))

(defn set-params [params]
  (doseq [[n v] params]
    (apply
      uniform
      (list*
        (keyword (name n))
        (seq-wrap v)))))

;;;;;;;;;;;;;;;;;;;

(defn- validate-elements
  "Make sure that there is one and only one type for each element"
  [expr]
  (let [elements (tree-filter expr element?)]
    (doseq [e (distinct (filter typeof elements))]
      (let [typs (distinct (map typeof (filter #(= e %) elements)))]
        (if (empty? typs)
          (throw (Exception. (str e " needs to have an explicitly defined type."))))
        (if (< 1 (count typs))
          (throw (Exception. (str "Ambiguous type for " e ", can't choose between " typs))))))))

(defn- validate-params
  "Make sure there isn't more than one type for each parameter"
  [expr]
  (let [params (tree-filter expr #(and (not (element? %)) (typeof %)))]
    (doseq [e (distinct (filter typeof params))]
      (let [typs (distinct (map typeof (filter #(= e %) params)))]
        (if (< 1 (count typs))
          (throw (Exception. (str "Ambiguous type for " e ", can't choose between " typs))))))))


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
              (list '<-
                (list '-> :frag-data (list 'nth idx))
                (add-meta e :result true)))
            (indexed results)))))))

(defn typecast-results
  [expr]
  (tree-map
   expr
   (fn [x]
     (if (:result ^x)
       (add-meta (typecast (add-meta x :result false)) :result false)
       x))))

(defn- validate-results
  "Make sure there is a type for each return value"
  [expr]
  (doseq [x (results expr)]
    (if (and (nil? (typeof x)) (or (not (sequential? x)) (not (contains? types (first x)))))
      (throw (Exception. (str x " must have an explicit type."))))))

(defn- rename-element [i]
  (symbol (str "-tex" i)))

(defn- transform-element [e]
  (let [tuple (type-tuple (typeof e))]
    (with-meta
      (list
       (swizzle tuple)
       (list 'texture2DRect (rename-element (element-index e)) '--coord))
      ^e)))

(defn- process-map
  "Transforms the body, and pulls out all the relevant information."
  [expr]
  (validate-elements expr)
  (validate-params expr)
  ;(validate-results expr)
  (let [[elements params]
          (separate element? (realize (tree-filter expr #(and (symbol? %) (typeof %)))))
        declarations
          (list
           'do
           (map
            #(list
              'declare
              (list
               'uniform
               (add-meta (rename-element (element-index %)) :tag 'sampler2DRect)))
            elements)
           (map
            #(list
              'declare
              (list
               'uniform
               (add-meta % :tag (typeof %))))
            params))
       body
         (->
          expr
          (apply-transforms
           (list*
            (replace-with :coord '--coord)
            (map #(replace-with % (transform-element %)) elements)))
          transform-operator-results
          wrap-and-prepend
          transform-glsl)
       results
         (map #(:tag ^%) (tree-filter body #(:result ^%)))  
       body
         (list
          'do
          declarations
          (typecast-results body))]
    {:body body
     :elements (zipmap elements (map typeof elements))
     :params (zipmap params (map typeof params))
     :results results}))

(defn- create-write-texture [typecast dim]
  (let [tuple  (type-tuple typecast)
        format (type-format typecast)
        i-f    (write-format format tuple)
        p-f    (pixel-format tuple)]
    (if (nil? i-f) (throw (Exception. (str "Cannot write to texture of type " typecast))))
    (create-texture :texture-rectangle dim (first i-f) p-f format tuple)))

(defn- run-map
  [info params elements dim]
  ;Check number of elements
  (if (not= (count elements) (count (:elements info)))
    (throw (Exception. (str "Expected " (count (:elements info)) " elements, was given " (count elements) "."))))
  ;Check dimensions of elements
  (if (and
        (not (empty? elements))
        (apply not= (list* dim (map :dim elements))))
    (throw (Exception. (str "All data must be of the same dimension.  Given dimensions are " (apply str (map :dim elements))))))
  (let [targets
        (map
          (fn [[typ dim]] (create-write-texture typ dim))
          (map (fn [x] [x dim]) (:results info)))]
    (set-params params)
    (apply uniform (list* :__dim (map float dim)))
    (attach-textures
      (interleave (map rename-element (range (count elements))) elements)
      targets)
    (apply draw dim)
    (doseq [e (distinct elements)]
      (release! e))
    (if (= 1 (count targets)) (first targets) targets)))

(defn create-map [expr]
  (let [info    (process-map expr)
        program (create-operator (:body info))
        wrap    #(map
                  (fn [[s type]] (wrap s (type-tuple type)))
                  (partition 2 (interleave % (-> info :elements vals sort))))]
    (fn this
      ([size]
        (this {} [] (rectangle size)))
      ([params elements-or-size]
        (if (number? elements-or-size)
          (this params [] (rectangle elements-or-size))
          (let [elements (wrap elements-or-size)]
            (this params elements (:dim (first elements))))))
      ([params elements dim]
        (let [elements (wrap elements)]
          (with-program program
            (run-map info params elements dim)))))))

;;;;;;;;;;;;;;;;;;

(defvar- reduce-program
  '(let [#^float2 source-coord (* (floor --coord) 2.0)
         #^bool x (> (.x --bounds) (.x source-coord))
         #^bool y (> (.y --bounds) (.y source-coord))]
     (<- #^type a #^lookup (texture2DRect --data source-coord))
     (if x
       (reduce a #^lookup (texture2DRect --data (+ source-coord (float2 1.0 0.0)))))
     (if y
       (reduce a #^lookup (texture2DRect --data (+ source-coord (float2 0.0 1.0)))))
     (if (and x y)
       (reduce a #^lookup (texture2DRect --data (+ source-coord (float2 1.0 1.0)))))
     (<- (-> :frag-data (nth 0)) #^result a)))

(defn- transform-reduce-program [type]
  (let [tuple (type-tuple type)]
    (apply-transforms
     reduce-program
     (list
      #(if (= 'type (:tag ^%)) (add-meta % :tag type))
      #(if (= 'lookup (:tag ^%)) (add-meta (list (swizzle tuple) (add-meta % :tag type)) :tag nil))
      #(if (= 'result (:tag ^%)) (add-meta (typecast (add-meta % :tag type)) :tag nil))))))

(defn process-reduce [expr]
  (let [params (tree-filter expr #(and (symbol? %) (not (element? %)) (:tag ^%)))
        body (->
              expr
              transform-glsl
              (transform-results (fn [x] (map #(add-meta % :result true) x)))) 
        type (first (map #(:tag ^%) (tree-filter body #(:result ^%))))
        body (->
              body
              (apply-transforms
               (list
                (replace-with '%1 (add-meta 'b :tag type))
                (replace-with '%2 (add-meta 'c :tag type))
                #(if (:result ^%) (add-meta (list '<- 'b (add-meta % :result false)) :result false)))))]
    {:type
       type
     :params
       (zipmap params (map typeof params))
     :body
       (list
        'do
        '(declare (uniform #^sampler2DRect --data))
        '(declare (uniform #^float2 --bounds))
         (list
           'do
           (map #(list 'declare (list 'uniform %)) params))
        (list
        'defn 'void 'reduce
        (vector
         (list 'inout (with-meta 'b {:tag type}))
         (list 'in (with-meta 'c {:tag type})))
         body)
       (wrap-and-prepend (transform-reduce-program type)))}))

(defn create-reduce
  [expr]
  (let [info (process-reduce expr)
        program (create-operator (:body info))]
    (fn this
      ([data]
        (this {} data))
      ([params data]
        (unbind-write)
        (let [data (wrap data (-> info :type type-tuple))
              dim* (:dim data)]
          (with-program program
            (set-params params)
            (attach-textures [] [data])
            (loop [dim dim*, input data]
              (if (= [1 1] dim)
                (do
                  (let [result (unwrap-first input)]
                    (release! input)
                    (seq result)))
                (let [half-dim  (map #(Math/ceil (/ % 2.0)) dim)
                      target    (mimic-texture input half-dim)
                      [w h]     half-dim
                      bounds    (map #(* 2 (Math/floor (/ % 2.0))) dim)]
                    (apply uniform (list* :__bounds bounds))
                    (apply uniform (list* :__dim half-dim))
                    (attach-textures [:__data input] [target])
                    (draw 0 0 w h)
                    (release! input)
                    (recur half-dim target))))))))))






