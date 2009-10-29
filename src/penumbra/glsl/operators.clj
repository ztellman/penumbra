;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.operators
  (:use [penumbra opengl slate])
  (:use [penumbra.opengl.texture :only (create-texture release! texture?)])
  (:use [penumbra.opengl.framebuffer :only (pixel-format write-format)])
  (:use [penumbra.glsl core data])
  (:use [penumbra.translate.core])
  (:use [clojure.contrib.def :only (defn-memo)])
  (:use [clojure.set :only (difference)])
  (:use [clojure.contrib (seq-utils :only (separate indexed flatten)) (def :only (defvar-)) pprint])
  (:require [clojure.zip :as zip]))

;;;

(defn- typecast-float4 [x]
  (condp = (:tag ^x)
    :float   (list 'float4 x)
    :float2  (list 'float4 x 1.0 1.0)
    :float3  (list 'float4 x 1.0)
    :float4  x
    :color   (list 'float4 x)
    :color2  (list 'float4 x 1.0 1.0)
    :color3  (list 'float4 x 1.0)
    :color4  x
    :int     (list 'float4 x)
    :int2    (list 'float4 x 1 1)
    :int3    (list 'float4 x 1)
    :int4    x
    nil      (throw (Exception. (str "Cannot typecast \n" (with-out-str (print-tree x)))))))

(defvar- type-format
  {:color :unsigned-byte
   :color2 :unsigned-byte
   :color3 :unsigned-byte
   :color4 :unsigned-byte
   :float :float       
   :float2 :float
   :float3 :float
   :float4 :float
   :int :int
   :int2 :int
   :int3 :int
   :int4 :int})

(defvar- texture-type
  {[:unsigned-byte 1] :float
   [:unsigned-byte 2] :float2
   [:unsigned-byte 3] :float3
   [:unsigned-byte 4] :float4
   [:float 1] :float
   [:float 2] :float2
   [:float 3] :float3
   [:float 4] :float4
   [:int 1] :int
   [:int 2] :int2
   [:int 3] :int3
   [:int 4] :int4})

(defvar- swizzle { 1 '.x, 2 '.xy, 3 '.xyz, 4 '.xyzw })

;;;

(defn- typeof-element [s]
  (texture-type [(:internal-type s) (:tuple s)]))

(defn- int? [p]
  (let [cls (class p)]
    (if (or (= cls Integer) (= cls Integer/TYPE)) true false)))

(defn- typeof-param [p]
  (if (number? p)
    (if (int? p) :int :float)
    (keyword (str (if (-> p first int?) "int" "float") (count p)))))

(defn- apply-transforms [funs tree]
  (reduce #(tree-map %2 %1) tree funs))

(defn- element? [s]
  (and (symbol? s) (.startsWith (name s) "%")))

(defn- element-index [param]
  (if (= '% param)
    0
    (dec (Integer/parseInt (.substring (name param) 1)))))

(defn-memo create-element [index]
  (symbol (str "%" (inc index))))

(defn- replace-with [from to]
  #(if (= from %) to))

(defn process-elements
  [coll]
  (map
    #(if (vector? %)
      (add-meta (first %) :persist true)
      %)
    coll))

;;;;;;;;;;;;;;;;;;;;;

(def fixed-transform
  '((<- --coord (-> :multi-tex-coord0 .xy (* --dim)))
    (<- :position (* :model-view-projection-matrix :vertex))))

(defn- prepend-index [x]
  (let [index
        '((<-
          --index
          (-> --coord .y floor (* (.x --dim)) (+ (-> --coord .x floor)))))]
    (if (contains? (set (flatten x)) :index)
      (concat
        index
        (apply-transforms [(replace-with :index '--index)] x))
      x)))

(defn wrap-and-prepend [x]
  (list
   '(do
      (declare (varying #^:float2 --coord))
      (declare (uniform #^:float2 --dim)))
   (list 'defn 'void 'main [] (prepend-index x))))             

(defn- result?
  "This assumes you only traverse down the last element of the tree"
  [x]
  (or
    (vector? x)
    (not (sequential? x))
    (and
      (-> x first sequential? not)
      (-> x transformer first (not= 'do))
      (-> x transformer first (not= 'scope)))))

(defn- results [x]
  (if (result? x)
    (if (vector? x) x (list x))
    (results (last x))))

(defn- transform-results [f x]
  (loop [z (zip/seq-zip x)]
    (if (result? (zip/node z))
      (zip/root (zip/replace z (f (results x))))
      (recur (-> z zip/down zip/rightmost)))))

(defn create-operator
  ([body]
     (create-program*
      "#extension GL_ARB_texture_rectangle : enable"
      (wrap-and-prepend fixed-transform)
      body)))

(defn-memo param-lookup [n]
  (keyword (name n)))

(defn set-params [params]
  (doseq [[n v] params]
    (apply
      uniform
      (list*
        (param-lookup n)
        (seq-wrap v)))))

;;;;;;;;;;;;;;;;;;;

(defn- validate-elements
  "Make sure that there is one and only one type for each element"
  [x]
  (let [elements (tree-filter element? x)]
    (doseq [e (distinct (filter typeof elements))]
      (let [typs (distinct (map typeof (filter #(= e %) elements)))]
        (if (empty? typs)
          (throw (Exception. (str e " needs to have an explicitly defined type."))))
        (if (< 1 (count typs))
          (throw (Exception. (str "Ambiguous type for " e ", can't choose between " typs))))))))

(defn- validate-params
  "Make sure there isn't more than one type for each parameter"
  [x]
  (let [params (tree-filter #(and (not (element? %)) (typeof %)) x)]
    (doseq [e (distinct (filter typeof params))]
      (let [typs (distinct (map typeof (filter #(= e %) params)))]
        (if (< 1 (count typs))
          (throw (Exception. (str "Ambiguous type for " e ", can't choose between " typs))))))))


(defn- transform-operator-results
  "Tranforms the final expression into one or more assignments to gl_FragData[n]"
  [x]
  (transform-results
    (fn [results]
      (list* 'do
        (realize
          (map
            (fn [[idx e]]
              (list '<-
                (list '-> :frag-data (list 'nth idx))
                (add-meta e :result true)))
            (indexed results)))))
    x))

(defn typecast-results
  "Results must be of type float4, so we have to pad any different type to equal that"
  [x]
  (tree-map
   (fn [x]
     (if (:result ^x)
       (add-meta (typecast-float4 (add-meta x :result false)) :result false)
       x))
   x))

(defn-memo rename-element [i]
  (symbol (str "-tex" i)))

(defn- transform-element [e]
  (let [tuple (type-tuple (typeof e))]
    (with-meta
      (list
       (swizzle tuple)
       (list 'texture2DRect (rename-element (element-index e)) '--coord))
      ^e)))

(defn- wrap-uniform [x]
  (list 'declare (list 'uniform x)))

(defn- process-map
  "Transforms the body, and pulls out all the relevant information."
  [x]
  (validate-elements x)
  (validate-params x)
  (let [[elements params]
          (separate element? (realize (tree-filter #(and (symbol? %) (typeof %)) x)))
        declarations
          (list
           'do
           (map
            #(wrap-uniform (add-meta (rename-element (element-index %)) :tag 'sampler2DRect))
            (distinct elements))
           (map
            #(wrap-uniform (add-meta % :tag (typeof %)))
            (distinct params)))
       body
         (->>
          x
          (apply-transforms
           (list*
            (replace-with :coord '--coord)
            (replace-with :dim '--dim)
            (map #(replace-with % (transform-element %)) elements)))
          transform-operator-results
          wrap-and-prepend
          transform-glsl)
       results
         (map #(:tag ^%) (tree-filter #(:result ^%) body))
       body
         (list
          'do
          declarations
          (typecast-results body))]
    {:body body
     :elements (zipmap elements (map typeof elements))
     :params (zipmap params (map typeof params))
     :results results}))

(defn- create-write-texture
  "Given the type (float4, etc.), creates the appropriate target texture"
  [typecast dim]
  (let [tuple  (type-tuple typecast)
        format (type-format typecast)
        i-f    (write-format format tuple)
        p-f    (pixel-format tuple)]
    (if (nil? i-f) (throw (Exception. (str "Cannot write to texture of type " typecast))))
    (create-texture :texture-rectangle dim (first i-f) p-f format tuple)))

(defn- run-map
  "Executes the map"
  [info params elements dim]
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
      (if (not (:persist ^e))
        (release! e)))
    (if (= 1 (count targets)) (first targets) targets)))

(defn- tag-map-types
  "Applies types to map, so that we can create a program"
  [x types]
  (let [[elements params] (separate #(-> % first element?) types)]
    (->>
      x
      (apply-transforms
       (concat
        (map
         (fn [[element type]]
           #(if (and (element? %) (apply = (map element-index [element %]))) (add-meta % :tag type)))
         elements)
        (map
         (fn [[param type]]
           (replace-with param (add-meta param :tag type)))
         params))))))

(defn- map-cache
  "Returns or creates the appropriate shader program for the types"
  [x]
  (memoize
    (fn [types]
      (let [x       (tag-map-types x types)
            info    (process-map x)
            program (create-operator (:body info))]
        [info program]))))

(defn create-map-template
  "Creates a template for a map, which will lazily create a set of shader programs based on the types passed in."
  [x]
  (let [cache        (map-cache x)
        elements?    #(and (vector? %) (-> % first number? not))
        dim          #(or (:dim (first %)) (:dim (ffirst %)))
        to-symbol    (memoize #(symbol (name %)))
        num-elements (count (distinct (tree-filter element? x)))]
    (fn this
      ([elements-or-size]
        (if (elements? elements-or-size)
          (this {} elements-or-size (dim elements-or-size))
          (this {} [] elements-or-size)))

      ([params-or-elements elements-or-size]
        (let [params   (if (map? params-or-elements)
                         params-or-elements
                         {})
              elements (if (elements? elements-or-size)
                         elements-or-size
                         [])
              size     (if (elements? elements-or-size)
                         (dim elements-or-size)
                         elements-or-size)]
          (this params elements size)))

      ([params elements size]
        (let [elements
              (process-elements elements)
              size
              (if (number? size) (rectangle size) size)]
          ;;verify none of the elements are nil
          (doseq [[idx e] (indexed elements)]
            (if (nil? e)
              (throw (Exception. (str "Element at position " idx " is nil")))))
          ;;verify element count
          (if (not= (count elements) num-elements)
            (throw (Exception. (str "Expected " num-elements " elements, was given " (count elements) "."))))
          ;;verify all elements are the same size
          (if (and
               (> (count elements) 1)
               (apply not= (list* (map :dim elements))))
            (throw (Exception. (str "All data must be of the same dimension.  Given dimensions are " (apply str (map :dim elements))))))
          (let [param-map
                (zipmap (map to-symbol (keys params)) (map typeof-param (vals params)))
                element-map
                (zipmap (map create-element (range (count elements))) (map typeof-element (filter texture? elements)))
                [info program]
                (cache (merge element-map param-map))]
           (with-program program
             (run-map info params elements size))))))))

;;;;;;;;;;;;;;;;;;

(defvar- reduce-program
  '(let [#^:float2 -source-coord (* (floor --coord) 2.0)
         #^:bool -x (> (.x --bounds) (.x -source-coord))
         #^:bool -y (> (.y --bounds) (.y -source-coord))]
     (<- #^type -a #^lookup (texture2DRect --data -source-coord))
     (if -x
       (reduce -a #^lookup (texture2DRect --data (+ -source-coord (float2 1.0 0.0)))))
     (if -y
       (reduce -a #^lookup (texture2DRect --data (+ -source-coord (float2 0.0 1.0)))))
     (if (and -x -y)
       (reduce -a #^lookup (texture2DRect --data (+ -source-coord (float2 1.0 1.0)))))
     (<- (-> :frag-data (nth 0)) #^result -a)))

(defn- transform-reduce-program [type]
  (let [tuple (type-tuple type)]
    (apply-transforms
     (list
      #(if (= 'type (:tag ^%)) (add-meta % :tag type))
      #(if (= 'lookup (:tag ^%)) (add-meta (list (swizzle tuple) (add-meta % :tag type)) :tag nil))
      #(if (= 'result (:tag ^%)) (add-meta (typecast-float4 (add-meta % :tag type)) :tag nil)))
      reduce-program)))

(defn- process-reduce
  [x]
  (let [params (tree-filter #(and (symbol? %) (not (element? %)) (:tag ^%)) x)
        body (->>
              x
              transform-glsl
              (transform-results (fn [x] (map #(add-meta % :result true) x)))) 
        type (first (map #(:tag ^%) (tree-filter #(:result ^%) body)))
        body (->>
              body
              (apply-transforms
               (list
                (replace-with '%1 (add-meta '-b :tag type))
                (replace-with '%2 (add-meta '-c :tag type))
                #(if (:result ^%) (add-meta (list '<- '-b (add-meta % :result false)) :result false)))))]
    {:type
       type
     :params
       (zipmap params (map typeof params))
     :body
       (list
        'do
        '(declare (uniform #^:sampler2DRect --data))
        '(declare (uniform #^:float2 --bounds))
         (list
           'do
           (map #(list 'declare (list 'uniform %)) params))
        (list
        'defn 'void 'reduce
        (vector
         (list 'inout (with-meta '-b {:tag type}))
         (list 'in (with-meta '-c {:tag type})))
         body)
       (wrap-and-prepend (transform-reduce-program type)))}))

(defn- run-reduce
  [params data]
  (set-params params)
  (attach-textures [] [data])
  (loop [dim (:dim data), input data]
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
          (if (not (:persist ^input))
            (release! input))
          (recur half-dim target)))))

(defn- tag-reduce-types
  [x data params]
  (apply-transforms
    (list*
      #(if (element? %) (add-meta % :tag data))
      (map
        (fn [[param type]]
          (replace-with param (with-meta param :tag type)))
        params))
    x))

(defn- reduce-cache
  [x]
  (memoize
    (fn [data params]
      (let [x (tag-reduce-types x data params)
            info (process-reduce x)
            program (create-operator (:body info))]
        program))))

(defn create-reduce-template
  [x]
  (let [cache (reduce-cache x)]
    (fn this
      ([data]
        (this {} data))
      ([params data]
        (let [data      (if (vector? data) (-> [data] process-elements first) data)
              data-type (typeof-element data)
              program   (cache data-type params)]
          (with-program program
            (run-reduce params data)))))))

;;;







