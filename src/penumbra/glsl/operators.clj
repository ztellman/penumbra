;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.operators
  (:use [penumbra opengl]
        [penumbra.opengl core]
        [penumbra.translate core operators]
        [penumbra.glsl core]
        [penumbra.opengl.context :only (draw-frame-buffer)]
        [clojure.contrib
         (seq :only (separate indexed))
         (def :only (defvar- defn-memo))
         (pprint :only (pprint))])
  (:require [clojure.zip :as zip]
            [penumbra.translate.c :as c]
            [penumbra.opengl.texture :as tex]
            [penumbra.data :as data]
            [penumbra.opengl.capabilities :as cap]
            [penumbra.opengl.frame-buffer :as fb]
            [penumbra.glsl.effects :as fx]))

;;;

(defn- typecast-float4 [x]
  (condp = (:tag (meta x))
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

(defvar- texture-tuple
  {:float 1
   :float2 2
   :float3 3
   :float4 4
   :int 1
   :int2 2
   :int3 3
   :int4 4})

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

(defvar- sampler-type
  {[:texture-1d 1] :sampler1D
   [:texture-rectangle 1] :sampler1DRect
   [:texture-2d 2] :sampler2D
   [:texture-rectangle 2] :sampler2DRect
   [:texture-3d 3] :sampler3D
   [:texture-rectangle 3] :sampler3DRect})

;;;

(defn typeof-element [e]
  (texture-type [(->> e data/params :internal-type) (->> e data/params :internal-format tex/internal-format->tuple)]))

(defn typeof-param [p]
  (if (number? p)
    (if (int? p) :int :float)
    (keyword (str (if (-> p first int?) "int" "float") (count p)))))

(defn-memo rename-element [idx]
  (symbol (str "-tex" idx)))

(defn- transform-dim [x]
  (let [idx (element-index (second x))]
    (add-meta (symbol (str "-dim" idx)) :tag (*typeof-dim* idx))))

(defn transform-element [e]
  (if (element? e)
    (let [location (when-not (symbol? e) (last e))
          element (if (symbol? e) e (first e))
          idx (element-index element)
          tex-name (rename-element idx)]
      (list
       (swizzle (texture-tuple (typeof e)))
       (condp = (*elements* idx)
         [:texture-1d 1]
         `(~'texture1D ~tex-name ~location)
         [:texture-rectangle 1]
         `(~'texture1DRect ~tex-name ~location)
         [:texture-2d 2]
         `(~'texture2D ~tex-name ~(or location '(div :coord :dim)))
         [:texture-rectangle 2]
         `(~'texture2DRect
           ~tex-name
           ~(cond
             (nil? location)
             :coord
             (= :float2 (typeof location))
             location
             (= :float (typeof location))
             `(~'float2 (floor (mod ~location (.x (~'dim ~element))))
                        (floor (div ~location (.x (~'dim ~element)))))
             (= :int (typeof location))
             `(~'float2 (floor (mod (float ~location) (.x (~'dim ~element))))
                        (floor (div (float ~location) (.x (~'dim ~element)))))
             :else
             (println "Don't recognize index type" location (typeof location))))
         [:texture-3d 3]
         `(~'texture3D ~tex-name ~location)
         [:texture-rectangle 3]
         `(~'texture3DRect ~tex-name ~location))))))

(defmacro with-glsl [& body]
  `(binding [*typeof-param* typeof-param
             *typeof-element* typeof-element
             *typeof-dim* (fn [idx#] (texture-type [:float (second (*elements* idx#))]))
             *dim-element* tex/dim
             *transformer* transformer,
             *generator* generator,
             *inspector* inspector,
             *preprocessor* transform-numbers,
             *tagger* c/tagger]
     ~@body))

;;;

(defvar- fixed-operator-transform
  '((<- -coord (-> :multi-tex-coord0 .xy (* -dim)))
    (<- :position (* :model-view-projection-matrix :vertex))))

(defvar- fixed-render-transform
  `{:position (* :model-view-projection-matrix :vertex)})

(defn- wrap-uniform
  ([x] (list 'declare (list 'uniform x)))
  ([x type] (list 'declare (list 'uniform (add-meta x :tag type)))))

(defn- wrap-attribute
  ([x] (list 'declare (list 'attribute x)))
  ([x type] (list 'declare (list 'attribute (add-meta x :tag type)))))

(defn- wrap-varying
  ([x] (list 'declare (list 'varying x)))
  ([x type] (list 'declare (list 'varying (add-meta x :tag type)))))

(defn- prepend-index
  "Adds an -index variable definition to beginning of program if :index is used anywhere inside"
  [x]
  (let [index
        '((<-
          -index
          (-> -coord .y floor (* (.x -dim)) (+ (-> -coord .x floor)))))]
    (if ((set (flatten x)) :index)
      (concat
        index
        (apply-transforms [(replace-with :index #^:float '-index)] x))
      x)))

(defn prepend-lighting
  [x]
  (if ((set (flatten x)) 'lighting)
    (do
      (list 'do fx/lighting x))
    x))

(defn- frag-data-typecast
  "Tranforms the final expression into one or more assignments to gl_FragData[n]"
  [results]
  (list* 'do
         (map
          (fn [[idx e]]
            (list '<- (list '-> :frag-data (list 'nth idx)) (typecast-float4 e)))
          (indexed results))))

(defn- wrap-and-prepend
  "Defines -coord and -dim, and applies prepend-index"
  [x]
  (list
   '(do
      (declare (varying #^:float2 -coord))
      (declare (uniform #^:float2 -dim))
      (declare (uniform #^:float2 -bounds)))
   (-> (list 'defn 'void 'main [] (-> x prepend-index)) prepend-lighting)))             

(defn- create-operator
  ([body]
     (create-program
      :literal true
      :extensions "#extension GL_ARB_texture_rectangle : enable"
      :vertex (wrap-and-prepend fixed-operator-transform)
      :fragment body)))

(defn- post-process
  "Transforms the body, and pulls out all the relevant information."
  ([results-fn program]
     (post-process results-fn #{} program))
  ([results-fn not-params program]
     (let [[elements params] (separate element? (tree-filter #(and (symbol? %) (typeof %)) program))
           elements (set (map element-index elements))
           locals (filter #(:assignment (meta %)) params)
           privates (filter #(and (symbol? %) (= \- (first (name %)))) params)
           params (remove (set (concat locals privates not-params)) (distinct params))
           declarations (list
                         'do
                         (map #(wrap-uniform (rename-element %) (sampler-type (*elements* %))) elements)
                         (map #(wrap-uniform (symbol (str "-dim" %)) (*typeof-dim* %)) elements)
                         (map #(wrap-uniform %) (distinct params)))
           body (->>
                 program
                 (tree-map #(when (first= % 'dim) (transform-dim %)))
                 (apply-element-transform transform-element)
                 (apply-transforms
                  (list
                   #(when (first= % 'dim) (transform-dim %))
                   (replace-with :coord #^:float2 '-coord)
                   (replace-with :dim #^:float2 '-dim)))
                 results-fn
                 wrap-and-prepend)]
       (list 'do declarations body))))

(defn compile-program [info]
  (->> info :program (post-process #(transform-results frag-data-typecast %)) create-operator))

(defn- operator-cache
  "Returns or creates the appropriate shader program for the types"
  [f program-creator]
  (let [programs (atom {})]
    (fn [program args]
      (let [info (apply signature args)
            element-sig (vec
                         (map
                          (fn [tx] [(:target (data/params tx)) (count (tex/dim tx))])
                          (:elements info)))
            sig (concat (:signature info) element-sig)
            hash (binding [*elements* element-sig]
                   (if-let [p (@programs sig)]
                     p
                     (do
                       (let [processed-info (f program (:params info) (:dim info) (:elements info))
                            processed-program (program-creator processed-info)
                            hash {:program processed-program
                                  :results (:results processed-info)}]
                        (swap! programs #(assoc % sig hash))
                        hash))))]
        (assoc hash
          :elements (:elements info)
          :params (:params info)
          :dim (:dim info))))))

(defn-memo param-lookup [n]
  (keyword (name n)))

(defn set-params [params]
  (doseq [[n v] params]
    (apply
      uniform
      (list*
        (param-lookup n)
        (seq-wrap v)))))

;;;

(defn- create-write-texture
  "Given the type (float4, etc.), creates the appropriate target texture"
  [typecast dim]
  (let [tuple  (type-tuple typecast)
        type   (type-format typecast)
        i-f    (cap/write-format type tuple)
        p-f    (tex/tuple->pixel-format tuple)]
    (when (nil? i-f)
      (throw (Exception. (str "Your graphics hardware does not support writing to texture of type=" type ", tuple=" tuple))))
    (create-texture
     :target :texture-rectangle
     :dim dim
     :internal-format (first i-f)
     :pixel-format p-f
     :internal-type type)))

(defn- run-map
  "Executes the map"
  [info]
  (with-frame-buffer
    (let [dim      (:dim info)
          elements (:elements info)
          params   (:params info)
          results  (force (:results info))
          targets  (map #(create-write-texture % dim) results)]
      (set-params params)
      (apply uniform (list* :_dim (map float dim)))
      (doseq [[idx d] (map vector (range (count elements)) (map tex/dim elements))]
        (apply uniform (list* (symbol (str "-dim" idx)) (map float d))))
      (fb/attach-textures
       (interleave (map rename-element (range (count elements))) elements)
       targets)
      (apply draw-frame-buffer dim)
      (doseq [e (distinct elements)]
        (when-not (:persist (meta e))
          (data/release! e)))
      (if (= 1 (count targets)) (first targets) targets))))

(defn create-map-template
  "Creates a template for a map, which will lazily create a set of shader programs based on the types passed in."
  [x]
  (let [cache (operator-cache process-map compile-program)]
    (fn [& args]
      (with-glsl
        (let [info (cache x args)]
          (with-program (:program info)
            (run-map info)))))))

;;;;;;;;;;;;;;;;;;

(defn- run-reduce
  [info]
  (with-frame-buffer
    (let [params (:params info)
          data (first (:elements info))]
      (set-params params)
      (fb/attach-textures [] [data])
      (loop [dim (tex/dim data), input data]
        (if (= [1 1] dim)
          (let [result (data/unwrap! input)]
            (data/release! input)
            (seq result))
          (let [half-dim  (map #(Math/ceil (/ % 2.0)) dim)
                target    (data/mimic input half-dim)
                [w h]     half-dim
                bounds    (map #(* 2 (Math/floor (/ % 2.0))) dim)]
            (apply uniform (list* :_bounds bounds))
            (apply uniform (list* :_dim half-dim))
            (fb/attach-textures [:_tex0 input] [target])
            (draw-frame-buffer 0 0 w h)
            (if (not (:persist (meta input)))
              (data/release! input))
            (recur half-dim target)))))))

(defn create-reduce-template
  "Creates a template for a reduce, which will lazily create a set of shader programs based on the types passed in."
  [x]
  (let [cache (operator-cache process-reduce compile-program)]
    (fn [& args]
      (with-glsl
        (let [info (cache x args)]
          (with-program (:program info)
            (run-reduce info)))))))

;;;

(defn process-attributes [program]
  (list 'do
        (map #(wrap-attribute %1 %2)
             (keys (:attributes program))
             (vals (:attributes program)))))

(defn process-varying [varying]
  (list 'do (map wrap-varying (keys varying) (vals varying))))

(defn- process-vertex [program params dim elements]
  (let [vertex   (transform-results
                  (fn [x] (list 'do (map (fn [[k v]] (list '<- k v)) x)))
                  (:vertex program))
        vertex   (reduce (fn [x [k v]] (tag-var k v x)) vertex (:attributes program))
        vertex   (:program (process-map vertex params dim elements))
        attribs  (list 'do (process-attributes program))
        varying  (->> program :vertex results keys (filter symbol?))  
        varying  (zipmap varying (map #(typeof-var % vertex) varying))]
    {:varying varying
     :vertex (list 'do
                   (process-varying varying)
                   attribs
                   (post-process identity (set (concat (-> program :attributes keys) (-> varying keys))) vertex))}))

;;TODO: tag attributes and varying
(defn- process-fragment [program varying params dim elements]
  (let [fragment (reduce (fn [x [k v]] (tag-var k v x)) (:fragment program) varying)
        {fragment :program} (process-map fragment params dim elements)]
    {:results (map #(-> % meta :tag) (results fragment))
     :fragment (list 'do
                     (process-varying varying)
                     (post-process #(transform-results frag-data-typecast %) (set (keys varying)) fragment))}))

(defn- process-renderer
  [program params dim elements]
  (let [program (merge
                {:vertex fixed-render-transform
                 :fragment :frag-color}
                program)]
    (let [vertex (process-vertex program params dim elements)
          fragment (process-fragment program (:varying vertex) params dim elements)]
      (merge vertex fragment))))

(defn- run-renderer
  [info f]
  (let [f* (fn [fb?]
             (let [dim      (:dim info)
                   [w h]    dim
                   elements (:elements info)
                   params   (:params info)
                   results  (force (:results info))
                   targets  (when fb?
                              (map #(create-write-texture % dim) results))]
               (apply declare-attributes (keys (:attributes info)))
               (set-params params)
               (apply uniform (list* :_dim (map float dim)))
               (doseq [[idx d] (map vector (range (count elements)) (map tex/dim elements))]
                 (apply uniform (list* (symbol (str "-dim" idx)) (map float d))))
               (fb/attach-textures
                (interleave (map rename-element (range (count elements))) elements)
                (when fb? targets))
               (with-viewport [0 0 w h]
                 (f))
               (doseq [e (distinct elements)]
                 (when-not (:persist (meta e))
                   (data/release! e)))
               (when fb?
                 (if (= 1 (count targets))
                   (first targets)
                   targets))))]
    (if *render-to-screen?*
      (f* false)
      (with-frame-buffer
        (fb/with-depth-buffer (:dim info)
          (f* true))))))

(defn create-renderer-template
  [programs]
  (let [cache (operator-cache
               process-renderer
               #(apply create-program
                       (concat
                        (apply concat %)
                        [:extensions "#extension GL_ARB_texture_rectangle : enable"
                         :literal true])))]
    (fn [args f]
      (with-glsl
        (let [info (cache programs args)]
          (with-program (:program info)
            (run-renderer
             (assoc info :attributes (:attributes programs))
             f)))))))
