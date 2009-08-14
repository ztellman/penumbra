;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.compute.operators
  (:use [penumbra.opengl translate shader])
  (:use [penumbra.compute data])
  (:use [clojure.set :only (map-invert)])
  (:use [clojure.contrib.seq-utils :only (indexed flatten)]))

;;;;;;;;;;;;;;;;;;

(def fixed-transform
  '((set! --coord (-> :multi-tex-coord0 .xy))
    (set! :position (* :model-view-projection-matrix :vertex))))

(defn- transform-return-expr
  "Tranforms the final expression into one or more assignments to gl_FragData[n]"
  [expr]
  (let [expr    (if (seq? (first expr)) expr (list expr))
        body    (take (dec (count expr)) expr)
        assn*   (if (-> expr last first seq?) (last expr) (list (last expr)))
        assn    (concat
                  (list 'do)
                  (map
                    (fn [[idx e]] (list 'set! (list '-> :frag-data (list 'nth idx)) e))
                    (indexed assn*)))]
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

(def types (set '(bool int float bvec2 bvec3 bvec4 ivec2 ivec3 ivec4 vec2 vec3 vec4)))

(defn- apply-transforms [tree & funs]
  (reduce #(tree-map %1 %2) tree funs))

(defn- filter-typed-exprs
  "Filters out all typecast expressions whose second element satisfies 'predicate'"
  [predicate tree]
  (let [tr-seq    (tree-seq sequential? seq tree)]
    (filter
      #(and
        (sequential? %)
        (= 2 (count %))
        (types (first %))
        (predicate (second %)))
      tr-seq)))

(defn- filter-symbols [predicate tree]
  (let [leaves (filter #(not (seq? %)) (flatten tree))]
    (distinct (filter predicate leaves))))

(defn- element? [s]
  (and (not (seq? s)) (.startsWith (name s) "%")))

(defn- element-index [param]
  (if (= '% param)
    1
    (Integer/parseInt (.substring (name param) 1))))

;;;;;;;;;;;;;;;;;;;;;

(defn- validate-types [symbols typecasts]
  (doseq [s symbols]
    (let [types (distinct (map first (filter #(= s (second %)) typecasts)))]
      (if (zero? (count types))
        (throw (Exception. (str s " needs to have an explicitly defined type."))))
      (if (not= 1 (count types))
        (throw (Exception. (str "Ambiguous type for " s ", can't choose between " (vec types))))))))

(defn- transform-gmap [expr]
  (let [index '((set! --index (-> --coord .y (* (.x --dim)) (+ (.x --coord)))))
        expr  (if (contains? (flatten expr) :index) ;If we use :index, calculate linear index
                (concat
                  index
                  (apply-transforms (replace-with :index '--index) expr))
                expr)]
    (let [expr        (if (seq? (first expr)) expr (list expr))
          elems       (distinct (filter-symbols element? expr))
          elem-types  (distinct (filter-typed-exprs element? expr))
          params      (distinct (filter-symbols keyword? expr))
          param-types (distinct (filter-typed-exprs keyword? expr))]
      (validate-types elems elem-types)
      (validate-types params param-types)
        (let [expr (apply-transforms expr
                     #(if (= :coord %) '--coord %)
                     #(if (not (element? %)) % (list 'texture2DRect (symbol (str "-tex" (element-index %))) '--coord))
                     #(if (not (keyword? %)) % (symbol (str "-" (name %)))))
              decl (concat
                     (if (empty? elems)
                       '()
                       (map
                         #(conj '(uniform sampler2DRect) (symbol (str "-tex" %)))
                         (range (apply max (map element-index elems)))))
                     (map (fn [[type sym]] (list 'uniform type sym)) param-types))]
          [decl expr]))))



