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
  (:use [clojure.contrib.seq-utils :only (indexed flatten)]))

(def fixed-transform
  '((set! pn-coord (-> :multi-tex-coord0 .xy))
    (set! :position (* :model-view-projection-matrix :vertex))))

(defn- transform-return-expr
  "Tranforms the final expression into one or more assignments to gl_FragData[n]"
  [expr]
  (let [body  (if (seq? (first expr)) (take (dec (count expr)) expr) nil)
        assn  (if (seq? (first expr)) (last expr) expr)
        assn* (if (vector? assn)
                (concat
                  (list 'do)
                  (map
                    (fn [[idx e]] (list 'set! (list '-> :frag-data (list 'nth idx)) e))
                    (indexed assn)))
                (list 'set! '(-> :frag-data (nth 0)) assn))]
    (if (nil? body)
      assn*
      (concat body (list assn*)))))

(defn create-operator
  ([source] (create-operator '() source))
  ([uniforms body]
    (create-program
      "#extension GL_ARB_texture_rectangle : enable"
      (concat
        '((varying vec2 pn-coord))
        (map #(cons 'uniform %) uniforms))
      fixed-transform
      (transform-return-expr body))))

;;;;;;;;;;;;;;;;;;

(defn- param? [s]
  (and (not (seq? s)) (.startsWith (name s) "%")))

(defn- params [tree]
  (let [leaves (filter #(not (seq? %)) (flatten tree))]
    (distinct (filter param? leaves))))

(defn- param-index [param]
  (if (= '% param)
    1
    (Integer/parseInt (.substring (name param) 1))))

(defn- apply-transforms [tree & funs]
  (reduce #(tree-map %1 %2) tree funs))

(defn- replace-fn [source result]
  #(if (= source %) result %))

(defn- tex-lookup [param]
  (list 'texture2DRect (symbol (str "pn-tex" (param-index param))) 'pn-coord))

(defn- tex-declaration [param]
  (list 'sampler2DRect (symbol (str "pn-tex" (param-index param)))))

(defn- gmap* [program params sources targets]
  (with-program program
    (doseq [[k v] params]
      (apply uniform `(~k ~@v)))
    (doseq [[idx t] (indexed sources)]
      (attach []))))

(defmacro gmap [params & body]
  (let [prm       (params body)
        fns       (concat
                    (map #(replace-fn % (tex-lookup %)) params)
                    #(replace-fn :coord 'pn-coord))
        body      (apply-transforms body fns)
        uniforms  (map tex-declaration prm)
        program   (create-operator uniforms body)]
    '(fn
      )))



