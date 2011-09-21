;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.translate.operators
  (:use [clojure.walk]
        [penumbra.translate core]
		[cantor :only (rectangle-factors)]
        [clojure.contrib.seq :only (indexed separate)]
        [clojure.contrib.def :only (defn-memo defvar-)]
        [penumbra.translate.core])
  (:require [clojure.zip :as zip]
            [penumbra.data :as data]))

;;

(def *dim-element* nil)
(def *typeof-dim* nil)
(def *typeof-element* nil)
(def *typeof-param* nil)

;;utilities

(defn first= [x y]
  (and (seq? x) (= (first x) y)))

(defn int? [p]
  (let [cls (class p)]
    (if (or (= cls Integer) (= cls Integer/TYPE)) true false)))

(defn tag= [x t]
  (and (meta? x) (= t (:tag (meta x)))))

(defn apply-transforms [funs tree]
  (reduce #(tree-map %2 %1) tree funs))

(defn replace-with [from to]
  #(if (= from %) to))

;;elements

(defn element? [s]
  (or
    (and (symbol? s) (re-find #"%$|%[0-9]+" (name s)))
    (and (seq? s) (< 1 (count s)) (element? (first s)))))

(defn-memo create-element [index]
  (symbol (str "%" (inc index))))

(defn element-index [x]
  (let [x (if (sequential? x) (first x) x)]
    (if (= '% x)
      0
      (dec (Integer/parseInt (.substring (name x) 1))))))

(defn- process-elements
  [coll]
  (vec
   (map
    #(if (vector? %)
       (add-meta (first %) :persist true)
       %)
    coll)))

(defn apply-element-transform [f x]
  (merge-meta
   x
   (cond
    (element? x) (or (f x) x)
    (sequential? x) (walk #(apply-element-transform f %) identity x)
    :else x)))

;;results

(defn- result?
  "This assumes you only traverse down the last element of the tree"
  [x]
  (or
   (vector? x)
   (map? x)
   (not (sequential? x))
   (let [frst (first x)]
     (and
      (not (and (-> x meta :tag nil?) (= 1 (count x)) (-> frst meta :tag)))
      (not (and (element? frst) (= 1 (count x))))
      (not (or (sequential? frst) (#{'do 'scope 'defn 'let} frst)))))))

(defn results [x]
  (if (result? x)
    (if (or (vector? x) (map? x))
      x
      (list x))
    (results (last x))))

(defn transform-results [f x]
  (loop [z (zip/seq-zip x)]
    (if (result? (zip/node z))
      (zip/root (zip/replace z (f (results x))))
      (recur (-> z zip/down zip/rightmost)))))

;;general program transformation

(defn param-dispatch [t]
  (cond
   (and (vector? t) (->> t first number?)) :dim
   (number? t) :dim
   (or (= :penumbra.opengl.texture/texture (type t)) (vector? t)) :elements ;;this should be using (satisfies? data/Data t) but that's surprisingly slow
   (map? t) :params
   (symbol? t) :symbol
   (keyword? t) :keyword
   :else (throw (Exception. (str "Don't recognize " (with-out-str (println t)))))))

(defn group-elements [params]
  (concat
   (filter #(not= :elements (param-dispatch %)) params)
   (list (vec (filter #(= :elements (param-dispatch %)) params)))))

(defn process-operator [x params elements]
  (->> x
       (apply-transforms ;;tag elements and params
        (concat
         (map (fn [[k v]]
                (let [x (-> k name symbol)]
                  (replace-with x (add-meta x :tag (*typeof-param* v)))))
              params)
         (map (fn [[idx e]]
                   (let [type (*typeof-element* e)]
                     (fn [x]
                       (when (and (element? x) (= idx (element-index x)))
                         (add-meta x :tag type)))))
              (indexed elements))))
       transform-expr))

;;special map operators

(defvar- element-convolution-expr
  '(let [-half-dim (/ (dim :element) 2.0)
         -start    (max (float2 0.0) (- :coord (floor -half-dim)))
         -end      (min :dim (+ :coord (ceil -half-dim)))]
     (for [(<- i (.x -start)) (< i (.x -end)) (+= i 1.0)]
       (for [(<- j (.y -start)) (< j (.y -end)) (+= j 1.0)]
         (let [-location (float2 i j)
               -offset   (- -location :coord)
               -lookup   (:element (+ -offset -half-dim))]
           :body)))))

(defvar- radius-convolution-expr
  '(let [-radius (float2 (float :radius))
         -start  (max (float2 0.0) (float2 (- :coord -radius)))
         -end    (min :dim (+ :coord -radius (float2 1.0)))]
     (for [(<- i (.x -start)) (< i (.x -end)) (+= i 1.0)]
       (for [(<- j (.y -start)) (< j (.y -end)) (+= j 1.0)]
         (let [-location (float2 i j)
               -offset   (- -location :coord)]
           :body)))))

(defmulti transform-convolution #(param-dispatch (second %)))

(defmethod transform-convolution :symbol [[_ element & body]]
  (let [body (apply-transforms
              (list
               (replace-with :offset '-offset)
               #(when (and (element? %) (symbol? %))
                  (if (= element %)
                    '-lookup
                    (list % '-location))))
              body)]
    (apply-transforms
     (list
      #(if (= :element %) element)
      #(if (= :body %) body))
     element-convolution-expr)))

(defmethod transform-convolution :dim [[_ radius & body]]
  (let [body (apply-transforms
              (list
               (replace-with :offset '-offset)
               #(when (and (element? %) (symbol? %))
                  (list % '-location)))
              body)]
    (apply-transforms
     (list
      #(if (= :radius %) radius)
      #(if (= :body %) body))
     radius-convolution-expr)))

;;signature

(defmulti signature (fn [& args] (->> args group-elements (map param-dispatch) vec)))

(defmethod signature [:dim :elements] [dim & elements]
  (apply signature (list* {} dim (if (empty? elements) [] (process-elements elements)))))

(defmethod signature [:elements] [& elements]
  (apply signature (list* {} (or elements []))))

(defmethod signature [:params :elements] [params & elements]
  (let [elements* (process-elements elements)]
    (apply signature (list* params (*dim-element* (first elements*)) elements*))))

(defmethod signature [:params :dim :elements] [params dim & elements]
  (let [elements (remove #(and (vector? %) (empty? %)) (process-elements elements))
        dim (if (number? dim) (rectangle-factors dim) dim)]
    {:signature [(map *typeof-param* params) (map *typeof-element* elements)]
     :params params
     :elements elements
     :dim dim}))

;;defmap

(defn process-map [program params dim elements]
  ;;verify elements are not nil
  (doseq [[idx e] (indexed elements)]
    (if (nil? e)
      (throw (Exception. (str "Element at position " idx " is nil")))))
  (let [dim
        (if (number? dim) (rectangle-factors dim) dim)
        program*
        (let [program
              (apply-transforms
               (list
                #(when (first= % 'convolve) (transform-convolution %))
                #(when (first= % 'dim) (add-meta % :tag (*typeof-dim* (-> % last element-index)))))
               program)]
          (process-operator program params elements))
        program-results
        (map typeof (results program*))]
    {:program program*
     :results program-results}))

;;defreduce

(defvar- reduce-program
  '(let [-source-coord (* (floor :coord) 2)
         -x (> (.x -bounds) (.x -source-coord))
         -y (> (.y -bounds) (.y -source-coord))]
     (<- -a (% -source-coord))
     (if -x
       (let [-b (% (+ -source-coord [1 0]))
             -c -a]
         :expr))
     (if -y
       (let [-b (% (+ -source-coord [0 1]))
             -c -a]
         :expr))
     (if (and -x -y)
       (let [-b (% (+ -source-coord [1 1]))
             -c -a]
         :expr))
     -a))

(defn process-reduce [program params dim elements]
  (let [program*
        (let [expr (apply-transforms
                    (list
                     (replace-with '%1 '-b)
                     (replace-with '%  '-b)
                     (replace-with '%2 '-c))
                    program)]
          (process-operator
           (tree-map
            (fn [x]
              (when (= :expr x)
                (transform-results #(list '<- '-a %) expr)))
            reduce-program)
           params
           elements))
        result*
        (typeof (results program))]
    {:program program*
     :result result*}))
