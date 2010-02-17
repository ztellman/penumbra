;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.openal.core
  (:use [clojure.contrib.def])
  (:import [org.lwjgl.openal AL AL10 AL11 ALC10 ALC11]
           [java.lang.reflect Field Method]))

(defvar- containers [AL10 AL11 ALC10 ALC11])

(defn- get-fields [#^Class static-class]
  (. static-class getFields))

(defn- get-methods [#^Class static-class]
  (. static-class getMethods))

(defn- contains-field? [#^Class static-class field]
  (first
   (filter
    #{ (name field) }
    (map #(.getName #^Field %) (get-fields static-class)))))

(defn- contains-method? [static-class method]
  (first
   (filter
    #{ (name method) }
    (map #(.getName #^Method %) (get-methods static-class)))))

(defn- field-container [field]
  (first (filter #(contains-field? % field) containers)))

(defn- method-container [method]
  (first (filter #(contains-method? % method) containers)))

(defn- get-al-method [method]
  (let [method-name (name method)]
    (first (filter #(= method-name (.getName #^Method %)) (mapcat get-methods containers)))))

(defn-memo enum-name
  "Takes the numeric value of a gl constant (i.e. AL_LOOPING), and gives the name"
  [enum-value]
  (if (= 0 enum-value)
    "NONE"
    (.getName
     #^Field (some
              #(if (= enum-value (.get #^Field % nil)) % nil)
              (mapcat get-fields containers)))))

(defn-memo enum [k]
  (when (keyword? k)
    (let [gl (str "AL_" (.. (name k) (replace \- \_) (toUpperCase)))
          sym (symbol gl)
          container (field-container sym)]
      (when (nil? container)
        (throw (Exception. (str "Cannot locate enumeration " k))))
      (eval `(. ~(field-container sym) ~sym)))))

(defn- get-doc-string [method]
  (str "Wrapper for " method ".  "
       "Type signature: ["
       (apply
        str
        (interpose
         " "
         (map
          #(.getCanonicalName #^Class %)
          (.getParameterTypes #^Method (get-al-method method))))) "]."))

(defmacro al-import
  [import-from import-as]
  (let [method-name (str import-from)
        container (method-container import-from)]
    (when (nil? container)
      (throw (Exception. (str "Cannot locate method " import-from))))
    (let [doc-string (get-doc-string import-from)]
      `(defmacro ~import-as
         ~doc-string
         [& args#]
         `(. ~'~container ~'~import-from ~@(map (fn [x#] (or (enum x#) x#)) args#))))))

(defmacro al-import-
  "Private version of al-import"
  [name & decls]
  (list* `al-import (with-meta name (assoc (meta name) :private true)) decls))


