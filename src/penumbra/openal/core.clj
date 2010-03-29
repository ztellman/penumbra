;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.openal.core
  (:use [clojure.contrib.def])
  (:import [org.lwjgl.openal AL AL10 AL11]
           [java.lang.reflect Field Method]))

;;;

(defvar *check-errors* true
  "Check errors after every OpenAL call.")

;;;

(defvar- containers [AL10 AL11])

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

(defn check-error
  ([]
     (check-error ""))
  ([name]
     (let [error (AL10/alGetError)]
       (if (not (zero? error))
         (throw (Exception. (str "OpenAL error: " name " " (enum-name error))))))))

(defn- get-parameters [method]
  (map
   #(keyword (.getCanonicalName #^Class %))
   (.getParameterTypes #^Method (get-al-method method))))

(defn- get-doc-string [method]
  (str "Wrapper for " method "."))

(defmacro al-import
  [import-from import-as]
  (let [method-name (str import-from)
        container (method-container import-from)]
    (when (nil? container)
      (throw (Exception. (str "Cannot locate method " import-from))))
    (let [doc-string (get-doc-string import-from)
          arg-list (vec (get-parameters import-from))
          doc-skip (if (contains? (meta import-as) :skip-wiki)
                     (:skip-wiki (meta import-as))
                     true)]
      `(defmacro ~import-as
         ~doc-string
         {:skip-wiki ~doc-skip
          :arglists (list ~arg-list)}
         [& args#]
         `(do
            (let [~'value# (. ~'~container ~'~import-from ~@(map (fn [x#] (or (enum x#) x#)) args#))]
              (when *check-errors* 
                (check-error ~'~method-name))
              ~'value#))))))

(defmacro al-import-
  "Private version of al-import"
  [import-from import-as]
  (list `al-import import-from (with-meta import-as (assoc (meta import-as) :private true))))

(defmacro al-import+
  "Documented version of al-import"
  [import-from import-as]
  (list `al-import import-from (with-meta import-as (assoc (meta import-as) :skip-wiki nil))))



