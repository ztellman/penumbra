;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.compute
  (:require [penumbra.glsl.operators :as glsl]
            [penumbra.opengl.texture :as tex]
            [clojure.contrib.def :only (defmacro-)])
  (:use [penumbra.opengl.core :only (*render-to-screen?*)]
        [cantor :only (rectangle-factors)]))

(defmacro defmap [name & body]
  `(def ~name (glsl/create-map-template (quote ~body))))

(defmacro defreduce [name & body]
  `(def ~name (glsl/create-reduce-template (quote ~body))))

(defmacro defpipeline [name & params]
  `(def ~name (glsl/create-renderer-template (apply hash-map (quote ~params)))))

(defmacro with-pipeline [pipeline args & body]
  `(~pipeline ~args (fn [] ~@body)))

(defmacro render-to-screen [& body]
  `(binding [*render-to-screen?* true]
     ~@body))

(defn wrap
  ([s] (wrap s 1))
  ([s tuple-or-dim & params]
     (let [tuple (if (number? tuple-or-dim)
                   tuple-or-dim
                   (/ (count s) (apply * tuple-or-dim)))
           dim (if-not (number? tuple-or-dim)
                 tuple-or-dim
                 (rectangle-factors (/ (count s) tuple)))]
       (apply tex/wrap (list* s tuple dim params)))))
