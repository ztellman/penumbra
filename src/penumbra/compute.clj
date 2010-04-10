;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.compute
  (:require [penumbra.glsl.operators :as glsl])
  (:require [penumbra.opengl.texture :as tex])
  (:require [clojure.contrib.def :only (defmacro-)]))

(defmacro defmap [name & body]
  `(def ~name (glsl/create-map-template (quote ~body))))

(defmacro defreduce [name & body]
  `(def ~name (glsl/create-reduce-template (quote ~body))))

(defn wrap
  ([s] (wrap s 1))
  ([s tuple] (tex/wrap s tuple)))
