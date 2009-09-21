;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.compute
  (:require [penumbra.glsl.data :as glsl-data])
  (:require [penumbra.glsl.operators :as glsl-op])
  (:require [clojure.contrib.def :only (defmacro-)]))

(defn wrap
  "Turns a flat seq into a texture, grouped by tuple
  (def s (map float (range 20))
  (wrap s) -> float texture w/ count = 20
  (wrap s 4) -> float4 texture w/ count = 5"
  [& args]
  (apply glsl-data/wrap args))

(defn unwrap
  "Returns a flat array containing the texture data"
  [& args]
  (apply glsl-data/unwrap args))

(defn unwrap*
  "Returns a sequence containing the texture data"
  [& args]
  (seq (apply unwrap args)))

(defmacro defmap [name & body]
  `(def ~name (glsl-op/create-map-template (quote ~body))))

(defmacro defreduce [name & body]
  `(def ~name (glsl-op/create-reduce-template (quote ~body))))