;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.compute
  (:require [penumbra.glsl.data :as glsl-data])
  (:require [penumbra.glsl.operators :as glsl-op]))

(defn wrap [& args]
  (apply glsl-data/wrap args))

(defn unwrap [& args]
  (apply glsl-data/unwrap args))

(defmacro defmap [name & body]
  `(def ~name (glsl-op/create-gmap (quote ~body))))