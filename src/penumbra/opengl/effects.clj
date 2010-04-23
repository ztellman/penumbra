;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.effects
  (:use [penumbra.opengl.core])
  (:import [org.lwjgl BufferUtils]))

(gl-import- glColor3d color-3)
(gl-import- glColor4d color-4)
(gl-import- glEnable gl-enable)
(gl-import- glLight set-light-array)
(gl-import- glLightf set-light)
(gl-import- glMaterial set-material-array)
(gl-import- glMaterialf set-material)
(gl-import- glFog set-fog-array)
(gl-import- glFogf set-fog)
(gl-import- glHint gl-hint)
(gl-import- glBlendFunc gl-blend-func)

(gl-import glPolygonMode gl-polygon-mode)

(defn enable-high-quality-rendering
  "Sets all flags and hints necessary for high quality rendering."
  []
  (gl-hint :point-smooth-hint :nicest)
  (gl-hint :line-smooth-hint :nicest)
  (gl-hint :polygon-smooth-hint :nicest)
  (gl-hint :fog-hint :nicest)
  (gl-hint :perspective-correction-hint :nicest)
  (gl-enable :point-smooth)
  (gl-enable :line-smooth)
  (gl-enable :polygon-smooth)
  (gl-blend-func :src-alpha-saturate :one))

(defn color
  "Calls glColor.  Values are normalized between 0 and 1."
  ([v] (apply color v))
  ([r g b] (color-3 r g b))
  ([r g b a] (color-4 r g b a)))

(defn light
  "Sets values for light 'num'.  Example:
   (light 0
     :position [1 1 1 0])"
  [num & params]
  (let [light-num (enum (keyword (str "light" num)))]
    (doseq [[property value] (partition 2 params)]
      (let [property (enum property)]
        (if (sequential? value)
          (set-light-array light-num property (-> (BufferUtils/createFloatBuffer (count value)) (.put (float-array value)) .rewind))
          (set-light light-num property value))))))

(defn material
  "Sets material values for 'side'.  Example:
   (material :front-and-back
     :ambient-and-diffuse [1 0.25 0.25 1])"
  [side & params]
  (let [side (enum side)]
    (doseq [[property value] (partition 2 params)]
      (let [property (enum property)]
        (if (sequential? value)
          (set-material-array side property (-> (BufferUtils/createFloatBuffer (count value)) (.put (float-array value)) .rewind))
          (set-material side property value))))))

(defn fog
  "Sets values for fog.  Example:
    (fog
     :fog-start 0
     :fog-end 10
     :fog-color [0 0 0 0])"
  [& params]
  (doseq [[property value] (partition 2 params)]
    (if (sequential? value)
      (set-fog-array (enum property) (-> (BufferUtils/createFloatBuffer (count value)) (.put (float-array (map #(or (enum %) %) value))) .rewind))
      (set-fog (enum property) (if (keyword? value) (enum value) value)))))

(defn render-mode
  "Sets current render-mode.  Valid modes are [:solid :wireframe :point-cloud]."
  [mode]
  (condp = mode
    :solid (gl-polygon-mode :front-and-back :fill)
    :wireframe (gl-polygon-mode :front-and-back :line)
    :point-cloud (gl-polygon-mode :front-and-back :point)))

(defn with-render-mode
  "Sets render-mode within inner scope.  Valid modes are [:solid :wireframe :point-cloud]."
  [mode body]
  (let [prev-mode (get-integer :polygon-mode)]
     (render-mode mode)
     (try
      (body)
      (finally
       (gl-polygon-mode :front-and-back prev-mode)))))