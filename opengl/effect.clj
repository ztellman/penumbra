;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.effect)

(use 'penumbra.opengl.core
     'penumbra.opengl.geometry)

(import '(java.awt Font)
        '(com.sun.opengl.util.j2d TextRenderer))

;;;;;;;;;;;;;;;;;;;;;;;

(gl-import glColor3d color)
(gl-import glCullFace gl-cull-face)
(gl-import glPolygonMode gl-polygon-mode)
(gl-import glClearColor clear-color)
(gl-import glLightfv set-light-array)
(gl-import glLightf set-light)
(gl-import glMaterialfv set-material-array)
(gl-import glMaterialf set-material)
(gl-import glFogfv set-fog)
(gl-import glShadeModel shade-model)
(gl-import glLineWidth line-width)

;;;;;;;;;;;;;;;;;;;;;;;

(defn light [num & params]
  (let [light-num (translate-keyword (keyword (str "light" num)))]
    (doseq [[property value] (partition 2 params)]
      (let [property (translate-keyword property)]
        (if (sequential? value)
          (set-light-array light-num property (float-array (count value) value) 0)
          (set-light light-num property value))))))

(defn material [side & params]
  (let [side (translate-keyword side)]
    (doseq [[property value] (partition 2 params)]
      (let [property (translate-keyword property)]
        (if (sequential? value)
          (set-material-array side property (float-array (count value) value) 0)
          (set-material side property value))))))

(defn fog [& params]
  (doseq [[property value] (partition 2 params)]
    (let [value (if (sequential? value) value [value])]
      (set-fog
        (translate-keyword property)
        (float-array (count value) (map #(if (keyword? %) (translate-keyword %) %) value))
        0))))

(defn draw-solid [] (gl-polygon-mode :front-and-back :fill))
(defn draw-wireframe [] (gl-polygon-mode :front-and-back :line))
(defn draw-point-cloud [] (gl-polygon-mode :front-and-back :point))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(def lighting
  '(defn vec4 lighting
      [(in int light)
       (in vec3 normal)]
      (let [(vec4 ambient) (vec4 0.0)
            (vec4 diffuse) (vec4 0.0)
            (vec4 specular) (vec4 0.0)]
        (directional-light light normal ambient diffuse specular)
        (return
          (+
            (-> :front-light-model-product .sceneColor)
            (-> :front-material .ambient (* ambient))
            (-> :front-material .specular (* specular))
            (-> :front-material .diffuse (* diffuse)))))))

(def directional-light
  '(defn void directional-light
    [(in int i)
     (in vec3 normal)
     (inout vec4 ambient)
     (inout vec4 diffuse)
     (inout vec4 specular)]
    (let [(float n-dot-vp)  (max 0.0 (dot normal (-> :light-source (nth i) .position vec3 normalize)))
          (float n-dot-hv) (max 0.0 (dot normal (-> :light-source (nth i) .halfVector vec3 normalize)))]
      (declare (float pf))
      (if (= n-dot-vp 0.0)
        (set! pf 0.0)
        (set! pf (pow n-dot-hv (-> :front-material .shininess))))
      (+= ambient (-> :light-source (nth i) .ambient))
      (+= diffuse (-> :light-source (nth i) .diffuse (* n-dot-vp)))
      (+= specular (-> :light-source (nth i) .specular (* pf))))))

(def point-light
  '(defn void point-light
    [(in int i)
     (in vec3 eye)
     (in vec3 ec-position)
     (in vec3 normal)
     (inout vec4 ambient)
     (inout vec4 diffuse)
     (inout vec4 specular)]
    (let [(vec3 vp)
            (-> :light-source (nth i) .position vec3 (- ec-position))
          (float d)
            (length vp)
          vp
            (normalize vp)
          (float attenuation)
            (/ 1.0
              (+
                (-> :light-source (nth i) .constantAttenuation)
                (-> :light-source (nth i) .linearAttenuation (* d))
                (-> :light-source (nth i) .quadraticAttenuation (* d d))))
          (vec3 half-vector)
            (normalize (+ vp eye))
          (float n-dot-vp)
            (max 0.0 (dot normal vp))
          (float n-dot-hv)
            (max 0.0 (dot normal half-vector))]
      (declare (float pf))
      (if (= 0.0 n-dot-vp)
        (set! pf 0.0)
        (set! pf (pow n-dot-hv (-> :front-material .shininess))))
      (+= ambient (-> :light-source (nth i) .ambient (* attenuation)))
      (+= diffuse (-> :light-source (nth i) .diffuse (* n-dot-vp attenuation)))
      (+= specular (-> :light-source (nth i) .specular (* pf attenuation))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(def #^TextRenderer *text* (TextRenderer. (Font. "Tahoma" java.awt.Font/PLAIN 20) true true))

(defn write
  "writes string at normalized coordinates (x,y)"
  [string x y]
  (let [[_ _ w h] @view-bounds
        text-height (.. *text* (getBounds string) getHeight)]
    (.beginRendering *text* w h)
    (.draw *text* string (int (* x w)) (int (* y (- h text-height))))
    (.endRendering *text*)))


