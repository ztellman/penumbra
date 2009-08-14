;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.effects)

(def lighting
  '((import (penumbra.glsl.effects directional-light))
    (defn vec4 lighting
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
            (-> :front-material .diffuse (* diffuse))))))))

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
 
