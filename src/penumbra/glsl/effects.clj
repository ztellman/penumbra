;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.effects)

(def lighting
  '((defn vec4 lighting
      [(in #^int i)
       (in #^float3 normal)]
      (let [n-dot-vp (max 0.0 (dot normal (-> :light-source (nth i) .position float3 normalize)))
            n-dot-hv (max 0.0 (dot normal (-> :light-source (nth i) .halfVector float3 normalize)))]
        (<- #^float pf (if (= n-dot-vp 0.0) 0.0 (pow n-dot-hv (-> :front-material .shininess))))
        (<- #^float4 ambient  (-> :light-source (nth i) .ambient))
        (<- #^float4 diffuse  (-> :light-source (nth i) .diffuse (* n-dot-vp)))
        (<- #^float4 specular (-> :light-source (nth i) .specular (* pf)))
        (return
         (+
          (-> :front-light-model-product .sceneColor)
          (-> :front-material .ambient (* ambient))
          (-> :front-material .specular (* specular))
          (-> :front-material .diffuse (* diffuse))))))))