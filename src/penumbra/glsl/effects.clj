;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.effects)

(def lighting
  '((defn float4 lighting
      [(in int -i)
       (in float3 -normal)]
      (let [n-dot-vp (max 0 (dot -normal (-> :light-source (nth 0) .position float3 normalize)))
            n-dot-hv (max 0 (dot -normal (-> :light-source (nth 0) .halfVector float3 normalize)))]
        (let [pf (float (if (= n-dot-vp 0)
                          0
                          (pow n-dot-hv (-> :front-material .shininess))))
              ambient (float4 (-> :light-source (nth 0) .ambient))
              diffuse (float4 (-> :light-source (nth 0) .diffuse (* n-dot-vp)))
              specular (float4 (-> :light-source (nth 0) .specular (* pf)))]
          (return
           (+
            (-> :front-light-model-product .sceneColor)
            (-> :front-material .ambient (* ambient))
            (-> :front-material .specular (* specular))
            (-> :front-material .diffuse (* diffuse)))))))))
