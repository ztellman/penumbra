;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.compute.operators
  (:use [penumbra.opengl translate shader])
  (:use [penumbra.compute data])
  (:use [clojure.contrib.seq-utils :only (indexed)]))

(def fixed-transform
  '((set! coord (-> :multi-tex-coord0 .xy))
    (set! :position (* :model-view-projection-matrix :vertex))))

(defn- transform-assignment
  "Tranforms the final expression into one or more assignments to gl_FragData[n]"
  [expr]
  (let [body  (if (seq? (first expr)) (take (dec (count expr)) expr) nil)
        assn  (if (seq? (first expr)) (last expr) expr)
        assn* (if (vector? assn)
                (concat
                  (list 'do)
                  (map
                    (fn [[idx e]] (list 'set! (list '-> :frag-data (list 'nth idx)) e))
                    (indexed assn)))
                (list 'set! '(-> :frag-data (nth 0)) assn))]
    (if (nil? body)
      assn*
      (concat body (list assn*)))))

(defn create-operator
  ([source] (create-operator '() source))
  ([uniforms source]
    (create-program
      "#extension GL_ARB_texture_rectangle : enable"
      (concat
        '((varying vec2 coord))
        (if (seq? (first uniforms))
          (map #(cons 'uniform %) uniforms)
          (concat '(uniform) uniforms)))
      fixed-transform
      (transform-assignment source))))

;;;;;;;;;;;;;;;;;;


