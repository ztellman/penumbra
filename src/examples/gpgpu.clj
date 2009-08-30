;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns examples.gpgpu
  (:use [penumbra slate compute]))

(def dim 1e3)
(def tuple 4)
(def source (float-array (range (* tuple dim))))

(with-blank-slate
  (def data (wrap source tuple))
  (defmap op
    (let [a #^float4 %1
          b #^float4 %2
          k #^float scale]
      #^float4 (+ a (* b k))))
  (println (take 20 (unwrap (op {:scale 0.5} [data data])))))

'(with-blank-slate
  (defmap op (float4 :coord 1.0 1.0))
  (println (take 20 (unwrap (op 20)))))
