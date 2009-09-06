;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns examples.gpgpu
  (:use [penumbra slate compute]))

(with-blank-slate
  (defmap generate
    (let [(float s) (sin :index)]
      #^float3
      (normalize
        (float3 s (* 3.0 (cos :index)) (/ (* s s) 2.0)))))
  (defreduce find-max #^float3
    (max %1 %2))
  (dotimes [_ 10]
    (time (println (find-max (generate 5e6))))))