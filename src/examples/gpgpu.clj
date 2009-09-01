;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns examples.gpgpu
  (:use [penumbra slate compute]))

(def dim 1e5)
(def tuple 4)
(def source (float-array (range (* tuple dim))))

'(with-blank-slate
  (def a (wrap source tuple))
  (def b (wrap source tuple))
  (defmap op
    (let [a #^float4 %1
          b #^float4 %2
          k #^float scale]
      #^float4 (+ a (* b k))))
  (println (take 20 (seq (unwrap (op {:scale 1.0} [a a]))))))
  '(time
    (dotimes [_ 1e2]
      (op {:scale 1.0} [a a])))

(with-blank-slate
  (defmap series
    (let [(float a) (* 4.0 :index)]
      ;(float4 a (+ 1.0 a) (+ 2.0 a) (+ 3.0 a))))
      (float4 1.0)))
  (defreduce sum
    #^float4 (+ %1 %2))
  (dotimes [_ 1]
    (time
      (let [n (int 5e6)
            results (apply + (seq (sum (series n))))]
      ;(println results (/ (* (* n 4) (dec (* n 4))) 2)))))
        (println results (* n 4))))))


