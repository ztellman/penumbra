;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns tests.gpgpu
  (:use [penumbra slate compute])
  (:use [clojure test]))

(with-blank-slate
  (let [s (map float (range 24))
        s2 (map #(* % 2) s)
        s3 (map #(* % 3) s)]
    (testing "GPGPU"

      (testing "Map"

        (defmap identity-map #^float4 %1)
        (is (= (unwrap* (identity-map {} [s])) s))

        (defmap multiply-map (* #^float4 %1 #^float k))
        (is (= (unwrap* (multiply-map {:k 2.0} [s])) s2))

        (defmap index-map
          (let [i (* 4.0 :index)]
            (float4 i (+ 1.0 i) (+ 2.0 i) (+ 3.0 i))))
        (is (= (unwrap* (index-map 6)) s))

        (defmap add-map (+ #^float4 %1 #^float4 %2))
        (is (= (unwrap* (add-map {} [s2 s])) s3))

        (defmap split-map
          (let [a #^float4 %1]
            [a a]))
        (let [[a b] (map unwrap* (split-map {} [s]))]
          (is (= a s) (= b s)))

      (testing "Reduce"

        (defreduce sum #^float4 (+ %1 %2))
        (dotimes [i 60]
          (let [i (+ 1 i)]
            (let [s (map float (range (* 4 i)))]
              (is (= (apply + (sum s)) (apply + s)))))))))))
