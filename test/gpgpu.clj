;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gpgpu
  (:use [clojure test]
        [penumbra compute])
  (:require [penumbra.app :as app]
            [penumbra.data :as data]))

(deftest run
  (app/with-gl
   (let [s (map float (range 12))
         s2 (map (partial * 2) s)
         s3 (map (partial * 3) s)]
     
     (testing "GPGPU"

       (testing "Map"

         (defmap identity-map %1)
         (doseq [tuple [3 4 3 4]] ;;repeated tuples test whether we're recreating the program when we shouldn't
           (let [tex (wrap s tuple)]
             (is (= s (seq (data/unwrap! (identity-map tex)))))))

         (defmap multiply-skip (* %2 k))
         (doseq [tuple [3 4 3 4]]
           (let [tex (wrap s tuple)
                 tex2 (wrap s2 tuple)]
             (is (= s2 (seq (data/unwrap! (multiply-skip {:k 2.0} tex2 tex)))))))

         (defmap multiply-map (* %1 k))
         (doseq [tuple [3 4]]
           (let [tex (wrap s tuple)]
             (is (= s2 (seq (data/unwrap! (multiply-map {:k 2.0} tex)))))))
         
         (defmap index-map
           (let [i (* 4 :index)]
             (+ (float4 i) [0 1 2 3])))
         (is (= (seq (data/unwrap! (index-map 3))) s))

         (defmap add-map (+ %1 %2))
         (doseq [tuple [3 4]]
            (let [tex1 (wrap s tuple), tex2 (wrap s2 tuple)]
              (is (= s3 (seq (data/unwrap! (add-map tex1 tex2)))))))

         (defmap split-map [%1 (* 2.0 %1) (* 3.0 %1)])
         (doseq [tuple [3 4 3 4]]
            (let [tex (wrap s tuple)]
              (let [[a b c] (map data/unwrap! (split-map tex))]
                (is (= (seq a) s))
                (is (= (seq b) s2))
                (is (= (seq c) s3))))))

       (testing "Reduce"

         (defreduce sum (+ %1 %2))
         (dotimes [i 120]
           (let [i (+ 1 i)]
             (let [s (map float (range (* 4 i)))
                   tex (wrap s 4)]
               (is (= (apply + (seq (sum tex))) (apply + s)))))))))))
