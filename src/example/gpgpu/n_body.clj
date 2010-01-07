;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.gpgpu.n-body
  (:use [penumbra compute])
  (:use [clojure.contrib.seq-utils :only (partition-all flatten)])
  (:require [penumbra.slate :as slate]))

(defn gen [min max]
  (+ min (* (rand) (- max min))))

(defn gen-mass [num min max]
  (wrap (map float (concat [5e12] (take (dec num) (repeatedly #(gen min max))))) 1))

(defn gen-velocity [num min max]
  (wrap (map float (concat [0.0 0.0 0.0] (take (* 3 (dec num)) (repeatedly #(gen min max))))) 3))

(defn gen-position [num min max]
  (wrap (map float (concat [0.0 0.0 0.0] (take (* 3 (dec num)) (repeatedly #(gen min max))))) 3))

(slate/with-slate 
  (defmap add
    (+ %1 (* k %2)))

  (defmap kinetic-energy
    (let [m %1, v %2]
      (float3 (* 0.5 m (dot v v)))))

  (defmap potential-energy
    (let [m1 %1, m2 (%1 idx)
          p1 %2, p2 (%2 idx)]
      (float3 (* g m1 m2 (length (- p1 p2))))))

  (defmap gravity
    (let [m2 (%1 idx)
          p1 %2, p2 (%2 idx)
          diff (- p2 p1)]
      (? (= :index (float idx))
        (float3 0.0)
        (/ (* diff g m2) (pow (dot diff diff) 1.5)))))

  (defreduce sum (+ %1 %2))

  (defn prn-tex [t]
    (acquire! t)
    (println (partition 3 (unwrap* t))))

  (defn piecewise-add [f size]
    (let [add #(add {:k 1.0} [%1 %2])
          s (partition-all 10 (range size))]
      (reduce add (map #(reduce add (doall (map f %))) s))))

  (defn energy [m v p num]
    (let [ke (first (sum [(kinetic-energy [ [m] [v] ])]))
          pe (first (sum [(piecewise-add #(potential-energy {:idx % :g 6.673e-11} [ [m] [p] ]) num)]))]
      (println "kinetic:" ke "potential:" pe "total:" (+ ke pe))))

  (defn run-sim [num iterations]
    (let [m (gen-mass num 1e3 1e4)
          v (gen-velocity num -1 1)
          p (gen-position num -100 100)
          dt 0.01]
      (time
       (do
         (energy m v p num)
         (loop [v v, p p, i 1]
           (if (> i iterations)
             (do
               (energy m v p num)
               (release! m) (release! v) (release! p))
             (let [a  (piecewise-add #(gravity {:g 6.673e-11 :idx %} [ [m] [p] ]) num)
                   v* (add {:k dt} [ v a ])
                   p* (add {:k dt} [ p [v*] ])]
               (energy m v* p* num)
               (recur v* p* (inc i)))))))))

  (defn start []
    (dotimes [i 1]
      (let [num (* 100 (inc i))]
        (println num)
        (dotimes [_ 1]
          (run-sim num 1))))))