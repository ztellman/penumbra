;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns examples.n-body
  (:use [penumbra slate compute]))

;NOT COMPLETE

(def s (create-slate))

(with-slate s
  (defmap add
    (+ %1 (* k %2)))

  (defmap kinetic-energy
    (let [m %1, v %2]
      (float3 (* 0.5 m (dot v v)))))

  (defmap potential-energy
    (let [m1 %1, m2 (lookup %1 idx)
          p1 %2, p2 (lookup %2 idx)]
      (float3 (* m1 m2 (length (- p1 p2))))))

  (defmap gravity
    (let [m2 (lookup %1 idx)
          p1 %2, p2 (lookup %2 idx)
          diff (- p1 p2)]
      (? (= :index (float idx))
        (float3 0.0)
        (* (normalize diff)
           (/ (* g m2) (dot diff diff))))))

  (let [num 50
        m (take num (repeatedly #(float (* (rand) 1.0))))
        p (map float (take (* 3 num) (repeatedly #(float (* (rand) 1.0)))))
        v (map float (take (* 3 num) (repeat (float 0.0))))
        m (wrap m 1)
        p (wrap p 3)
        v (wrap v 3)]
    (gravity {:g 6.673e-11 :idx 0} [m p])
    (kinetic-energy [m v])
    (potential-energy {:idx 0} [m p])
    (add {:k 0.5} [p v])))