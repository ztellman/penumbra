;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.examples.gpgpu
  (:use [penumbra.interface.slate])
  (:use [penumbra.opengl core shader texture])
  (:use [penumbra.compute data operators]))

(def dim 1e2)
(def tuple 4)
(def source (float-array (range (* tuple dim))))

(defn op [program]
  (with-program program
    (let [data          (tex source tuple)
          [data target] (attach-textures
                          ['tex data]
                          [(create-tex data)])]
        ;(println (frame-buffer-status))
        (draw)
        ;(println (take 20 (seq (array target))))
        (release! data)
        (release! target))))

(with-slate
  (def operator
      (create-operator
        '[(sampler2DRect tex)]
        '(* (texture2DRect tex pn-coord) 8.)))
  '(time (dotimes [_ 50]
    (doall (map #(* 8 (int %)) (seq source)))))
  (binding [*tex-count-threshold* 2]
    (time (dotimes [_ 500]
      (op operator)))))
