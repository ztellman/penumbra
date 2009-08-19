;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns examples.gpgpu
  (:use [penumbra opengl slate])
  (:use [penumbra.opengl texture core])
  (:use [penumbra.glsl data operators]))

(def dim 1e4)
(def tuple 4)
(def source (float-array (range (* tuple dim))))

(defn op [program]
  (with-program program
    (let [data    (wrap source tuple)
          target  (mimic-texture data)]
      (attach-textures ['tex data] [target])
      (println (frame-buffer-status))
      (draw)
      (println (take 20 (seq (unwrap target))))
      (release! data)
      (release! target))))

(with-blank-slate
  (def operator
      (create-operator
        '[(sampler2DRect tex)]
        '(* (texture2DRect tex --coord) 8.)))
  '(time (dotimes [_ 50]
    (doall (map #(* 8 (int %)) (seq source)))))
  (binding [*tex-count-threshold* 500]
    (time (dotimes [_ 1]
      (op operator)))))
