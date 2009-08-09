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

(def tuple 4)
(def dim 4)

(with-slate (create-slate dim)
  (def data (tex (float-array (range (* tuple dim))) tuple))
  (def operator
    (create-operator
      '[(sampler2DRect tex)]
      '(* (texture2DRect tex coord) 8.)))
  (with-program operator
    (let [target (attach (create-tex data) 0)]
      (gl-active-texture :texture0)
      (gl-bind-texture :texture-rectangle (:id data))
      (uniform-1i (uniform-location "tex") 0)
      (draw-buffers [0])
      (println (verify-frame-buffer))
      (draw)
      (println (seq (array target))))))