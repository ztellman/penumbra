;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.teapot
  (:use [penumbra.opengl.core])
  (:import [org.lwjgl BufferUtils]
           [java.nio FloatBuffer]))

(def patch [
	  ;; rim 
	[102, 103, 104, 105, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
	  ;; body 
	[12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27],
	[24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40],
	  ;; lid 
	[96, 96, 96, 96, 97, 98, 99, 100, 101, 101, 101, 101, 0, 1, 2, 3,],
	[0, 1, 2, 3, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117],
	  ;; bottom 
	[118, 118, 118, 118, 124, 122, 119, 121, 123, 126, 125, 120, 40, 39, 38, 37],
	  ;; handle 
	[41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56],
	[53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 28, 65, 66, 67],
	  ;; spout
	[68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83],
	[80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95]])

(def cp [
        [0.2, 0, 2.7] [0.2, -0.112, 2.7], [0.112, -0.2, 2.7], [0,
	-0.2, 2.7], [1.3375, 0, 2.53125], [1.3375, -0.749, 2.53125],
	[0.749, -1.3375, 2.53125], [0, -1.3375, 2.53125], [1.4375,
	0, 2.53125], [1.4375, -0.805, 2.53125], [0.805, -1.4375,
	2.53125], [0, -1.4375, 2.53125], [1.5, 0, 2.4], [1.5, -0.84,
	2.4], [0.84, -1.5, 2.4], [0, -1.5, 2.4], [1.75, 0, 1.875],
	[1.75, -0.98, 1.875], [0.98, -1.75, 1.875], [0, -1.75,
	1.875], [2, 0, 1.35], [2, -1.12, 1.35], [1.12, -2, 1.35],
	[0, -2, 1.35], [2, 0, 0.9], [2, -1.12, 0.9], [1.12, -2,
	0.9], [0, -2, 0.9], [-2, 0, 0.9], [2, 0, 0.45], [2, -1.12,
	0.45], [1.12, -2, 0.45], [0, -2, 0.45], [1.5, 0, 0.225],
	[1.5, -0.84, 0.225], [0.84, -1.5, 0.225], [0, -1.5, 0.225],
	[1.5, 0, 0.15], [1.5, -0.84, 0.15], [0.84, -1.5, 0.15], [0,
	-1.5, 0.15], [-1.6, 0, 2.025], [-1.6, -0.3, 2.025], [-1.5,
	-0.3, 2.25], [-1.5, 0, 2.25], [-2.3, 0, 2.025], [-2.3, -0.3,
	2.025], [-2.5, -0.3, 2.25], [-2.5, 0, 2.25], [-2.7, 0,
	2.025], [-2.7, -0.3, 2.025], [-3, -0.3, 2.25], [-3, 0,
	2.25], [-2.7, 0, 1.8], [-2.7, -0.3, 1.8], [-3, -0.3, 1.8],
	[-3, 0, 1.8], [-2.7, 0, 1.575], [-2.7, -0.3, 1.575], [-3,
	-0.3, 1.35], [-3, 0, 1.35], [-2.5, 0, 1.125], [-2.5, -0.3,
	1.125], [-2.65, -0.3, 0.9375], [-2.65, 0, 0.9375], [-2,
	-0.3, 0.9], [-1.9, -0.3, 0.6], [-1.9, 0, 0.6], [1.7, 0,
	1.425], [1.7, -0.66, 1.425], [1.7, -0.66, 0.6], [1.7, 0,
	0.6], [2.6, 0, 1.425], [2.6, -0.66, 1.425], [3.1, -0.66,
	0.825], [3.1, 0, 0.825], [2.3, 0, 2.1], [2.3, -0.25, 2.1],
	[2.4, -0.25, 2.025], [2.4, 0, 2.025], [2.7, 0, 2.4], [2.7,
	-0.25, 2.4], [3.3, -0.25, 2.4], [3.3, 0, 2.4], [2.8, 0,
	2.475], [2.8, -0.25, 2.475], [3.525, -0.25, 2.49375],
	[3.525, 0, 2.49375], [2.9, 0, 2.475], [2.9, -0.15, 2.475],
	[3.45, -0.15, 2.5125], [3.45, 0, 2.5125], [2.8, 0, 2.4],
	[2.8, -0.15, 2.4], [3.2, -0.15, 2.4], [3.2, 0, 2.4], [0, 0,
	3.15], [0.8, 0, 3.15], [0.8, -0.45, 3.15], [0.45, -0.8,
	3.15], [0, -0.8, 3.15], [0, 0, 2.85], [1.4, 0, 2.4], [1.4,
	-0.784, 2.4], [0.784, -1.4, 2.4], [0, -1.4, 2.4], [0.4, 0,
	2.55], [0.4, -0.224, 2.55], [0.224, -0.4, 2.55], [0, -0.4,
	2.55], [1.3, 0, 2.55], [1.3, -0.728, 2.55], [0.728, -1.3,
	2.55], [0, -1.3, 2.55], [1.3, 0, 2.4], [1.3, -0.728, 2.4],
	[0.728, -1.3, 2.4], [0, -1.3, 2.4], [0, 0, 0], [1.425,
	-0.798, 0], [1.5, 0, 0.075], [1.425, 0, 0], [0.798, -1.425,
	0], [0, -1.5, 0.075], [0, -1.425, 0], [1.5, -0.84, 0.075],
	[0.84, -1.5, 0.075]])

(gl-import- glMap2f gl-map-2f)
(gl-import- glMapGrid2f gl-map-grid-2f)
(gl-import- glEvalMesh2 gl-eval-mesh-2)
(gl-import- glEnable gl-enable)
(gl-import- glRotatef gl-rotate)
(gl-import- glScalef gl-scale)
(gl-import- glTranslatef gl-translate)

(defn fill-buffer [buf ary]
  (.clear buf)
  (dotimes [i 4]
    (dotimes [j 4]
      (dotimes [k 3]
        (.put #^FloatBuffer buf (aget ary i j k)))))
  (.rewind buf))

(defn teapot [grid s]
  (gl-enable :auto-normal)
  (gl-enable :map2-vertex-3)
  (gl-rotate 270 1 0 0)
  (gl-scale (* 0.5 s) (* 0.5 s) (* 0.5 s))
  (gl-translate 0 0 -1.5)

  (let [p (make-array Float/TYPE 4 4 3)
        q (make-array Float/TYPE 4 4 3)
        r (make-array Float/TYPE 4 4 3)
        s (make-array Float/TYPE 4 4 3)
        buf (BufferUtils/createFloatBuffer (* 4 4 3))]
    (dotimes [i 10]
      (dotimes [j 4]
        (dotimes [k 4]
          (dotimes [l 3]
            (aset p j k l (float (-> cp (nth (-> patch (nth i) (nth (+ (* j 4) k)))) (nth l))))
            (aset q j k l (float (-> cp (nth (-> patch (nth i) (nth (+ (* j 4) (- 3 k))))) (nth l))))
            (when (= l 1)
              (aset q j k l (float (* -1 (aget q j k l)))))
            (when (< i 6)
              (aset r j k l (float (-> cp (nth (-> patch (nth i) (nth (+ (* j 4) (- 3 k))))) (nth l))))
              (when (= l 0)
                (aset r j k l (float (* -1 (aget r j k l)))))
              (aset s j k l (float (-> cp (nth (-> patch (nth i) (nth (+ (* j 4) k)))) (nth l))))
              (when (or (= 1 l) (= l 0))
                (aset s j k l (float (* -1 (aget s j k l)))))))))

      (fill-buffer buf p)
      (gl-map-2f :map2-vertex-3 0 1 3 4 0 1 12 4 buf)
      (gl-map-grid-2f grid 0 1 grid 0 1)
      (gl-eval-mesh-2 :fill 0 grid 0 grid)
      
      (fill-buffer buf q)
      (gl-map-2f :map2-vertex-3 0 1 3 4 0 1 12 4 buf)
      (gl-eval-mesh-2 :fill 0 grid 0 grid)

      (when (< i 6)

        (fill-buffer buf r)
        (gl-map-2f :map2-vertex-3 0 1 3 4 0 1 12 4 buf)
        (gl-eval-mesh-2 :fill 0 grid 0 grid)

        (fill-buffer buf s)
        (gl-map-2f :map2-vertex-3 0 1 3 4 0 1 12 4 buf)
        (gl-eval-mesh-2 :fill 0 grid 0 grid)))))