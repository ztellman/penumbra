;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.texture
  (:use [clojure.contrib.def :only (defmacro- defn-memo)])
  (:use [clojure.contrib.seq-utils :only (separate)])
  (:use [penumbra.opengl.core])
  (:import (java.nio ByteBuffer FloatBuffer))
  (:import (com.sun.opengl.util BufferUtil))
  (:import (com.sun.opengl.util.texture TextureIO))
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;

(defstruct texture-struct
  :target :id
  :dim :size :tuple
  :internal-type :internal-format :pixel-format
  :permanent :ref-count :attach-point)

(defn num-dimensions [t]
  (count (:dim t)))

(defn sizeof [t]
  (* (apply * (:dim t)) (:tuple t) (if (= :unsigned-byte (:internal-type t)) 8 32)))

(defn permanent? [t]
  (or (nil? (:permanent t)) @(:permanent t)))

(defn permanent! [t]
  (dosync (ref-set (:permanent t) true)))

(defn ephemeral! [t]
  (dosync (ref-set (:permanent t) false)))

(defn acquire! [t]
  (dosync (alter (:ref-count t) inc)))

(defn release! [t]
  (dosync (alter (:ref-count t) dec)))

(defn texture? [t]
  (= 'texture-struct (:tag ^t)))

(defn-memo attachment-lookup [point]
  (enum (keyword (str "color-attachment" point))))

(defn attach! [t point]
  (dosync
    (ref-set
      (:attach-point t)
      (if (nil? point)
        nil
        (attachment-lookup point)))))

(defn available? [t]
  (let [permanent (and (not (nil? (:permanent t))) @(:permanent t))
        ref-count (if (nil? (:ref-count t)) 0 @(:ref-count t))]
    (and (not permanent) (>= 0 ref-count))))

;;;;;;;;;;;;;;;;;;;;;;;

(gl-import glBindTexture gl-bind-texture)
(gl-import glGenTextures gl-gen-textures)
(gl-import glTexImage1D gl-tex-image-1d)
(gl-import glTexImage2D gl-tex-image-2d)
(gl-import glTexImage3D gl-tex-image-3d)
(gl-import glTexSubImage1D gl-tex-sub-image-1d)
(gl-import glTexSubImage2D gl-tex-sub-image-2d)
(gl-import glTexSubImage3D gl-tex-sub-image-3d)
(gl-import glTexParameteri tex-parameter)
(gl-import glTexCoord1d gl-tex-coord-1)
(gl-import glTexCoord2d gl-tex-coord-2)
(gl-import glTexCoord3d gl-tex-coord-3)
(gl-import glCopyTexSubImage2D gl-copy-tex-sub-image-2d)
(glu-import gluBuild2DMipmaps glu-build-2d-mipmaps)
(gl-import glDeleteTextures gl-delete-textures)
(gl-import glPixelStorei gl-pixel-store)
(gl-import glGetTexParameteriv gl-get-tex-parameter)

;;;;;;;;;;;;;;;;;;;;;;;;;

(def *texture-pool* nil)
(def *tex-mem-threshold* 100e6)
(def *tex-count-threshold* 100)

(defn- gen-texture []
  (let [a (int-array 1)]
    (gl-gen-textures 1 a 0)
    (nth a 0)))

(defn- separate-textures [textures]
  (separate available? textures))

(defn destroy-textures [textures]
  (if (not (empty? textures))
    (gl-delete-textures (count textures) (int-array (map :id textures)) 0)))

(defn- cleanup-textures []
  (let [[discard keep] (separate-textures @(:textures *texture-pool*))]
    (if (< 0 (count discard))
      (destroy-textures discard))
    (dosync
      (let [textures (alter (:textures *texture-pool*)
                        #(let [[d k] (separate-textures %)]
                          (vec (concat (drop (count discard) d) k))))]
        (ref-set (:texture-size *texture-pool*) (reduce + (map sizeof textures)))))))

(defn- add-texture [t]
  (if *texture-pool*
    (let [textures @(:textures *texture-pool*)
          texture-size @(:texture-size *texture-pool*)]
      (if (or (> (count textures) *tex-count-threshold*)
              (> texture-size *tex-mem-threshold*))
        (cleanup-textures))
      (dosync
        (alter (:texture-size *texture-pool*)
          #(+ % (sizeof t)))
        (alter (:textures *texture-pool*)
          #(cons t %))))))

(defn num-textures []
  (count (filter (complement available?) @(:textures *texture-pool*))))

(defn create-texture [type dim internal-format pixel-format internal-type tuple]
  (let [available   (if *texture-pool* (filter available? @(:textures *texture-pool*)) '())
        equivalent  (filter
                      (fn [t]
                        (=
                          [dim internal-type pixel-format internal-format]
                          (map #(% t) [:dim :internal-type :pixel-format :internal-format])))
                      available)]
    (if (not (empty? equivalent))
      (let [t (first equivalent)]
        (dosync (ref-set (:ref-count t) 1))
        (gl-bind-texture (int (enum (:target t))) (:id t))
        t)
      (let [id    (int (gen-texture))
            typ   (int (enum type))
            p-f   (int (enum pixel-format))
            i-t   (int (enum internal-type))
            i-f   (int (enum internal-format))
            dim   (vec (map int dim))]
        (gl-bind-texture typ id)
        (doseq [p (take (count dim) [:texture-wrap-s :texture-wrap-t :texture-wrap-r])]
          (tex-parameter typ (enum p) :clamp))
        (doseq [p [:texture-min-filter :texture-mag-filter]]
          (tex-parameter typ (enum p) :nearest))
        (condp = (count dim)
          1 (gl-tex-image-1d typ 0 i-f (dim 0) 0 p-f internal-type nil)
          2 (gl-tex-image-2d typ 0 i-f (dim 0) (dim 1) 0 p-f i-t nil)
          3 (gl-tex-image-3d typ 0 i-f (dim 0) (dim 1) (dim 2) 0 p-f i-t nil))
        (let [tex
              (with-meta
                (struct-map texture-struct
                  :target type :id id
                  :dim dim :tuple tuple
                  :internal-type internal-type :internal-format internal-format :pixel-format pixel-format
                  :ref-count (ref 1) :attach-point (ref nil) :permanent (ref false))
                {:tag 'texture-struct})]
          (add-texture tex)
          tex)))))

;;;;;;;;;;;;;;;;;;;;;;

(defmacro get-tex-parameter [dim param]
  `(let [ary# (int-array 1)]
    (gl-get-tex-parameter ~dim ~param ary# 0)
    (get-name (nth ary# 0))))

;;;;;;;;;;;;;;;;;;;;;;;

(defn texture-from-texture-io [tex]
  (with-meta
    (struct-map texture-struct
      :dim [(.getWidth tex) (.getHeight tex)]
      :id (.getTextureObject tex)
      :target :texture-2d
      :pixel-format :rgba
      :internal-format :rgba
      :internal-type :unsigned-byte
      :permanent (ref false)
      :tuple 4
      :ref-count (ref 1))
    {:tag 'texture-struct}))

;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro to-byte [num] `(byte (* 255 (double ~num)))) ;this is a macro for performance reasons

(defn put [#^ByteBuffer buf [r g b a]]
  (doto buf
    (.put (to-byte r))
    (.put (to-byte g))
    (.put (to-byte b))
    (.put (to-byte a))))

(defn populate-buffer [fun tex]
  (let [#^ByteBuffer buf (ByteBuffer/allocate (* 4 (apply * (:dim tex))))
        dim (vec (:dim tex))]
    (condp = (num-dimensions tex)
      1 (dotimes [x (dim 0)]
          (put buf (fun [x] [(/ x (double (dim 0)))])))
      2 (dotimes [x (dim 0)] (dotimes [y (dim 1)]
          (put buf (fun [x y] [(/ x (double (dim 0))) (/ y (double (dim 1)))]))))
      3 (dotimes [x (dim 0)] (dotimes [y (dim 1)] (dotimes [z (dim 2)]
          (put buf (fun [x y z] [(/ x (double (dim 0))) (/ y (double (dim 1))) (/ z (double (dim 2)))]))))))
    (.rewind buf)
    buf))
