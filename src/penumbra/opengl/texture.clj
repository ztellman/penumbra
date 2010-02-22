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
  (:import (java.nio ByteBuffer FloatBuffer IntBuffer))
  (:import (java.io File))
  (:import (org.newdawn.slick.opengl Texture)))

;;;

(defstruct texture-struct
  :target :id
  :dim :size :tuple :transform
  :internal-type :internal-format :pixel-format
  :permanent :ref-count :attach-point)

(defn num-dimensions [t]
  (count (:dim t)))

(defn sizeof [t]
  (* (apply * (:dim t)) (:tuple t) (if (= :unsigned-byte (:internal-type t)) 1 4)))

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
  (= ::texture (:type (meta t))))

(defmethod print-method ::texture [tex writer]
  (let [[w h] (:dim tex)]
    (.write
     writer
     (format "<<Texture %dx%d %.2fM\n  :id %d refs: %d permanent: %b\n  %s %s %s %s >>"
             (int w) (int h) (float (/ (sizeof tex) 1e6))
             (:id tex) @(:ref-count tex) @(:permanent tex)
             (:target tex) (:internal-format tex) (:internal-type tex) (:pixel-format tex)))))

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

(defn create-texture-pool []
  {:texture-size 0 :textures []})

;;;

(gl-import glBindTexture gl-bind-texture)
(gl-import- glScalef gl-scale-texture)
(gl-import- glGenTextures gl-gen-textures)
(gl-import- glTexImage1D gl-tex-image-1d)
(gl-import- glTexImage2D gl-tex-image-2d)
(gl-import- glTexImage3D gl-tex-image-3d)
(gl-import- glTexSubImage1D gl-tex-sub-image-1d)
(gl-import- glTexSubImage2D gl-tex-sub-image-2d)
(gl-import- glTexSubImage3D gl-tex-sub-image-3d)
(gl-import- glTexParameteri gl-tex-parameter)
(gl-import glTexCoord1d gl-tex-coord-1)
(gl-import glTexCoord2d gl-tex-coord-2)
(gl-import glTexCoord3d gl-tex-coord-3)
(gl-import- glCopyTexSubImage2D gl-copy-tex-sub-image-2d)
(gl-import gluBuild2DMipmaps glu-build-2d-mipmaps)
(gl-import- glDeleteTextures gl-delete-textures)
(gl-import- glPixelStorei gl-pixel-store)
(gl-import- glGetTexParameter gl-get-tex-parameter)

;;;

(defn texture-pool-stats
  ([]
     (texture-pool-stats @*texture-pool*))
  ([texture-pool]
     (let [{size :texture-size textures :textures} texture-pool
           active-size (->> textures (remove available?) (map sizeof) (apply +))
           active-percent (float (if (zero? size) 1 (/ active-size size)))]
       {:count (count textures)
        :size size
        :active-size active-size
        :active-percent active-percent})))

(defn- gen-texture []
  (let [a (int-array 1)]
    (gl-gen-textures (IntBuffer/wrap a))
    (first a)))

(defn- separate-textures [textures]
  (separate available? textures))

(defn destroy-textures [textures]
  (let [ids (set (map :id textures))]
    (when *texture-pool*
      (swap!
       *texture-pool*
       (fn [x] (assoc x
                 :textures (remove #(ids (:id %)) (:textures x))
                 :texture-size (- (:texture-size x) (->> x :textures (map sizeof) (apply +)))))))
    (when (not (empty? textures))
      (gl-delete-textures (IntBuffer/wrap (int-array ids))))))

(defn- cleanup-textures []
  (let [[discard keep] (separate-textures (:textures @*texture-pool*))]
    (when (< 0 (count discard))
      (destroy-textures discard))
    (swap!
     *texture-pool*
     (fn [pool]
       (let [[discard keep] (separate-textures (:textures pool))
             textures (vec (concat (drop (count discard) discard) keep))]
         (assoc pool
           :textures textures
           :texture-size (reduce + (map sizeof textures))))))))

(defn- add-texture [t]
  (when *texture-pool*
    (let [pool @*texture-pool*
          textures (:textures pool)
          texture-size (:texture-size pool)]
      (when (or (> (count textures) *tex-count-threshold*)
                (> texture-size *tex-mem-threshold*))
        (cleanup-textures))
      (swap!
       *texture-pool*
       (fn [pool]
         (assoc pool
           :textures (cons t (:textures pool))
           :texture-size (+ (:texture-size pool) (sizeof t))))))))

(defn num-textures []
  (count (filter (complement available?) (:textures @*texture-pool*))))

(defn create-texture [type dim internal-format pixel-format internal-type tuple]
  (let [available   (if *texture-pool*
                      (filter available? (:textures @*texture-pool*))
                      '())
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
          (gl-tex-parameter typ (enum p) :clamp))
        (doseq [p [:texture-min-filter :texture-mag-filter]]
          (gl-tex-parameter typ (enum p) :nearest))
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
                {:type ::texture})]
          (add-texture tex)
          tex)))))

;;;

(defn texture-from-texture-object
  ([#^Texture tex]
     (texture-from-texture-object tex :linear))
  ([#^Texture tex filter]
     (let [alpha? (.hasAlpha tex)
           dim [(.getImageWidth tex) (.getImageHeight tex)]
           tex-dim [(.getTextureWidth tex) (.getTextureHeight tex)]
           [sx sy] (map / dim tex-dim)
           tex (with-meta
                 (struct-map texture-struct
                   :dim dim
                   :id (.getTextureID tex)
                   :target :texture-2d
                   :transform #(gl-scale-texture sx sy 1)
                   :pixel-format (if alpha? :rgba :rgb)
                   :internal-format (if alpha? :rgba :rgb)
                   :internal-type :unsigned-byte
                   :permanent (ref false)
                   :tuple (if alpha? 4 3)
                   :ref-count (ref 1))
                 {:type ::texture})
           target (enum (:target tex))
           filter (enum filter)]
     (add-texture tex)
     (gl-bind-texture target (:id tex))
     (gl-tex-parameter target :texture-min-filter filter)
     (gl-tex-parameter target :texture-mag-filter filter)
     tex)))

(defn subsampled-texture-from-texture-object
  [#^Texture tex filter]
  (let [texture (texture-from-texture-object tex filter)]
    (glu-build-2d-mipmaps
     :texture-2d
     (enum (:internal-format texture))
     (.getTextureWidth tex)
     (.getTextureHeight tex)
     (enum (:pixel-format texture))
     (enum (:internal-type texture))
     (ByteBuffer/wrap (.getTextureData tex)))
    (gl-tex-parameter :texture-2d :texture-min-filter :linear-mipmap-nearest)
    texture))

;;;

(defmacro get-tex-parameter [target param]
  `(let [ary# (int-array 1)]
    (gl-get-tex-parameter ~target ~param (IntBuffer/wrap ary#))
    (get-name (first ary#))))

;;;

(defmacro to-byte [num] ;;this is a macro for performance reasons
  `(let [n# (int (* 255 (double ~num)))]
     (if (> n# 127)
       (byte (- n# 256))
       (byte n#)))) 

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
      2 (dotimes [x (dim 0)]
          (dotimes [y (dim 1)]
            (put buf (fun [x y] [(/ x (double (dim 0))) (/ y (double (dim 1)))]))))
      3 (dotimes [x (dim 0)]
          (dotimes [y (dim 1)]
            (dotimes [z (dim 2)]
              (put buf (fun [x y z] [(/ x (double (dim 0))) (/ y (double (dim 1))) (/ z (double (dim 2)))]))))))
    (.rewind buf)
    buf))
