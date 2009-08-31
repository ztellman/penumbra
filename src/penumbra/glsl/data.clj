;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.data
  (:use [penumbra.opengl.framebuffer :only (read-format)])
  (:use [penumbra.slate :only (rectangle)])
  (:use [penumbra.opengl.texture :only (create-texture gl-tex-sub-image-2d)])
  (:use [penumbra.opengl.core])
  (:use [penumbra opengl])
  (:import (java.nio Buffer FloatBuffer IntBuffer ByteBuffer))
  (:import (com.sun.opengl.util.texture TextureData)))

;;;;;;;;;;;;;;;;;;;

(defn- array? [a] (.isArray (class a)))
(defn- flat? [s] (not (or (array? s) (sequential? s))))
(defn- or= [cmp & args] (some #(= cmp %) args))

(defn byte-array [size-or-seq]
  (if (number? size-or-seq)
    (make-array Byte/TYPE size-or-seq)
    (let [a (make-array Byte/TYPE (count size-or-seq))]
      (loop [idx 0, s size-or-seq]
        (if (empty? s)
          a
          (do
            (aset a idx (byte (first s)))
            (recur (inc idx) (next s))))))))

(defn- create-array [size-or-seq type]
  (cond
    (= type :int)           (int-array size-or-seq)
    (= type :float)         (float-array size-or-seq)
    (= type :unsigned-byte) (byte-array size-or-seq)
    :else                   (throw (Exception. "Don't recognize type"))))

(defn- array-to-buffer [a type]
  (cond
    (= type :float)         (FloatBuffer/wrap a)
    (= type :int)           (IntBuffer/wrap a)
    (= type :unsigned-byte) (ByteBuffer/wrap a)
    :else                   (throw (Exception. "Don't recognize type"))))

(defn- seq-type [s]
  (let [type (-> s first .getClass)]
    (cond
      (or= type Integer Integer/TYPE) :int
      (or= type Float Float/TYPE)     :float
      (or= type Byte Byte/TYPE)       :unsigned-byte
      :else                           (throw (Exception. "Don't recognize type")))))

;;;;;;;;;;;;;;;;;;;

(defn write-to-texture [tex ary]
  (let [[w h] (:dim tex)]
    (gl-tex-sub-image-2d
      :texture-rectangle
      0 0 0
      (int w) (int h)
      (int (enum (:pixel-format tex)))
      (int (enum (:internal-type tex)))
      #^Buffer (array-to-buffer ary (:internal-type tex))))
    tex)

(defn- seq-to-texture
  ([s] (seq-to-texture s 1))
  ([s tuple]
    (let [type                (seq-type s)
          [internal pixel _]  (read-format type tuple)
          ary                 (if (array? s) s (create-array s type))
          dim                 (rectangle (/ (count ary) tuple))
          tex                 (create-texture :texture-rectangle dim internal pixel type tuple)]
      (write-to-texture tex ary)
      (assoc tex :size (count s)))))

(defn mimic-texture [tex]
  (apply create-texture (map #(% tex) [:target :dim :internal-format :pixel-format :internal-type :tuple])))

(defn wrap
  ([s] (seq-to-texture s))
  ([s tuple] (seq-to-texture s tuple)))

(defn unwrap
  ([tex]
    (unwrap tex (* (apply * (:dim tex)) (:tuple tex))))
  ([tex size]
    (if (nil? (:attach-point tex))
      (throw (Exception. "Cannot read from unattached texture.")))
    (gl-read-buffer @(:attach-point tex))
    (let [[w h] (:dim tex)
          dim   (* w h (:tuple tex))
          a     (create-array size (:internal-type tex))]
      (gl-read-pixels
        0 0 (int w) (int h)
        (int (enum (:pixel-format tex)))
        (int (enum (:internal-type tex)))
        #^Buffer (array-to-buffer a (:internal-type tex)))
       a)))

(defn unwrap-first [tex]
  (if (nil? (:attach-point tex))
    (throw (Exception. "Cannot read from unattached texture.")))
  (gl-read-buffer @(:attach-point tex))
  (let [a (create-array (:tuple tex) (:internal-type tex))]
    (gl-read-pixels
      0 0 1 1
      (int (enum (:pixel-format tex)))
      (int (enum (:internal-type tex)))
      #^Buffer (array-to-buffer a (:internal-type tex)))
     a))


;;;;;;;;;;;;;;;;;;

