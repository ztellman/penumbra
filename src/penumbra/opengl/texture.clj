;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.texture
  (:use [clojure.contrib.def :only (defmacro- defn-memo)])
  (:use [clojure.contrib.seq :only (separate)])
  (:use [penumbra.opengl core])
  (:use [penumbra data])
  (:import [org.lwjgl BufferUtils])
  (:import (java.io File))
  (:import (org.newdawn.slick.opengl Texture)))

;;;

(gl-import- glBindTexture gl-bind-texture)
(gl-import- glScalef gl-scale)
(gl-import- glIsTexture gl-is-texture)
(gl-import- glGenTextures gl-gen-textures)
(gl-import- glTexImage1D gl-tex-image-1d)
(gl-import- glTexImage2D gl-tex-image-2d)
(gl-import- glTexImage3D gl-tex-image-3d)
(gl-import- glTexSubImage1D gl-tex-sub-image-1d)
(gl-import- glTexSubImage2D gl-tex-sub-image-2d)
(gl-import- glTexSubImage3D gl-tex-sub-image-3d)
(gl-import- glCopyTexSubImage1D gl-copy-tex-sub-image-1d)
(gl-import- glCopyTexSubImage2D gl-copy-tex-sub-image-2d)
(gl-import- glCopyTexSubImage3D gl-copy-tex-sub-image-3d)
(gl-import- glGetTexImage gl-get-tex-image)
(gl-import- glTexParameteri gl-tex-parameter)
(gl-import- glDeleteTextures gl-delete-textures)
(gl-import- glTexCoord1d gl-tex-coord-1)
(gl-import- glTexCoord2d gl-tex-coord-2)
(gl-import- glTexCoord3d gl-tex-coord-3)
(gl-import- gluBuild2DMipmaps glu-build-2d-mipmaps)
(gl-import- glDeleteTextures gl-delete-textures)
(gl-import- glPixelStorei gl-pixel-store)
(gl-import- glGetTexParameter gl-get-tex-parameter)
(gl-import glMatrixMode gl-matrix-mode)
(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)

;;;

(def internal-formats
  [[:float 1 :luminance32f-arb]
   [:float 2 :luminance-alpha32f-arb]
   [:float 3 :rgb32f-arb]
   [:float 4 :rgba32f-arb]
   [:float 1 :float-r32-nv]
   [:float 2 :float-rg32-nv]
   [:float 3 :float-rgb32-nv]
   [:float 4 :float-rgba32-nv]
   [:float 1 :alpha-float32-ati]
   [:float 1 :intensity-float32-ati]
   [:float 1 :luminance-float32-ati]
   [:float 2 :luminance-alpha-float32-ati]
   [:float 3 :rgb-float32-ati]
   [:float 4 :rgba-float32-ati]
   [:int 1 :luminance32i]
   [:int 2 :luminance-alpha32i]
   [:int 3 :rgb32i]
   [:int 4 :rgba32i]
   [:int 1 :alpha-integer]
   [:int 1 :luminance-integer]
   [:int 2 :luminance-alpha-integer]
   [:int 3 :rgb-integer]
   [:int 4 :rgba-integer]
   [:unsigned-byte 1 :luminance]
   [:unsigned-byte 2 :luminance-alpha]
   [:unsigned-byte 3 :rgb]
   [:unsigned-byte 4 :rgba]
   [:unsigned-byte 2 :depth-component16]
   [:unsigned-byte 3 :depth-component24]
   [:unsigned-byte 4 :depth-component32]])

(def tuple->pixel-format
     {1 :luminance, 2 :luminance-alpha, 3 :rgb, 4 :rgba})

(defn- internal-type-bytes [internal-type]
  (if (= internal-type :unsigned-byte)
    1
    4))

(defn-memo internal-format->tuple [internal-format]
  (->> internal-formats
       (filter #(= internal-format (last %)))
       first
       second))

;;;

(defprotocol Tex
  (target [t])
  (id [t])
  (dim [t])
  (transform [t]))

'(defmethod print-method ::texture [tex writer]
  (let [[w h] (dim tex)]
    (.write
     writer
     (str "Texture: " (int w) "x" (int h)))))

(defn bind-texture [tex]
  (when tex
    (gl-bind-texture (target tex) (id tex))))

;;;

(defn- array? [a] (.isArray (class a)))
(defn- flat? [s] (not (or (array? s) (sequential? s))))
(defn- or= [cmp & args] (some #(= cmp %) args))

(defn- seq-type [s]
  (let [type (-> s first .getClass)]
    (cond
     (or= type Double Double/TYPE)   :double
     (or= type Integer Integer/TYPE) :int
     (or= type Float Float/TYPE)     :float
     (or= type Byte Byte/TYPE)       :unsigned-byte
     :else                           (throw (Exception. "Don't recognize type")))))

(defn- create-array [size-or-seq type]
  (cond
   (array? size-or-seq)    size-or-seq
   (= type :int)           (int-array size-or-seq)
   (= type :float)         (float-array size-or-seq)
   (= type :unsigned-byte) (byte-array size-or-seq)
   (= type :double)        (double-array size-or-seq)
   :else                   (throw (Exception. "Don't recognize type"))))

(defn- denormalize-bytes [s]
  (map
   (fn [n]
     (let [n (int (* 255 n))]
       (if (> n 127)
        (byte (- n 256))
        (byte n))))
   s))

(defn- create-write-array [tex s]
  (condp = (:internal-type (params tex))
    :double (double-array s)
    :float (float-array s)
    :int (int-array s)
    :unsigned-byte (if (= :unsigned-byte (seq-type s))
                     (byte-array s)
                     (byte-array (denormalize-bytes s)))))

(defn- array-to-buffer [a type]
  (println "type" type)
  (cond
   (= type :double)        (-> (BufferUtils/createDoubleBuffer (count a)) (.put a) .rewind)
   (= type :float)         (-> (BufferUtils/createFloatBuffer (count a)) (.put a) .rewind)
   (= type :int)           (-> (BufferUtils/createIntBuffer (count a)) (.put a) .rewind)
   (= type :unsigned-byte) (-> (BufferUtils/createByteBuffer (count a)) (.put a) .rewind)
   :else                   (throw (Exception. (str "Don't recognize type " type)))))

(defn- create-buffer [size type]
  (cond
    (= type :double)        (BufferUtils/createDoubleBuffer size)
    (= type :float)         (BufferUtils/createFloatBuffer size) 
    (= type :int)           (BufferUtils/createIntBuffer size)
    (= type :unsigned-byte) (BufferUtils/createByteBuffer size)
    :else                   (throw (Exception. (str "Don't recognize type " type)))))

(defn- write-to-texture [tex bounds s]
  (bind-texture tex)
  (let [target (target tex)
        params (params tex)
        pixel  (enum (:pixel-format params))
        type   (:internal-type params)
        buf    (array-to-buffer (create-write-array tex s) type)
        type   (enum type)]
    (condp = (count (dim tex))
      1 (let [[x w] bounds]
          (gl-tex-sub-image-1d
	    (int target) 0
	    (int x) (int w)
	    (int pixel) (int type)
	    buf))
      2 (let [[x y w h] bounds]
          (gl-tex-sub-image-2d
	    (int target) 0
	    (int x) (int y) (int w) (int h)
	    (int pixel) (int type)
	    buf))
      3 (let [[x y z w h d] bounds]
          (gl-tex-sub-image-3d
	    (int target) 0
	    (int x) (int y) (int z) (int w) (int h) (int d)
	    (int pixel) (int type)
	    buf)))))

(defn- unwrap-texture
  ([tex]
     (unwrap-texture tex (* (apply * (dim tex)) (->> tex params :internal-format internal-format->tuple))))
  ([tex size]
     (let [params (params tex)
	   buf (create-buffer size (:internal-type params))]
       (bind-texture tex)
       (gl-get-tex-image
        (target tex)
        0
        (enum (:pixel-format params))
        (enum (:internal-type params))
	buf)
       buf)))

;;;

(defn- gen-texture-id []
  (let [buf (BufferUtils/createIntBuffer 1)]
    (gl-gen-textures buf)
    (.get buf 0)))

(defn- gen-texture [params]
  (let [params (->> params
                     (map (fn [[k v]] [k (if (keyword? v) (enum v) v)]))
                     (apply concat)
                     (apply hash-map))
        valid? (and (:id params) (gl-is-texture (:id params)))
        id (if valid?
             (:id params)
             (gen-texture-id))
        dim (vec (map int (:dim params)))
        i-f (:internal-format params)
        p-f (:pixel-format params)
        i-t (:internal-type params)
        target (:target params)]
    (gl-bind-texture (:target params) id)
    (doseq [p (take (count dim) [:texture-wrap-s :texture-wrap-t :texture-wrap-r])]
      (gl-tex-parameter (:target params) (enum p) (params p)))
    (doseq [p [:texture-min-filter :texture-mag-filter]]
      (gl-tex-parameter (:target params) (enum p) (params p)))
    (doseq [p [:texture-compare-mode :texture-compare-func]]
      (when (contains? params p)
        (gl-tex-parameter (:target params) (enum p) (params p))))
    (when-not valid?
      (condp = (count dim)
        1 (gl-tex-image-1d target 0 i-f (dim 0) 0 p-f i-t nil)
        2 (gl-tex-image-2d target 0 i-f (dim 0) (dim 1) 0 p-f i-t nil)
        3 (gl-tex-image-3d target 0 i-f (dim 0) (dim 1) (dim 2) 0 p-f i-t nil)))
    id))

(defn create-texture [& params]
  (let [params    (->> params
		    (apply hash-map)
		    (merge
		      {:target :texture-2d
		       :texture-wrap-s :clamp-to-edge
		       :texture-wrap-t :clamp-to-edge
		       :texture-wrap-r :clamp-to-edge
		       :texture-min-filter :nearest
		       :texture-mag-filter :nearest
		       :internal-format :rgba
		       :pixel-format :rgba
		       :internal-type :unsigned-byte}))
        sig       (map params [:internal-format :pixel-format :internal-format :dim])
        tuple     (internal-format->tuple (:internal-format params))
        sizeof    (* tuple (apply * (:dim params)))
        located   (and *texture-pool* (locate! *texture-pool* sig))
        id        (if located
                    (id located)
                    (gen-texture params))
        refcount  (ref 1)
        permanent (ref nil)
        texture (if located
                  located
                  #^{:type ::texture}
                  (reify
		    Object
		    (toString [this] (str "Texture " (:dim params) " " id))
		    Tex
		    (target [_] (enum (:target params)))
		    (id [_] id)
		    (dim [_] (:dim params))
		    (transform [_] (:transform params))
		    Data
		    (acquire! [_] (dosync (alter refcount inc)))
		    (release! [_] (dosync (alter refcount dec)))
		    (refcount [_] @refcount)
		    (refcount! [_ count] (dosync (ref-set refcount count)))
		    (permanent? [_] @permanent)
		    (permanent! [_ flag] (dosync (ref-set permanent flag)))
		    (signature [_] sig)
		    (params [_] params)
		    (matches? [_ s] (= sig s))
		    (sizeof [_] sizeof)
		    (mimic [_] (apply create-texture (apply concat params)))
		    (mimic [_ dim] (apply create-texture (apply concat (assoc params :dim dim))))
		    (unwrap [this] (unwrap-texture this))
		    (overwrite! [this data]
		      (write-to-texture this (concat (take (count (dim this)) (repeat 0)) (dim this)) data))
		    (overwrite! [this bounds data]
		      (write-to-texture this bounds data))
		    (destroy! [this]
		      (if *texture-pool*
			(remove! *texture-pool* this)
			(destroy! this)))))]
    (when-not located
      (add! *texture-pool* texture))
    texture))

(defn build-mip-map [tex]
  (let [params (params tex)
        pixel (enum (:pixel-format params))
        format (enum (:internal-format params))
        type (enum (:internal-type params))
        [w h] (dim tex)
        target (target tex)]
    (gl-bind-texture target (id tex))
    (glu-build-2d-mipmaps
     target format
     w h
     pixel type
     (array-to-buffer (unwrap tex) (:internal-type params)))))

(defn wrap
  ([s tuple dim & params]
     (let [type                (seq-type s)
           [internal pixel _]  (*read-format* type tuple)]
       (when (or (nil? internal) (nil? pixel))
	 (throw (Exception. (str "Can't wrap sequence of type=" type " and tuple=" tuple))))
       (let [ary (create-array s type)
	     tex (apply create-texture
		   (apply concat
		     (merge
		       {:target :texture-rectangle
			:dim dim
			:internal-format internal
			:pixel-format pixel
			:internal-type type}
		       (apply hash-map params))))]
	 (write-to-texture tex (concat (take (count dim) (repeat 0)) dim) ary)
	 tex))))

;;;

(defn texture-from-texture-object
  ([#^Texture tex]
     (texture-from-texture-object tex :linear))
  ([#^Texture tex filter]
     (let [alpha? (.hasAlpha tex)
           dim [(.getImageWidth tex) (.getImageHeight tex)]
           tex-dim [(.getTextureWidth tex) (.getTextureHeight tex)]
           [sx sy] (map / dim tex-dim)]
       (create-texture
	 :dim tex-dim
	 :id (.getTextureID tex)
	 :target :texture-2d
	 :transform #(gl-scale sx sy 1)
	 :pixel-format (if alpha? :rgba :rgb)
	 :internal-format (if alpha? :rgba :rgb)
	 :internal-type :unsigned-byte
	 :tuple (if alpha? 4 3)
	 :texture-min-filter filter
	 :texture-mag-filter filter))))

;;;

(defn texture
  "Calls glTexCoord*d."
  ([u]
     (if (sequential? u)
       (apply texture u)
       (gl-tex-coord-1 u)))
  ([u v]
     (gl-tex-coord-2 u v))
  ([u v w]
     (gl-tex-coord-3 u v w)))

(defn with-texture-transform [trnsform body]
  (do
    (do
      (gl-matrix-mode :texture)
      (gl-push-matrix)
      (trnsform)
      (gl-matrix-mode :modelview))
    (body)
    (do
      (gl-matrix-mode :texture)
      (gl-pop-matrix)
      (gl-matrix-mode :modelview))))

(defn with-texture
  "Binds a texture struct within the inner scope."
  [t body]
  (with-texture-transform
    #(when (and t (transform t))
       ((transform t)))
    #(do
       (bind-texture t)
       (body))))
