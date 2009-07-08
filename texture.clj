(ns penumbra.texture)

(use 'penumbra.opengl)

(set! *warn-on-reflection* true)

(import '(java.nio ByteBuffer))

(defstruct texture :width :height :id) ;type is either :texture-1d or :texture-2d

(gl-import glTexCoord1d tex-1)
(gl-import glTexCoord2d tex-2)
(gl-import glBindTexture gl-bind-texture)
(gl-import glGenTextures gl-gen-textures)
(gl-import glTexParameteri tex-parameter)
(gl-import glTexImage1D tex-image-1d)
(gl-import glTexImage2D tex-image-2d)

(defn tex
  ([u] (tex-1 u))
  ([u v] (tex-2 u v)))

(defn bind-tex [t]
  (if (< 1 (:height t))
    (gl-bind-texture :texture-2d (:id t))
    (gl-bind-texture :texture-1d (:id t))))

(defn byte-array [source]
  (let [array (make-array (. Byte TYPE) (count source))]
    (loop [idx 0 coll source]
      (if (seq coll)
        (do
          (aset array idx (byte (first coll)))
          (recur (inc idx) (rest coll)))))
    array))

(defn gen-texture []
  (let [a (int-array 1)]
    (gl-gen-textures 1 a 0)
    (nth a 0)))

(defn populate-1d-texture [size fun]
  (let [colors (map #(fun % (/ % (float size))) (range size))]
    (apply concat (map (fn [color] (map #((int (* 255 %))) color)) colors))))

(defn create-1d-texture [size fun]
  (let [tex-id (gen-texture)
        buf (ByteBuffer/wrap (populate-1d-texture size fun))]
    (gl-bind-texture :texture-1d tex-id)
    (tex-image-1d :texture-1d 0 4 size 0 :rgba :unsigned-byte buf)
    (struct-map texture :width size :height 1 :id tex-id)))

(defn populate-2d-texture [w h fun]
  (let [colors (map #(apply fun %) (for [x (range w) y (range h)] [[x y] [(/ x (float w)) (/ y (float h))]]))]
    (apply concat (map (fn [color] (map #((int (* 255 %))) color)) colors))))

(defn create-2d-texture [w h fun]
  (let [tex-id (gen-texture)
        buf (ByteBuffer/wrap (populate-2d-texture w h fun))]
    (gl-bind-texture :texture-2d tex-id)
    (tex-image-2d :texture-2d 0 4 w h 0 :rgba :unsigned-byte buf)
    (struct-map texture :width w :height h :id tex-id)))

