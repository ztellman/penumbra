(ns penumbra.texture)

(use 'penumbra.opengl)

(set! *warn-on-reflection* true)

(import '(java.nio ByteBuffer)
        '(com.sun.opengl.util BufferUtil))


(defstruct tex-struct :width :height :id)

(gl-import glTexCoord1d gl-tex-1)
(gl-import glTexCoord2d gl-tex-2)
(gl-import glBindTexture gl-bind-texture)
(gl-import glGenTextures gl-gen-textures)
(gl-import glTexParameteri tex-parameter)
(gl-import glTexEnvf tex-env)
(gl-import glTexImage1D tex-image-1d)
(gl-import glTexImage2D tex-image-2d)
(gl-import glPixelStorei gl-pixel-store)

(defn texture
  ([u] (gl-tex-1 u))
  ([u v] (gl-tex-2 u v)))

(defn bind-texture [t]
  (if (= 1 (:height t))
    (gl-bind-texture :texture-1d (:id t))
    (gl-bind-texture :texture-2d (:id t))))

(defn get-byte-buffer [coll]
  (let [buf (BufferUtil/newByteBuffer (count coll))]
    (doseq [b coll] (.put buf (byte b)))
    (.rewind buf)))

(defn gen-texture []
  (let [a (int-array 1)]
    (gl-gen-textures 1 a 0)
    (nth a 0)))

(defn populate-1d-texture [size fun]
  (map 
   #(byte (* 255 %))
   (apply 
    concat 
    (for [x (range size)] 
     (fun x (/ x (float size))))))) 

(defn create-1d-texture [size fun]
  (let [tex-id (gen-texture)
        buf (get-byte-buffer (populate-1d-texture size fun))]
    (gl-bind-texture :texture-1d tex-id)
    (tex-image-1d :texture-2d 0 :rgba size 0 :rgba :unsigned-byte buf)
    (struct-map tex-struct :width size :height 1 :id tex-id)))

(defn populate-2d-texture [w h fun]
  (map
   #(byte (* 255 %))
   (apply
    concat
    (for [x (range w) y (range h)]
      (fun [x y] [(/ x (float w)) (/ y (float h))])))))

(defn create-2d-texture [w h fun]
  (let [tex-id (gen-texture)
        buf (get-byte-buffer (populate-2d-texture w h fun))]
    (gl-bind-texture :texture-2d tex-id)
    (gl-pixel-store :unpack-alignment 1)
    (tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte buf)
    (struct-map tex-struct :width w :height h :id tex-id)))
