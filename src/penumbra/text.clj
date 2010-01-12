(ns penumbra.text
  (:use [penumbra.opengl])
  (:use [penumbra.opengl.core :only [*view*]])
  (:use [clojure.contrib.def :only (defvar defn-memo)])
  (:import [java.awt Font])
  (:import [java.awt.font TextAttribute])
  (:import [org.newdawn.slick TrueTypeFont])
  (:import [org.newdawn.slick.opengl TextureImpl]))

(defvar *font-cache* nil
  "Where all the fonts are kept")

(defvar *font* nil
  "Current font")

(defn-memo attribute
  "Takes :keyword and returns TextAttribute/KEYWORD"
  [k]
  (eval `(. TextAttribute ~(-> k name (.replace \- \_) .toUpperCase symbol))))

(defn font [name & modifiers]
  (if-let [font (@*font-cache* (list* name modifiers))]
    font
    (let [hash (apply hash-map modifiers)
          hash (assoc hash :family name)
          hash (zipmap (map attribute (keys hash)) (vals hash))
          font (TrueTypeFont. (Font. hash) true)]
      (swap! *font-cache* #(assoc % (list* name modifiers) font))
      font)))

(defmacro with-font [f & body]
  `(binding [*font* ~f]
     ~@body))

(defn write-to-screen
  "writes string at pixel coordinates (x, y)"
  [string x y]
  (with-font (font "Tahoma" :size 20)
    (with-disabled [:texture-rectangle :lighting]
      (with-enabled [:texture-2d :blend]
        (let [[x y w h] @*view*]
          (with-projection (ortho-view x (+ x w) (+ y h) y -1 1)
            (push-matrix
             (load-identity)
             (TextureImpl/bindNone)
             (.drawString *font* x y string))))))))



