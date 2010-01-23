;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.text
  (:use [penumbra.opengl]
        [penumbra.opengl.core :only [*view*]]
        [clojure.contrib.def :only (defvar defn-memo)])
  (:import [java.awt Font]
           [java.awt.font TextAttribute]
           [org.newdawn.slick TrueTypeFont]
           [org.newdawn.slick.opengl TextureImpl]))

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
  "Draws string at pixel coordinates (x, y)"
  [string x y]
  (with-font (or *font* (font "Tahoma" :size 20))
    (with-disabled [:texture-rectangle :lighting]
      (with-enabled [:texture-2d :blend]
        (let [[x y w h] @*view*]
          (with-projection (ortho-view x (+ x w) (+ y h) y -1 1)
            (push-matrix
             (load-identity)
             (TextureImpl/bindNone)
             (.drawString *font* x y string))))))))



