;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.text
  (:use [penumbra.opengl]
        [penumbra.opengl.core :only [get-integer *view* *font-cache* *font*]]
        [clojure.contrib.def :only (defvar defn-memo)])
  (:import [java.awt Font]
           [java.awt.font TextAttribute]
           [org.newdawn.slick TrueTypeFont]
           [org.newdawn.slick.opengl TextureImpl]))

(defn-memo text-attribute
  "Takes :keyword and returns TextAttribute/KEYWORD"
  [k]
  (eval `(. TextAttribute ~(-> k name (.replace \- \_) .toUpperCase symbol))))

(defn font [name & modifiers]
  (if-let [font (@*font-cache* (list* name modifiers))]
    font
    (let [hash (apply hash-map modifiers)
	      hash (update-in hash [:size] float)
          hash (assoc hash :family name)
          hash (zipmap (map text-attribute (keys hash)) (vals hash))
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
    (try-with-program nil
      (with-disabled [:texture-rectangle :lighting]
        (with-enabled [:texture-2d :blend]
          (let [blend-dst (get-integer :blend-dst)
		blend-src (get-integer :blend-src)]
	    (blend-func :src-alpha :one-minus-src-alpha)
	    (let [[x-origin y-origin w h] @*view*]
	      (with-projection (ortho-view x-origin (+ x-origin w) (+ y-origin h) y-origin -1 1)
		(push-matrix
		  (load-identity)
		  (TextureImpl/bindNone)
		  (.drawString *font* x y string))))
	    (blend-func blend-src blend-dst)))))))



