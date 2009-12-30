(ns penumbra.text
  (:use [clojure.contrib.def :only (defvar defn-memo)])
  (:import [java.awt Font])
  (:import [java.awt.font TextAttribute])
  (:import [org.newdawn.slick TrueTypeFont]))

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



