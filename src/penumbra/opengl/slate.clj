;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.slate
  (:use [clojure.contrib.def :only [defmacro- defn-memo defvar-]]
        [clojure.contrib.pprint]
        [clojure.contrib.seq :only [separate]]
        [clojure.contrib.core :only [-?>]]
        [penumbra.opengl core]
        [penumbra.app core])
  (:import [org.lwjgl.opengl Pbuffer PixelFormat]))

;;;

(defstruct slate-struct
  :drawable
  :pixel-buffer)

(defvar- *slate* nil)

(defn supported?
  "Checks whether pixel buffers are supported."
  []
  (< 0 (bit-and Pbuffer/PBUFFER_SUPPORTED (Pbuffer/getCapabilities))))

(defn create
  "Creates a slate."
  ([]
     (create nil))
  ([parent]
     (let [drawable (when-let [drawable-fn (-?> *app* :window :drawable)]
                      (drawable-fn))
           pixel-buffer (Pbuffer. 1 1 (-> (PixelFormat.)) drawable)]
       (struct-map slate-struct
         :drawable (constantly pixel-buffer)
         :pixel-buffer pixel-buffer))))

(defn destroy
  "Destroys a slate."
  ([]
     (destroy *slate*))
  ([slate]
     nil))

(defmacro with-slate-
  [slate & body]
  `(do
     (.makeCurrent (:pixel-buffer ~slate))
     (binding [*slate* ~slate]
       (try
        ~@body
        (finally
         (destroy ~slate))))))

(defmacro with-slate
  [& body]
  `(let [slate# (create)]
     (with-slate- slate#
       ~@body)))

;;;




