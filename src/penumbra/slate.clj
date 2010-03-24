;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.slate
  (:use [clojure.contrib.def :only [defmacro- defn-memo defvar-]]
        [clojure.contrib.pprint]
        [clojure.contrib.seq-utils :only [separate]]
        [clojure.contrib.core :only [-?>]]
        [penumbra opengl]
        [penumbra.opengl core]
        [penumbra.app core])
  (:require [penumbra.opengl
             [texture :as texture]
             [context :as context]])
  (:import [org.lwjgl.opengl Pbuffer PixelFormat]))

;;;

(defstruct slate-struct
  :drawable
  :pixel-buffer)

(defvar- *slate* nil)

(defn create
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
  ([]
     (destroy *slate*))
  ([slate]
     (when (:base-context? slate)
       (destroy-frame-buffer (:frame-buffer slate))
       (texture/destroy-textures (-> *texture-pool* deref :textures)))
     '(.destroy (:pixel-buffer slate))))

(defmacro with-slate [slate & body]
  `(let [context# (context/current)]
     (.makeCurrent (:pixel-buffer ~slate))
     (with-frame-buffer
       (context/with-context context#
         (binding [*slate* ~slate]
           (try
            ~@body
            (finally
             (destroy ~slate)
             (when context#
               (context/destroy)))))))))

(defmacro with-blank-slate [& body]
  `(with-slate (create)
     ~@body))

;;;




