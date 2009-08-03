;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.interface.slate
  (:use [clojure.contrib.def :only (defmacro-)])
  (:use [penumbra.opengl core geometry])
  (:import (java.util.concurrent Semaphore))
  (:import (javax.media.opengl
              GLPbuffer GLDrawableFactory GLEventListener
              GLCapabilities GL GLAutoDrawable)))

(defstruct slate-struct :p-buffer :queue)

(defn repaint [s]
  (.repaint #^GLPbuffer (:p-buffer s)))

(defn destruct [s]
  (.destroy #^GLPbuffer (:p-buffer s)))

(defn enqueue [s f]
  (dosync
    (alter (:queue s)
      #(concat % (list f))))
  (repaint s))

(defn execute [s]
  (let [coll @(:queue s)]
    (doseq [f coll]
      (push-matrix (f)))
    (dosync
      (alter (:queue s)
        #(drop (count coll) %)))))

(defn invoke [slate f]
  (let [#^Semaphore s (Semaphore. 1)]
    (.acquire s)
    (enqueue slate (fn [] (f) (.release s)))
    (.acquire s)))

(defn create-slate
  ([size]
    (create-slate
      (-> size Math/sqrt Math/floor int)
      (-> size Math/sqrt Math/ceil int)))
  ([width height]
    (let
      [cap (GLCapabilities.)]

      ;cap stuff goes here

      (let [p-buffer  (.. (GLDrawableFactory/getFactory) (createGLPbuffer cap nil width height nil))
            slt       (struct-map slate-struct :p-buffer p-buffer :queue (ref '()))]

        (doto p-buffer
          (.addGLEventListener
            (proxy [GLEventListener] []

              (display [drawable]
                (bind-gl drawable
                  (execute slt)))

              (displayChanged [drawable mode-change device-changed])

              (reshape [#^GLAutoDrawable drawable x y width height]
                (bind-gl drawable
                  (viewport 0 0 width height)
                  (ortho-view 0 0 width height)))

              (init [#^GLAutoDrawable drawable]
                ))))
        slt))))

(defmacro with-slate [slate & body]
  `(invoke ~slate (fn [] ~@body)))