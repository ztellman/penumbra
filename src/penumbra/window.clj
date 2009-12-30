;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.window
  (:use [clojure.contrib.def :only (defvar)])
  (:use [clojure.contrib.core :only (-?>)])
  (:use [penumbra.opengl])
  (:use [penumbra.opengl.texture :only (create-texture-pool)])
  (:use [penumbra.opengl.core :only (*texture-pool*)])
  (:require [penumbra.slate :as slate])
  (:require [penumbra.opengl.texture :as texture])
  (:import [org.lwjgl.opengl Display PixelFormat])
  (:import [org.newdawn.slick.opengl InternalTextureLoader])
  (:import [java.awt Frame Canvas GridLayout Color])
  (:import [java.awt.event WindowAdapter]))

;;

(defstruct window-struct
  :texture-pool
  :frame
  :size)

(defvar *window* nil
  "Current window.")

(defvar *callback-handler* nil)

;;Display Mode

(defn- transform-display-mode [m]
  {:resolution [(.getWidth m) (.getHeight m)]
   :bpp (.getBitsPerPixel m)
   :fullscreen (.isFullscreenCapable m)
   :mode m})

(defn display-modes
  "Returns a list of available display modes."
  []
  (map transform-display-mode (Display/getAvailableDisplayModes)))

(defn current-display-mode
  "Returns the current display mode."
  []
  (transform-display-mode (Display/getDisplayMode)))

(defn set-display-mode
  "Sets the current display mode."
  ([width height]
     (->>
      (display-modes)
      (filter #(= [width height] (:resolution %)))
      (sort-by :bpp)
      last
      set-display-mode))
  ([mode]
     (Display/setDisplayMode (:mode mode))
     (apply viewport (concat [0 0] (:resolution mode)))))

(defn dimensions
  "Returns dimensions of window as [width height]."
  ([]
     (dimensions *window*))
  ([window]
     (if-let [canvas (-?> window :frame deref (.getComponent 1))]
       [(.getWidth canvas) (.getHeight canvas)]
       (:resolution (current-display-mode)))))

(defn vsync [enabled]
  (Display/setVSyncEnabled enabled))

(defn check-for-resize
  ([]
     (check-for-resize *window*))
  ([window]
     (let [dim (dimensions window)]
       (when (not= @(:size window) dim)
         (reset! (:size window) dim)
         (*callback-handler* :reshape (concat [0 0] dim))))))

;;

(defn create []
  (struct-map window-struct
    :drawable #(Display/getDrawable)
    :texture-pool (atom (create-texture-pool))
    :frame (atom nil)
    :size (atom [1024 768])))

(defn init
  ([]
     (init *window*))
  ([window]
     (Display/create (-> (PixelFormat.) (.withSamples 4)))
     (Display/setParent nil)
     (apply set-display-mode @(:size window))
     (*callback-handler* :reshape (concat [0 0] @(:size window)))))

(defn destroy
  ([]
     (destroy *window*))
  ([window]
     (-> (InternalTextureLoader/get) .clear)
     (texture/destroy-textures (:textures @*texture-pool*))
     (when-let [f @(:frame window)]
       (.dispose f))
     (Display/destroy)))

(defmacro with-window [window & body]
  `(binding [*window* ~window
             *texture-pool* (:texture-pool ~window)]
     ~@body))

;;Frame

(defn- enable-resizable [window]
  (when (nil? @(:frame window))
    (let [frame (Frame.)
          canvas (Canvas.)
          [w h] @(:size window)]
      (doto canvas
        (.setFocusable true)
        (.setIgnoreRepaint true)
        (.setSize w h))
      (doto frame
        (.addWindowListener
         (proxy [WindowAdapter] []
           (windowOpened [event] (.requestFocus canvas))
           (windowClosing [event] (reset! (:disposed window) true))))
        (.setTitle (Display/getTitle))
        (.setIgnoreRepaint true)
        (.setResizable true)
        (.setVisible true)
        (.add canvas)
        (.pack))
      (Display/setParent canvas)
      (reset! (:frame window) frame))))

(defn- disable-resizable [window]
  (when-let [frame @(:frame window)]
    (.dispose frame)
    (Display/setParent nil)
    (reset! (:frame window) nil)))

(defn resizable
  ([enabled]
     (resizable *window* enabled))
  ([window enabled]
     (if enabled
       (enable-resizable window)
       (disable-resizable window))))

