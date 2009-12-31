;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this
;;   software.

(ns penumbra.app
  (:use [clojure.contrib.core :only (-?>)])
  (:use [clojure.contrib.def :only [defmacro- defvar-]])
  (:use [penumbra.opengl])
  (:use [penumbra.app.core])
  (:use [penumbra.opengl.core :only [*texture-pool*]])
  (:require [penumbra.opengl.texture :as texture])
  (:require [penumbra.slate :as slate])
  (:require [penumbra.app.window :as window])
  (:require [penumbra.app.input :as input])
  (:require [penumbra.app.loop :as loop])
  (:import [org.lwjgl.input Keyboard])
  (:import [org.lwjgl.opengl Display])
  (:import [java.util.concurrent CountDownLatch]))

;;;

(defstruct app-struct
  :window
  :input
  :callbacks
  :state
  :stopped?
  :paused?
  :latch
  :invalidated?)

(defn create [callbacks state]
  (with-meta
    (struct-map app-struct
      :window (window/create)
      :input (input/create)
      :callbacks callbacks
      :state (atom state)
      :stopped? (atom true)
      :paused? (atom false)
      :latch (atom nil)
      :started false
      :invalidated? (atom true))
    {:type ::app}))

(defmethod print-method ::app [app writer]
  (let [{size :texture-size textures :textures} @(-> app :window :texture-pool)
        active-size (->> textures (remove texture/available?) (map texture/sizeof) (apply +))
        active-percent (float (if (zero? size) 1 (/ active-size size)))]
    (.write
     writer
     (if @(:stopped? app)
       (format "<<%s: STOPPED>>" (Display/getTitle))
       (format "<<%s: PAUSED\n  Allocated texture memory: %.2fM (%.2f%% in use)>>"
               (Display/getTitle)
               (/ size 1e6) (* 100 active-percent))))))

;;Input

(defn key-pressed? [key]
  ((-> @(:keys *input*) vals set) key))

(defn key-repeat [enabled]
  (Keyboard/enableRepeatEvents enabled))

;;Window

(defn set-title [title]
  (Display/setTitle title))

(defn display-modes
  "Returns a list of available display modes."
  []
  (map window/transform-display-mode (Display/getAvailableDisplayModes)))

(defn current-display-mode
  "Returns the current display mode."
  []
  (window/transform-display-mode (Display/getDisplayMode)))

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
  ([w]
     (window/dimensions w)))

(defn vsync [enabled]
  (Display/setVSyncEnabled enabled)
  (reset! (:vsync? *window*) enabled))

(defn vsync? []
  @(:vsync? *window*))

(defn resizable
  ([enabled]
     (resizable *app* enabled))
  ([app enabled]
     (if enabled
       (window/enable-resizable app)
       (window/disable-resizable app))))

(defn fullscreen [enabled]
  (Display/setFullscreen enabled))


;;Update loops

(defn clock []
  (/ (System/nanoTime) 1e9))

(defn frequency!
  "Update frequency of update-loop.  Can only be called from inside update-loop."
  [hz]
  (reset! *hz* hz))

;;App state

(defn cleanup
  "Cleans up any active apps."
  []
  (Display/destroy))

(defn repaint
  "Forces a new frame to be redrawn"
  []
  (when *app*
    (reset! (:invalidated? *app*) true)))

(defn stop
  "Stops the application or update-loop, and returns nil."
  ([]
     (if *async-running?*
       (do
         (reset! *async-running?* false)
         nil)
       (stop *app*)))
  ([app]
     (reset! (:stopped? app) true)
     nil))

(defn pause
  "Halts main loop, and yields control back to the REPL."
  ([]
     (pause *app*))
  ([app]
     (reset! (:paused? app) true)
     (reset! (:latch app) (CountDownLatch. 1))
     nil))

(defn pause
  "Halts main loop, and yields control back to the REPL."
  ([]
     (pause *app*))
  ([app]
     (reset! (:paused? app) true)
     (reset! (:latch app) (CountDownLatch. 1))
     nil))

(defn- destroy
  ([]
     (destroy *app*))
  ([app]
     (loop/try-callback :close)
     (input/destroy)
     (window/destroy)))

(defn- init
  ([]
     (init *app*))
  ([app]
     (loop/with-app app
       (when @(:stopped? *app*)
         (window/init)
         (loop/try-callback :init)
         (loop/try-callback :reshape (concat [0 0] (dimensions)))
         (input/init)
         (reset! (:stopped? app) false)))))

(defn update-once
  "Runs through the main loop once."
  ([]
     (update-once *app*))
  ([app]
     (loop/with-app app
       (input/handle-keyboard)
       (input/handle-mouse)
       (window/check-for-resize)
       (loop/try-callback :update)
       (if (or (Display/isDirty) @(:invalidated? app))
         (do
           (reset! (:invalidated? app) false)
           (clear 0 0 0)
           (push-matrix
            ((-> app :callbacks :display) @(:state app)))
           (when (vsync?)
             (Display/sync 60)))
         (Thread/sleep 15))
       (if (Display/isCloseRequested)
         (stop)
         (Display/update)))))

(defn- alter-callbacks [app]
  (if (:started app)
    app
    (->
    app
    (assoc :started true)
    (update-in
     [:callbacks]
     (fn [callbacks]
       (->
        callbacks
        (update-in [:update] #(loop/timed-fn %))
        (update-in [:display] #(loop/timed-fn %)))))
    (with-meta (meta app)))))

(defn start
  "Starts a window from scratch, or from a closed state."
  ([callbacks state]
     (start (create callbacks state)))
  ([app]
     (let [app (alter-callbacks app)]
       (loop/with-app app
         (try
          (init)
          (reset! (:paused? app) false)
          (when-let [latch @(:latch app)]
            (.countDown #^CountDownLatch latch))
          (loop/primary-loop app update-once)
          (catch Exception e
            (reset! (:stopped? app) true)
            (throw e))
          (finally
           (when @(:stopped? app)
             (destroy)))))
       app)))

(defn start-update-loop
  ([hz f]
     (start-update-loop *app* hz f))
  ([app hz f]
     (loop/secondary-loop
      app
      hz
      #(do
         (swap! (:state app) f)
         (repaint)))))



