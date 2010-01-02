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
  (:require [penumbra.app.clock :as clock])
  (:import [org.lwjgl.input Keyboard])
  (:import [org.lwjgl.opengl Display])
  (:import [java.util.concurrent CountDownLatch]))

;;;

(defstruct app-struct
  :window
  :input
  :controller
  :clock
  :callbacks
  :callbacks-altered?
  :state)

(defn create [callbacks state]
  (with-meta
    (struct-map app-struct
      :window (window/create)
      :input (input/create)
      :controller (loop/create-controller)
      :clock (atom (clock/create))
      :callbacks callbacks
      :callbacks-altered? false
      :state (atom state))
    {:type ::app}))

(defmethod print-method ::app [app writer]
  (let [{size :texture-size textures :textures} @(-> app :window :texture-pool)
        active-size (->> textures (remove texture/available?) (map texture/sizeof) (apply +))
        active-percent (float (if (zero? size) 1 (/ active-size size)))]
    (.write
     writer
     (if (loop/stopped? (:controller app))
       (format "<<%s: STOPPED>>" (Display/getTitle))
       (format "<<%s: PAUSED\n  Allocated texture memory: %.2fM (%.2f%% in use)>>"
               (Display/getTitle)
               (/ size 1e6) (* 100 active-percent))))))

;;;

(defn repaint []
  (loop/repaint))

(defn try-callback [callback & args]
  (when-let [f (callback (:callbacks *app*))]
    (let [state @(:state *app*)
          new-state (swap!
                     (:state *app*)
                     (fn [s]
                       (or (if (empty? args)
                             (f s)
                             (apply f (concat args [s])))
                        state)))]
      (when-not (identical? state new-state)
        (repaint)))))

(defmacro with-app [app & body]
  `(binding [*app* ~app
             *clock* (:clock ~app)
             *callback-handler* try-callback]
     (input/with-input (:input ~app)
       (window/with-window (:window ~app)
         (loop/with-controller (:controller ~app)
           ~@body)))))

;;Clock

(defn clock
  ([]
     (clock *app*))
  ([app]
     (if app
       (clock/now (:clock app))
       (clock/real-time))))


;;Input

(defn key-pressed? [key]
  ((-> @(:keys *input*) vals set) key))

(defn key-repeat [enabled]
  (Keyboard/enableRepeatEvents enabled))

;;Window

(defn set-title [title]
  (when @(:frame *window*)
    (.setTitle @(:frame *window*) title))
  (Display/setTitle title))

(defn display-modes
  "Returns a list of available display modes."
  []
  (window/display-modes))

(defn current-display-mode
  "Returns the current display mode."
  []
  (window/current-display-mode))

(defn set-display-mode
  "Sets the current display mode."
  ([width height] (window/set-display-mode width height))
  ([mode] (window/set-display-mode mode)))

(defn dimensions
  "Returns dimensions of window as [width height]."
  ([] (dimensions *window*))
  ([w] (window/dimensions w)))

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
  (loop/repaint))

(defn stop
  "Stops the application or update-loop, and returns nil."
  ([]
     (stop *app*))
  ([app]
     (loop/stop (:controller app))
     nil))

(defn pause
  "Halts main loop, and yields control back to the REPL. Returns nil."
  ([]
     (pause *app*))
  ([app]
     (loop/pause (:controller app))
     nil))

(defn- destroy
  ([]
     (destroy *app*))
  ([app]
     (try-callback :close)
     (input/destroy)
     (window/destroy)))

(defn- init
  ([]
     (init *app*))
  ([app]
     (with-app app
       (when (loop/stopped?)
         (window/init)
         (try-callback :init)
         (try-callback :reshape (concat [0 0] (dimensions)))
         (input/init)))))

(defn update-once
  "Runs through the main loop once."
  ([]
     (update-once *app*))
  ([app]
     (with-app app
       (Display/processMessages)
       (input/handle-keyboard)
       (input/handle-mouse)
       (when (window/check-for-resize)
         (repaint))
       (if (or (Display/isDirty) (loop/invalidated?))
         (do
           (try-callback :update)
           (loop/repainted)
           (clear 0 0 0)
           (push-matrix
            ((-> app :callbacks :display) @(:state app))))
         (Thread/sleep 15))
       (if (Display/isCloseRequested)
         (stop)
         (Display/update)))))

(defn- alter-callbacks [app]
  (if (:altered? app)
    app
    (->
    app
    (assoc :altered? true)
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
     (with-app app
       (let [app (alter-callbacks app)]
         (with-app app
           (try
            (clock/start (:clock app))
            (init)
            (loop/resume)
            (loop/primary-loop (fn [x] (x)) update-once)
            (catch Exception e
              (loop/stop)
              (throw e))
            (finally
             (when (loop/stopped?)
               (destroy)))))
         (clock/stop (:clock app))
         app))))

(defn start-update-loop
  ([hz f]
     (start-update-loop *app* hz f))
  ([app hz f]
     (loop/secondary-loop
      hz
      #(with-app app
         (let [s (slate/create)]
           (try
            (%)
            (finally
             (slate/destroy s)))))
      #(do
         (swap! (:state app) f)
         (repaint)))))



