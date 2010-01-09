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
  (:require [penumbra.opengl.texture :as texture])
  (:require [penumbra.slate :as slate])
  (:require [penumbra.app.window :as window])
  (:require [penumbra.app.input :as input])
  (:require [penumbra.app.controller :as controller])
  (:require [penumbra.app.loop :as loop])
  (:require [penumbra.app.queue :as queue])
  (:require [penumbra.time :as time])
  (:import [org.lwjgl.opengl Display])
  (:import [org.lwjgl.input Keyboard]))

;;;

(defstruct app-struct
  :window
  :input
  :controller
  :queue
  :clock
  :callbacks
  :state)

(defn- alter-callbacks [clock callbacks]
  (let [callbacks (if-not (:update callbacks)
                    callbacks
                    (update-in callbacks [:update] #(loop/timed-fn clock %)))
        callbacks (if-not (:display callbacks)
                    callbacks
                    (update-in callbacks [:display] #(loop/timed-fn clock %)))]
    callbacks))

(defn create [callbacks state]
  (let [clock (time/clock)]
    (with-meta
      (struct-map app-struct
        :window (window/create)
        :input (input/create)
        :controller (controller/create)
        :queue (atom nil)
        :clock (atom clock)
        :callbacks (alter-callbacks clock callbacks)
        :state (atom state))
      {:type ::app})))

(defmethod print-method ::app [app writer]
  (let [{size :texture-size textures :textures} @(-> app :window :texture-pool)
        active-size (->> textures (remove texture/available?) (map texture/sizeof) (apply +))
        active-percent (float (if (zero? size) 1 (/ active-size size)))
        elapsed-time @@(:clock app)]
    (.write
     writer
     (if (controller/stopped? (:controller app))
       (format "<<%s: STOPPED\n  %.2f seconds elapsed>>"
               (Display/getTitle)
               elapsed-time)
       (format "<<%s: PAUSED\n  %.2f seconds elapsed\n  Allocated texture memory: %.2fM (%.2f%% in use)>>"
               (Display/getTitle)
               elapsed-time
               (/ size 1e6) (* 100 active-percent))))))

;;Clock

(defn clock
  ([]
     (clock *app*))
  ([app]
     (:clock app)))

(defn now
  ([]
     (now *app*))
  ([app]
     @@(clock app)))

;;Input

(defn key-pressed? [key]
  ((-> @(:keys *input*) vals set) key))

(defn key-repeat! [enabled]
  (Keyboard/enableRepeatEvents enabled))

;;Window

(defn title! [title]
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

(defn display-mode!
  "Sets the current display mode."
  ([width height] (window/display-mode! width height))
  ([mode] (window/display-mode! mode)))

(defn dimensions
  "Returns dimensions of window as [width height]."
  ([] (dimensions *window*))
  ([w] (window/dimensions w)))

(defn vsync? []
  (window/vsync?))

(defn vsync! [enabled]
  (window/vsync! enabled))

(defn resizable!
  ([enabled]
     (resizable! *app* enabled))
  ([app enabled]
     (if enabled
       (window/enable-resizable! app)
       (window/disable-resizable! app))))

(defn fullscreen! [enabled]
  (Display/setFullscreen enabled))

;;Updates

(defn frequency!
  "Update frequency of recurring update.  Can only be called from inside recurring-update callback."
  [hz]
  (reset! *hz* hz))

(defn speed!
  ([clock-speed]
     (speed! *app* clock-speed))
  ([app clock-speed]
     (swap! (:clock app) #(time/speed % clock-speed))))

(defn update
  ([f]
     (update *app* f))
  ([app f]
     (queue/update app #(loop/sync-update app f))))

(defn periodic-update
  ([hz f]
     (periodic-update *app* hz f))
  ([app hz f]
     (queue/periodic-update app hz #(loop/sync-update app f))))

;;App state

(defn cleanup!
  "Cleans up any active apps."
  []
  (Display/destroy))

(defn repaint!
  "Forces a new frame to be redrawn"
  []
  (controller/repaint!))

(defn stop!
  "Stops the application."
  ([]
     (stop! *app*))
  ([app]
     (controller/stop! (:controller app))))

(defn pause!
  "Halts main loop, and yields control back to the REPL. Returns nil."
  ([]
     (pause! *app*))
  ([app]
     (controller/pause! (:controller app))))

(defn- resume!
  ([]
     (resume! *app*))
  ([app]
     (loop/with-app app
       (let [stopped? (controller/stopped?)]
         (controller/resume!)
         (input/resume!)
         (when stopped?
            (reset! (:queue app) (queue/create))
            (loop/try-callback :init)
            (loop/try-callback :reshape (concat [0 0] (dimensions))))
         (speed! 1)))))

(defn- destroy
  ([]
     (destroy *app*))
  ([app]
     (loop/try-callback :close)
     (-> app
          (update-in [:input] input/destroy)
          (update-in [:window] window/destroy))))

(defn- init
  [app]
  (if (controller/stopped? (:controller app))
    (assoc app
      :window (window/init (:window app))
      :input (input/init (:input app)))
    app)) 
       
;;;

(defn single-thread-main-loop
  "Does everything in one pass."
  ([]
     (single-thread-main-loop *app*))
  ([app]
     (Display/processMessages)
     (input/handle-keyboard!)
     (input/handle-mouse!)
     (when (window/check-for-resize)
       (repaint!))
     (if (or (Display/isDirty) (controller/invalidated?))
       (do
         (loop/try-callback :update)
         (controller/repainted!)
         (when-let [display (-> app :callbacks :display)]
           (clear 0 0 0)
           (push-matrix
            (display @(:state app)))))
       (Thread/sleep 1))
     (if (Display/isCloseRequested)
       (stop!)
       (Display/update))))

(defn start-single-thread [app]
  (let [app (init app)]
    (loop/primary-loop
     app
     (fn [inner-fn]
       (try
        (resume!)
        (inner-fn)
        (catch Exception e
          (.printStackTrace e)
          (controller/stop!)))
       (speed! 0)
       (if (controller/stopped?)
         (destroy app)
         app))
     single-thread-main-loop)))

(defn start
  "Starts a window from scratch, or from a closed state."
  ([callbacks state]
     (start (create callbacks state)))
  ([app]
     (start-single-thread app)))



