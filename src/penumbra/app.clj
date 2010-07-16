;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this
;;   software.

(ns penumbra.app
  (:use [clojure.contrib.core :only (-?>)]
        [clojure.contrib.def :only [defmacro- defvar-]]
        [penumbra.opengl]
        [penumbra.opengl.core]
        [clojure.walk :only (postwalk-replace)])
  (:require [penumbra.opengl
             [texture :as texture]
             [context :as context]
             [slate :as slate]]
            [penumbra
             [time :as time]]
            [penumbra.app
             [core :as app]
             [window :as window]
             [input :as input]
             [controller :as controller]
             [loop :as loop]
             [event :as event]
             [queue :as queue]]))

;;;

(defn- transform-extend-arglists [protocol name arglists template]
  (list*
   `fn
   (map
    (fn [args]
      (let [template (postwalk-replace {'this (first args)} template)]
        (list
         (vec args)
         (list*
          (intern (symbol (namespace protocol)) name)
          template
          (next args)))))
    arglists)))

(defmacro- auto-extend
  "Lets the application, which contains an implementation of a protocol, automatically extend that protocol."
  [type protocol template & explicit]
  (let [sigs (eval `(vals (:sigs ~protocol)))]
    (list
     `extend
     type
     protocol
     (merge
      (->> (map
            (fn [{name :name arglists :arglists}]
              [(keyword name)
               (transform-extend-arglists protocol name arglists template)])
            sigs)
           (apply concat)
           (apply hash-map))
      (apply hash-map explicit)))))

;;;

(defn- update-
  "Updates the state of the application."
  [app state f args]
  (swap! state
        (fn [state]
          (let [state* (if (empty? args)
                         (f state)
                         (apply f (concat args [state])))]
            (when state*
              (controller/invalidated! app true))
            (or state* state)))))

(defn- alter-callbacks
  "Updates the update and display callbacks using timed-fn"
  [clock callbacks]
  (let [wrap (partial loop/timed-fn clock)
        callbacks (if (:update callbacks)
                    (update-in callbacks [:update] wrap)
                    callbacks)
        callbacks (if (:display callbacks)
                    (update-in callbacks [:display] wrap)
                    callbacks)]
    callbacks))

(defrecord App
  [state
   clock
   event-handler
   queue
   window
   input-handler
   controller
   parent]
  clojure.lang.IDeref
  (deref [this] @(:state this)))

(auto-extend App penumbra.app.window/Window (deref (:window this)))
(auto-extend App penumbra.app.input/InputHandler (deref (:input-handler this)))
(auto-extend App penumbra.app.queue/QueueHash (deref (:queue this)))
(auto-extend App penumbra.app.event/EventHandler (:event-handler this))
(auto-extend App penumbra.app.controller/Controller (:controller this))

(extend
 App
 app/App
 {:speed! (fn [app speed] (time/speed! (:clock app) speed))
  :now (fn [app] @(:clock app))
  :callback- (fn [app event args] ((-> app :callbacks event) args))
  :init! (fn [app]
           (window/init! app)
           (input/init! app)
           (queue/init! app)
           (controller/resume! app)
           (event/publish! app :init))
  :destroy! (fn [app]
              (event/publish! app :close)
              (controller/stop! app)
              (when-not (:parent app)
                (window/destroy! app)
                (input/destroy! app)))})

(defmethod print-method penumbra.app.App [app writer]
  (.write writer "App"))

(defn create
  "Creates an application."
  [callbacks state]
  (let [window (atom nil)
        input (atom nil)
        queue (atom nil)
        event (event/create)
        clock (time/clock)
        state (atom state)
        controller (controller/create)
        app (App. state clock event queue window input controller app/*app*)]
    (reset! window (window/create-fixed-window app))
    (reset! input (input/create app))
    (reset! queue (queue/create app))
    (doseq [[event f] (alter-callbacks clock callbacks)]
      (if (= event :display)
        (event/subscribe! app :display (fn [& args] (f @state)))
        (event/subscribe! app event (fn [& args] (update- app state f args)))))
    app))

;;;

(defn app
  "Returns the current application."
  []
  app/*app*)

(defn- transform-import-arglists [protocol name doc arglists]
  (list*
   `defn name
   doc
   (map
    (fn [args]
      (list
       (vec (next args))
       (list*
        (intern (symbol (namespace protocol)) name)
        'penumbra.app.core/*app*
        (next args))))
    arglists)))

(defmacro- auto-import
  "Creates an function which automatically fills in app with *app*"
  [protocol & imports]
  (let [sigs (eval `(vals (:sigs ~protocol)))]
    (list*
     'do
     (map
      (fn [{name :name arglists :arglists doc :doc}]
        (transform-import-arglists protocol name doc arglists))
      (let [imports (set imports)]
        (filter #(imports (:name %)) sigs))))))

(auto-import penumbra.app.window/Window
             title! size fullscreen! vsync! display-mode! display-modes)

(auto-import penumbra.app.controller/Controller
             stop! pause!)

(auto-import penumbra.app.input/InputHandler
             key-pressed? button-pressed? key-repeat! mouse-location)

;;

(defn clock
  "Returns the application clock."
  ([] (clock app/*app*))
  ([app] (:clock app)))

(defn now
  "Returns the elapsed clock time, in seconds, since the application began."
  ([] (now app/*app*))
  ([app] @(clock app)))

(defn speed!
  "Sets the application clock speed."
  ([speed] (speed! app/*app* speed))
  ([app speed] (time/speed! (clock app) speed)))

(defn periodic-update!
  "Starts a recurring update, which is called 'hz' times a second.
   Time is governed by 'clock', which defaults to the application clock.

   OpenGL calls cannot be made within this callback."
  ([hz f] (periodic-update! (clock)  hz f))
  ([clock hz f] (periodic-update! app/*app* clock hz f))
  ([app clock hz f] (queue/periodic-enqueue! app clock hz #(update- app (:state app) f nil))))

(defn delay!
  "Enqueues an update to be executed in 'delay' milliseconds.
   Time is goverend by 'clock', which defaults to the application clock.

   OpenGL calls cannot be made within this callback."
  ([delay f] (delay! (clock) delay f))
  ([clock delay f] (delay! app/*app* clock delay f))
  ([app clock delay f] (queue/enqueue! app clock delay #(update- app (:state app) f nil))))

(defn update!
  "Enqueues an update to happen immediately.

   OpenGL calls cannot be made within the callback."
  ([f] (update! app/*app* f))
  ([app f] (delay! (clock app) 0 f)))

(defn enqueue!
  "Enqueues an update to happen before the next frame is rendered.

   OpenGL calls in this callback are okay."
  ([f] (enqueue! app/*app* f))
  ([app f] (event/subscribe-once! app :enqueue #(update- app (:state app) f nil))))

(defn repaint!
  "Forces the application to repaint."
  ([] (repaint! app/*app*))
  ([app] (controller/invalidated! app true)))

(defn frequency! [hz]
  "Updates the update frequency of a periodic update.

   This can only be called from within the periodic callback.  A frequency of 0 or less will halt the update."
  (reset! app/*hz* hz))

;;

(defmacro with-gl
  "Creates a valid OpenGL context within the scope."
  [& body]
  `(slate/with-slate
    (context/with-context nil
      ~@body)))

;;App state

(defn single-thread-main-loop
  "Does everything in one pass."
  ([app]
     (window/process! app)
     (input/handle-keyboard! app)
     (input/handle-mouse! app)
     (window/handle-resize! app)
     (if (or (window/invalidated? app) (controller/invalidated? app))
       (do
         (event/publish! app :enqueue)
         (event/publish! app :update)
         (controller/invalidated! app false)
         (push-matrix
          (clear 0 0 0)
          (event/publish! app :display))
	 (Thread/sleep 1)
         (window/update! app))
       (Thread/sleep 1))
     (if (window/close? app)
       (controller/stop! app :requested-by-user))))

(defn start-single-thread
  [app loop-fn]
  (context/with-context nil
    (loop-fn
     app
     (fn [inner-fn]
       (app/speed! app 0)
       (app/init! app)
       (app/speed! app 1)
       (inner-fn)
       (app/speed! app 0))
     (partial single-thread-main-loop app))
    (when (controller/stopped? app)
      (app/destroy! app)))
  app)

(defn start
  "Starts a window from scratch, or from a closed state.
   Supported callbacks are: 
   :update         [[delta time] state]
   :display        [[delta time] state]
   :reshape        [[x y width height] state]
   :init           [state]
   :close          [state]
   :mouse-drag     [[[dx dy] [x y]] button state]
   :mouse-move     [[[dx dy] [x y]] state]
   :mouse-up       [[x y] button state]
   :mouse-click    [[x y] button state]
   :mouse-down     [[x y] button state]
   :key-type       [key state]
   :key-press      [key state]
   :key-release    [key state]"
  ([callbacks state]
     (start-single-thread (create callbacks state) loop/basic-loop)))


