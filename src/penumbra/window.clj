;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.window
  (:use [clojure.contrib.def :only (defmacro- defn-memo defvar-)])
  (:use [clojure.contrib.str-utils :only (str-join)])
  (:use [penumbra opengl])
  (:use [penumbra.opengl core])
  (:import [java.lang.reflect Field])
  (:import [java.util.concurrent CountDownLatch])
  (:import [org.lwjgl.input Mouse Keyboard])
  (:import [org.lwjgl.opengl Display])
  (:import [java.awt Frame Canvas GridLayout])
  (:import [java.awt.event WindowAdapter]))

;;;

(defstruct window-struct
  :frame
  :state
  :callbacks
  :texture-pool
  :latch
  :disposed
  :yield)

(defvar- *window* nil
  "Current window.")

(defvar- *invalidated* nil
  "Denotes whether the window should be redrawn")

(defvar- *size* nil
  "Current size of window.  If this doesn't equal frame size, call :reshape.")

(defn- create-frame [window]
  (let [frame (Frame.)
        canvas (Canvas.)]
    (doto canvas
      (.setFocusable true)
      (.setIgnoreRepaint true)
      (.setSize 640 480))
    (doto frame
      (.addWindowListener
       (proxy [WindowAdapter] []
         (windowOpened [event] (.requestFocus canvas))
         (windowClosing [event] (reset! (:disposed window) true))))
      (.setTitle "Penumbra")
      (.setLayout (GridLayout. 1 1))
      (.setSize 640 480)
      (.setResizable true)
      (.setVisible true)
      (.add canvas))
    (Display/setParent canvas)
    frame))

(defn- init-window [callbacks state]
  {:state (atom state)
   :callbacks callbacks
   :texture-pool (atom {:texture-size 0 :textures []})
   :latch (atom nil)
   :disposed (atom false)
   :yield (atom false)})

;;;

(defn- clock [] (System/nanoTime))

(defvar- *hz* nil
  "Refresh rate of periodic function")

(defn set-frequency
  "Update frequency of update-loop.  Can only be called from inside update-loop."
  [hz]
  (reset! *hz* hz))

(defn- periodic-fn
  [hz]
  (let [frequency (atom hz)]
    (fn [f]
      (binding [*hz* frequency]
        (let [start (clock)]
          (f)
          (let [delta (- (clock) start)
                sleep (max 0 (- (/ 1e9 @*hz*) delta))]
            (when-not (zero? sleep)
              (Thread/sleep (long (/ sleep 1e6)) (long (rem sleep 1e6))))))))))

(defn- timed-fn [f]
  (let [last-time (atom (/ (clock) 1e9))]
    (fn [& args]
      (let [current-time (/ (clock) 1e9)]
        (try
         (if f
           (do
             (apply f (list* [(- current-time @last-time) current-time] args)))
           @(:state *window*))
         (finally
          (reset! last-time current-time)))))))

(defn- update-loop
  [latch-fn complete-fn inner-loop]
  (loop []
    (if-let [latch (latch-fn)]
      (.await #^CountDownLatch latch))
    (inner-loop)
    (when-not (complete-fn)
      (recur))))

(defn- secondary-loop [hz f]
  (let [window *window*
        invalidated *invalidated*
        wrapper (periodic-fn hz)]
    (->
     (Thread.
      (fn []
        (binding [*window* window
                  *invalidated* invalidated]
          (Thread/sleep (/ 1000 hz))
          (update-loop
           #(deref (:latch window))
           #(deref (:disposed window))
           #(wrapper f)))))
     .start)))

(defn- primary-loop [window f]
  (let [wrapper (periodic-fn 200)]
    (binding [*window* window
              *invalidated* (atom true)]
      (update-loop
       (constantly nil)
       #(or @(:yield window) @(:disposed window))
       #(wrapper f)))))

;;;

(defmacro- try-call
  [k & args]
  `(if-let [callback# (~k (:callbacks *window*))]
     (let [state# (:state *window*)]
       (when (and
              (not= @state# (swap! state# (fn [x#] (callback# ~@args x#))))
              *invalidated*)
         (reset! *invalidated* true)))))

(defn- shift-down? []
  (or (Keyboard/isKeyDown Keyboard/KEY_LSHIFT)
      (Keyboard/isKeyDown Keyboard/KEY_RSHIFT)))

(defn- current-key []
  (let [desc (Keyboard/getKeyName (Keyboard/getEventKey))
        len (count desc)]
    (cond
     (and (shift-down?) (= 1 len)) (.toUpperCase #^String desc)
     (= 1 len) (.toLowerCase #^String desc)
     :else (keyword (.toLowerCase #^String desc)))))

(defn- handle-keyboard []
  (Keyboard/poll)
  (while
   (Keyboard/next)
   (if (Keyboard/getEventKeyState)
     (try-call :key-press (current-key))
     (do
       (try-call :key-release (current-key))
       (try-call :key-type (current-key))))))

(defn- handle-mouse []
  (loop [buttons (vec (map #(Mouse/isButtonDown %) (range (Mouse/getButtonCount))))]
    (Mouse/poll)
    (let [dw (Mouse/getEventDWheel)
          dx (Mouse/getEventDX), dy (Mouse/getEventDY)
          x (Mouse/getEventX), y (Mouse/getEventY)]
      (when (not (zero? dw))
        (try-call :mouse-wheel dw))
      (if (>= 0 (Mouse/getEventButton))
        (do
          (try-call :mouse-move [[dx dy] [x y]])
          (when (Mouse/next)
            (recur buttons)))
        (let [button-pressed? (Mouse/getEventButtonState)
              button-index (Mouse/getEventButton)
              button (-> button-index Mouse/getButtonName keyword)]
          (when (not= (nth buttons button-index) button-pressed?)
            (if button-pressed?
              (try-call :mouse-down [x y] button)
              (try-call :mouse-up [x y] button)))
          (when (or (not (zero? dx)) (not (zero? dy)))
            (try-call :mouse-drag [[dx dy] [x y]] button))
          (when (Mouse/next)
            (recur (if () (assoc buttons button-index button-pressed?)))))))))

;;;

(defn key-pressed? []
  '())

(defn update-once
  ([]
     (update-once *window*))
  ([window]
     (binding [*window* window]
       (let [invalidated (or *invalidated* (atom true))
             *size* (or *size* (atom [0 0]))]
         (Display/update)
         (handle-keyboard)
         (handle-mouse)
         (let [[w h] [(.. Display getDisplayMode getWidth) (.. Display getDisplayMode getHeight)]]
           (when (or (nil? *size*) (not= @*size* [w h]))
             (when *size*
               (reset! *size* [w h]))
             (viewport 0 0 w h)
             (try-call :reshape [0 0 w h])))
         (try-call :update)
         (when true ;;@*invalidated*
           (reset! *invalidated* false)
           (clear)
           (push-matrix
            ((-> window :callbacks :display) @(:state window))))
         (when (Display/isCloseRequested)
           (reset! (:disposed window) true))))))

(defn repaint []
  (when *invalidated*
    (reset! *invalidated* true)))

(defn pause
  "Halts main loop, and yields control back to the REPL."
  ([]
     (pause *window*))
  ([window]
     (reset! (:yield window) true)
     (reset! (:latch window) (CountDownLatch. 1))))

(defn close
  "Closes the window."
  ([]
     (close *window*))
  ([window]
     (reset! (:disposed window) true)))

(defn- alter-callbacks [window]
  (update-in
   window
   [:callbacks]
   (fn [callbacks]
     (->
      callbacks
      (update-in [:update] #(timed-fn %))
      (update-in [:display] #(timed-fn %))))))

(defn- initialize []
  (reset! (:disposed *window*) false)
  (reset! (:yield *window*) false)
  (when @(:latch *window*)
  (.countDown #^CountDownLatch @(:latch *window*)))
  (Display/create)
  (Keyboard/create)
  (Mouse/create))

(defn- dispose []
  (try-call :close)
  (.dispose (:frame *window*))
  (Mouse/destroy)
  (Keyboard/destroy)
  (Display/destroy))

(defn start
  ([callbacks state]
     (start (init-window callbacks state)))
  ([window]
     (let [window (alter-callbacks (assoc window :frame (create-frame window)))]
       (binding [*window* window
                 *size* (atom [0 0])]
         (initialize)
         (try
          (try-call :init)
          (primary-loop window update-once)
          window
          (finally
           (dispose)))))))



