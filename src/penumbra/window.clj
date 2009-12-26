;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.window
  (:use [clojure.contrib.def :only (defmacro- defn-memo defvar-)])
  (:use [clojure.contrib.seq-utils :only (indexed)])  
  (:use [penumbra opengl])
  (:use [penumbra.opengl core])
  (:import [java.lang.reflect Field])
  (:import [java.util.concurrent CountDownLatch])
  (:import [org.lwjgl.input Mouse Keyboard])
  (:import [org.lwjgl.opengl Display AWTGLCanvas])
  (:import [java.awt Frame Canvas GridLayout Color])
  (:import [java.awt.event WindowAdapter]))

;;;

(defstruct window-struct
  :frame
  :state
  :callbacks
  :texture-pool
  :latch
  :disposed
  :yield
  :size)

(defvar- *window* nil
  "Current window.")

(defvar- *invalidated* nil
  "Denotes whether the window should be redrawn")

(defn window-in-frame [window]
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
    (reset! (:frame window) frame)
    frame))

(defn- init-window [callbacks state]
  {:state (atom state)
   :callbacks callbacks
   :texture-pool (atom {:texture-size 0 :textures []})
   :latch (atom nil)
   :disposed (atom true)
   :yield (atom false)
   :frame (atom nil)
   :size (atom [1024 768])})

(defn disposed? [window]
  @(:disposed window))

(defn yield? [window]
  @(:yield window))

(defn latch [window]
  (when (:latch window) @(:latch window)))

(defn frame [window]
  (when (:frame window) @(:frame window)))

(defn canvas [window]
  (if-let [f (frame window)]
    (.getComponent #^Frame f 0)))

;;;

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

(defmacro- try-call
  [k & args]
  `(if-let [callback# (~k (:callbacks *window*))]
     (let [state# (:state *window*)]
       (when (and
              (not= @state# (swap! state# (fn [x#] (callback# ~@args x#))))
              *invalidated*)
         (reset! *invalidated* true)))))

(defn- current-key []
  (let [char (Keyboard/getEventCharacter)
        key (Keyboard/getEventKey)
        name (Keyboard/getKeyName key)]
    (cond
     (= key Keyboard/KEY_DELETE) :delete
     (= key Keyboard/KEY_BACK) :back
     (not= 0 (int char)) (str char)
     :else (-> name .toLowerCase keyword))))

(defn- handle-keyboard []
  (Keyboard/poll)
  (while
   (Keyboard/next)
   (if (Keyboard/getEventKeyState)
     (try-call :key-press (current-key))
     (do
       (try-call :key-release (current-key))
       (try-call :key-type (current-key))))))

(defn- mouse-button-name [button-idx]
  (condp = button-idx
    0 :left
    1 :right
    2 :center
    (keyword (str "button" (inc button-idx)))))

(defn- handle-mouse []
  (loop [buttons (vec (map #(Mouse/isButtonDown %) (range (Mouse/getButtonCount))))]
    (Mouse/poll)
    (when (Mouse/next)
      (let [dw (Mouse/getEventDWheel)
            dx (Mouse/getEventDX), dy (Mouse/getEventDY)
            x (Mouse/getEventX), y (Mouse/getEventY)
            button (Mouse/getEventButton)
            button? (not (neg? button))
            button-state (Mouse/getEventButtonState)]
        (when (not (zero? dw))
          (try-call :mouse-wheel dw))
        (cond
         ;;mouse down/up 
         (and (zero? dx) (zero? dy) button?)
         (try-call (if button-state :mouse-down :mouse-up) [x y] (mouse-button-name button))
         ;;mouse-move
         (and (not-any? identity buttons) (or (not (zero? dx)) (not (zero? dy))))
         (try-call :mouse-move [dx dy] [x y])
         ;;mouse-drag
         :else
         (doseq [button-idx (map first (filter second (indexed buttons)))]
           (try-call :mouse-drag [dx dy] [x y] (mouse-button-name button-idx))))
        (if button?
          (recur (assoc buttons button button-state))
          (recur buttons))))))

;;;

(defn key-pressed? []
  '())

(defn close
  "Closes the window."
  ([]
     (close *window*))
  ([window]
     (reset! (:disposed window) true)))

(defn- check-resize [window]
  (if-let [c #^Canvas (canvas window)]
    (let [size [(.getWidth c) (.getHeight c)]]
      (when (not= size @(:size window))
        (reset! (:size window) size)
        (apply viewport (concat [0 0] size))
        (try-call :reshape (concat [0 0] size))))))

(defn update-once
  ([]
     (update-once *window*))
  ([window]
     (binding [*window* window]
       (let [invalidated (or *invalidated* (atom true))]
         (handle-keyboard)
         (handle-mouse)
         (check-resize window)
         (try-call :update)
         (when true ;;@*invalidated*
           (reset! *invalidated* false)
           (push-matrix
            (clear)
            ((-> window :callbacks :display) @(:state window))))
         (when (Display/isCloseRequested)
           (close window))
         (Display/update)))))

(defn repaint
  "Forces a repaint of the window."
  []
  (when *invalidated*
    (reset! *invalidated* true)))

(defn pause
  "Halts main loop, and yields control back to the REPL."
  ([]
     (pause *window*))
  ([window]
     (reset! (:yield window) true)
     (reset! (:latch window) (CountDownLatch. 1))))

(defn- dispose
  ([]
     (dispose *window*))
  ([window]
     (try-call :close)
     (if-let [f (frame window)]
       (.dispose (:frame window)))
     (Mouse/destroy)
     (Keyboard/destroy)
     (Display/destroy)))

(defn- init
  ([]
     (init *window*))
  ([window]
     (when (disposed? window)
       (Display/setParent nil)
       (Display/create)
       (Keyboard/create)
       (Mouse/create)
       (apply set-display-mode @(:size window))
       (reset! (:disposed window) false))))

(defn resume
  "Resumes a window which has been paused or closed"
  ([]
     (resume *window*))
  ([window]
     (init)
     (reset! (:yield window) false)
     (if-let [latch (latch window)]
       (.countDown #^CountDownLatch latch))
     (try
      (primary-loop window update-once)
      (catch Exception e
        (close window)
        (throw e))
      (finally
       (when (disposed? window)
         (dispose window))))))

(defn- alter-callbacks [window]
  (update-in
   window
   [:callbacks]
   (fn [callbacks]
     (->
      callbacks
      (update-in [:update] #(timed-fn %))
      (update-in [:display] #(timed-fn %))))))

(defn start
  "Starts a window from scratch, or from a closed state."
  ([callbacks state]
     (start (init-window callbacks state)))
  ([window]
     (let [window (alter-callbacks window)]
       (binding [*window* window]
         (init window)
         (try-call :init)
         (try-call :reshape (concat [0 0] @(:size window)))
         (resume window)
         window))))



