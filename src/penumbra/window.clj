;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.window
  (:use [clojure.contrib.def :only (defmacro- defn-memo)])
  (:use [clojure.contrib.str-utils :only (str-join)])
  (:use [penumbra opengl])
  (:use [penumbra.opengl core])
  (:use [penumbra.opengl.texture :only (*texture-pool*)])
  (:import (java.lang.reflect Field))
  (:import (java.awt
              Frame Dimension
              GraphicsDevice GraphicsEnvironment))
  (:import (java.awt.event
              MouseAdapter MouseListener MouseEvent
              MouseMotionListener MouseMotionAdapter
              MouseWheelListener MouseWheelEvent
              WindowListener WindowAdapter
              KeyAdapter KeyListener KeyEvent))
  (:import (javax.media.opengl
              GLEventListener GLCapabilities GLCapabilitiesChooser
              GLAutoDrawable GLProfile))
  (:import (javax.media.opengl.awt GLCanvas)))

;;;;;;;;;;;;;;;;;;;;;

(defn- clock [] (System/nanoTime))

(defstruct window-struct :canvas :frame :state :callbacks)

(def *window* nil)

;;;;;;;;;;;;;;;;;;;;;

(defstruct window-struct :canvas :frame :state :callbacks :texture-pool :keys :queue)

(defn get-canvas [] #^GLCanvas (:canvas *window*))

(defn get-canvas-size [] [(.getWidth (get-canvas)) (.getHeight (get-canvas))])
(defn set-canvas-size [w h] (.setSize (get-canvas) (Dimension. w h)))

(defn repaint [] (.repaint (get-canvas)))

(defn get-frame [] #^Frame (:frame *window*))
(defn set-title [title] (.setTitle (get-frame) title))

(defn get-state [] (:state *window*))

(defn- get-callbacks [] (:callbacks *window*))

(defn key-pressed? [key] (@(:keys *window*) key))

(defn get-queue [] (:queue *window*))

(defn enqueue [f]
  (alter (get-queue) #(conj % f))
  (repaint))

(defn execute-queue []
  (when (pos? (count @(get-queue)))
    (dosync
     (alter (get-state) (fn [state] (reduce #(%2 %1) state @(get-queue))))
     (ref-set (get-queue) []))))

;;;

(defmacro- try-call
  "If callback exists, calls it and updates state ref."
  [window k & args]
  `(binding [*window* ~window, *texture-pool* (:texture-pool ~window)]
    (if (~k (get-callbacks))
      (dosync
       (if (not=
              @(get-state)
              (alter (get-state) (fn [state#] ((~k (get-callbacks)) ~@args state#))))
        (repaint))))))

(defmacro- mouse-motion
  "Calculates delta from previous mouse position, and invokes try-call."
  [window last-pos event k & args]
  `(let [x# (. ~event getX), y# (. ~event getY)
         [last-x# last-y#] (deref ~last-pos)
         delta# [(- x# last-x#) (- y# last-y#)]]
    (dosync (ref-set ~last-pos [x# y#]))
    (try-call ~window
      ~k [delta# [x# y#]] ~@args)))

(defn-memo get-keycode-keyword
  "Walks through VK_* constants in KeyEvent, and converts the matching value into a keyword."
  [#^KeyEvent event keycode]
  (let [fields (seq (-> event .getClass .getFields))
        name (.getName #^Field (some #(if (= keycode (.get #^Field % event)) % nil) fields))]
    (keyword (.toLowerCase (str-join "-" (next (.split name "_")))))))

(defn- get-key
  "Returns friendlier representation of KeyEvent.
  Alphanumeric values will return as a string, special characters as a keyword.
  a -> 'a', caps lock -> :caps-lock"
  [#^KeyEvent event]
  (let [key (.getKeyCode event)
        char (.getKeyChar event)]
    (cond
      (= key KeyEvent/VK_ESCAPE) :escape
      (= key KeyEvent/VK_TAB) :tab
      (= key KeyEvent/VK_ENTER) :enter
      (not= char KeyEvent/CHAR_UNDEFINED) (str char)
      :else (get-keycode-keyword event key))))

(defn- get-button [#^MouseEvent event]
  (let [button (.getButton event)]
    (cond
      (= button MouseEvent/NOBUTTON) :none
      (= button MouseEvent/BUTTON1) :left
      (= button MouseEvent/BUTTON2) :right
      (= button MouseEvent/BUTTON3) :center)))

;;;

(def *period* nil)

(defn set-frequency
  "Sets the frequency of the update loop surrounding the call, in hertz."
  [hertz]
  (dosync (ref-set *period* (/ 1e9 hertz))))

(defn get-frequency []
  (/ 1e9 @*period*))

(defn- start-update-loop-
  [hertz f executor]
  (let [window *window*
        state  (get-state)
        period (ref (/ 1e9 hertz))
        run (atom true)
        thread (Thread.
                (fn []
                  (try
                   (Thread/sleep (/ 1000 hertz))
                   (binding [*window* window, *period* period]
                     (loop []
                       (let [time (clock)]
                         (executor state)
                         (repaint)
                         (let [diff  (- (clock) time)
                               sleep (max 0 (- @period diff))]
                           (Thread/sleep (long (/ sleep 1e6)) (long (rem sleep 1e6)))
                           (if @run (recur))))))
                   (catch Exception e
                     (println "Error in update loop:\n" (with-out-str (.printStackTrace e)))))))]
    (doto (:canvas window)
      (.addGLEventListener
       (proxy [GLEventListener] []
         (display [_])
         (reshape [_ _ _ _ _])
         (init [_])
         (dispose [_] (reset! run false)))))
    (.start thread)
    thread))

(defn start-update-loop
  "Creates an update loop on a separate thread that repeats 'hertz' times a second.
  Assumes that callback will execute in less than the update period."
  [hertz f]
  (start-update-loop- hertz f (fn [s] (dosync (alter s f)))))

(defn start-update-loop*
  "Same as start-upate-loop, but passes in the ref to the state rather than just the state.
  Only use this if you're sure you know what you're doing."
  [hertz f]
  (start-update-loop- hertz f (fn [s] (f s))))

;;;

(defn vsync-enabled
  "Toggles vertical sync.  Generally will clamp frame rate to <=60fps."
  [enabled]
  (. *gl* setSwapInterval (if enabled 1 0)))

(defn key-repeat
  "If enabled, allows auto-repeat of key presses."
  [enabled]
  (reset! (:repeat-enabled *window*) enabled))

;;;

(defn start
  "Creates a window.  Supported callbacks are:
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
  [callbacks initial-state]
  (let
      [frame (new Frame)
       profile (GLProfile/get GLProfile/GL2)
       cap (new GLCapabilities profile)
       state (ref initial-state)
       texture-pool  {:texture-size (ref 0) :textures (ref '())}]

    '(doto cap
      (.setSampleBuffers true)
      (.setNumSamples 4))

    (let [canvas (GLCanvas. cap)
          last-render (ref (clock))
          last-pos (ref [0 0])
          repeat-enabled (atom false)
          keys (atom #{})
          queue (ref [])
          window (struct-map window-struct
                    :canvas canvas
                    :frame frame
                    :state state
                    :callbacks callbacks
                    :texture-pool texture-pool
                    :keys keys
                    :repeat-enabled repeat-enabled
                    :queue queue)]

      (doto canvas
        (.requestFocus)
        (.addGLEventListener
          (proxy [GLEventListener] []

            (display
             [#^GLAutoDrawable drawable]
              (let [current (clock)
                    delta (/ (- current @last-render) 1e9)
                    time [delta (/ current 1e9)]]
                (dosync (ref-set last-render current))
                (bind-gl drawable
                  (clear)
                  (push-matrix
                    (binding [*window* window, *texture-pool* texture-pool]
                      (execute-queue)
                      (if (:update callbacks)
                        (dosync (alter state #((:update callbacks) time %))))
                      (if (:display callbacks)
                        ((:display callbacks) time @state)))))))

            (reshape
             [#^GLAutoDrawable drawable x y width height]
             (bind-gl drawable
               (viewport 0 0 width height)
               (try-call window
                 :reshape [x y width height])))

            (init
             [#^GLAutoDrawable drawable]
             (bind-gl drawable
               (init-text)
               (try-call window
                 :init)))

            (dispose
             [#^GLAutoDrawable drawable]
              )))

        (.addMouseListener
          (proxy [MouseAdapter] []

            (mouseClicked
             [#^MouseEvent event]
             (try-call window
              :mouse-click [(.getX event) (.getY event)] (get-button event)))

            (mousePressed
             [#^MouseEvent event]
             (try-call window
              :mouse-down [(.getX event) (.getY event)] (get-button event)))

            (mouseReleased
             [#^MouseEvent event]
             (try-call window
              :mouse-up [(.getX event) (.getY event)] (get-button event)))))

        (.addMouseMotionListener
          (proxy [MouseMotionAdapter] []

            (mouseDragged
             [#^MouseEvent event]
             (mouse-motion window last-pos
                event :mouse-drag (get-button event)))

            (mouseMoved
             [#^MouseEvent event]
             (mouse-motion window last-pos
                event :mouse-move))))

        (.addMouseWheelListener
          (proxy [MouseWheelListener] []

            (mouseWheelMoved
             [#^MouseWheelEvent event]
             (try-call window
               :mouse-wheel (.getWheelRotation event)))))

        (.addKeyListener
          (proxy [KeyListener] []

            (keyTyped
             [#^KeyEvent event]
             (try-call window
               :key-type (get-key event)))

            (keyPressed
             [#^KeyEvent event]
              (let [key (get-key event)]
                (when (or @repeat-enabled (not (@keys key)))
                  (swap! keys #(conj % key))
                  (try-call window
                      :key-press key))))

            (keyReleased
             [#^KeyEvent event]
             (let [key (get-key event)
                   timestamp (.getWhen event)]
               (swap! keys #(disj % key))
               (try-call window
                 :key-release key))))))

      (doto frame
        (.addWindowListener
            (proxy [WindowAdapter] []
              (windowOpened [event]
                (.requestFocus canvas))
              (windowClosing [event]
                (try-call window
                  :close)         
                (.start (new Thread #(.dispose frame))))))
        (.add canvas)
        (.setSize 640 480)
        (.setFocusable true)
        (.show)))))
