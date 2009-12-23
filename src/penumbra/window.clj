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
  (:import [org.lwjgl.input Mouse Keyboard]))

;;;

(defstruct window-struct :state :callbacks :texture-pool :latch :disposed :yield)

(defvar- *window* nil)

(defn- init-window [callback state]
  {:state state
   :callback callback
   :texture-pool (atom {:texture-size 0 :textures []})
   :latch (atom nil)
   :diposed (atom false)
   :yield (atom false)})

;;;

(defn- clock [] (System/nanoTime))

(defvar- *hz* nil
  "Refresh rate of periodic function")

(defn set-frequency [hz]
  (reset! *hz* hz))

(defn- periodic-fn
  [hz]
  (let [frequency (atom hz)]
    (fn [f]
      (binding [*hz* frequency]
        (let [start (clock)]
          (f)
          (let [delta (- (clock) start)
                sleep (max 0 (- (/ 1e9 *hz*) delta))]
            (when-not (zero? sleep)
              (Thread/sleep (long (/ sleep 1e6)) (long (rem sleep 1e6))))))))))

(defn- timed-fn [f]
  (let [last-time (atom (/ (clock) 1e9))]
    (fn [& args]
      (let [current-time (/ (clock) 1e9)]
        (try
         (apply f (list* [(- current-time @last-time) current-time] args))
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

(defn pause
  ([]
     (pause *window*))
  ([window]
     (reset! (:yield window) true)
     (reset! (:latch window) (CountDownLatch. 1))))

(defn resume
  ([]
     (resume *window*))
  ([window]
     (when @(:latch window)
       (.countDown #^CountDownLatch @(:latch window)))))

(defn- secondary-loop [hz f]
  (let [window *window*
        wrapper (periodic-fn hz)]
    (->
     (Thread.
      (fn []
        (binding [*window* window]
          (Thread/sleep (/ 1000 hz))
          (update-loop
           #(deref (:latch window))
           #(deref (:disposed window))
           #(wrapper f)))))
     .start)))

(defn- primary-loop [window f]
  (binding [*window* window]
    (update-loop
     f
     (constantly nil)
     #(or @(:yield window) @(:disposed window)))))

;;;

(defvar- *invalidated* nil
  "Denotes whether the window should be redrawn")

(defmacro- try-call
  [k & args]
  `(if-let [callback# (if (keyword? ~k)
                        (~k (:callbacks *window*))
                        ~k)]
     (let [state# (:state *window*)]
       (when (not= @state# (swap! state# (fn [x#] (callback# ~@args x#))))
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
  (loop [button-states (vec (map #(Mouse/isButtonDown %) (range (Mouse/getButtonCount))))]
    (Mouse/poll)
    (let [dw (Mouse/getEventDWheel)
          dx (Mouse/getEventDX)
          dy (Mouse/getEventDY)
          x  (Mouse/getEventX)
          y  (Mouse/getEventY)
          button (Mouse/getEventButton)
          button-state (Mouse/getEventButtonState)
          button-name (keyword (Mouse/getButtonName button))]
      (when (not= (nth button-states button) button-state)
        (if (button-state)
          (try-call :mouse-down [x y] button-name)
          (try-call :mouse-up [x y] button-name)))
      (when (not (zero? dw))
        (try-call :mouse-wheel dw))
      (when (or (not (zero? dx)) (not (zero? dy)))
        (if (Mouse/isButtonDown button)
          (try-call :mouse-move [[dx dy] [x y]] button-name)
          (try-call :mouse-drag [[dx dy] [x y]] button-name)))
      (when (Mouse/next)
        (recur (assoc button-states button button-state))))))

(defn update-once [window]
  (let [update (when (:update window) (timed-fn (:update window)))
        display (when (:display window) (timed-fn (:display window)))]
    (handle-keyboard)
    (handle-mouse)
    (try-call update)
    (when (or (nil? *invalidated*) @*invalidated*)
      (reset! *invalidated* false)
      (try-call display))))

;;;

(defn repaint []
  (when *invalidated*
    (reset! *invalidated* true)))



