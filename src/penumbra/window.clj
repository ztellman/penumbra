;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.window
  (:use [clojure.contrib.def :only (defmacro-)])
  (:use [penumbra opengl])
  (:use [penumbra.opengl core])
  (:import (java.awt Frame Dimension))
  (:import (java.awt.event
              MouseAdapter MouseListener MouseEvent
              MouseMotionListener MouseMotionAdapter
              WindowListener WindowAdapter
              KeyAdapter KeyListener KeyEvent))
  (:import (javax.media.opengl GLEventListener GLCapabilities GLAutoDrawable GLProfile))
  (:import (javax.media.opengl.awt GLCanvas)))

;;;;;;;;;;;;;;;;;;;;;

(defn- clock [] (System/nanoTime))

(defstruct window-struct :canvas :frame :state :callbacks)

(def *window* nil)

;;;;;;;;;;;;;;;;;;;;;

(defstruct window-struct :canvas :frame :state :callbacks)

(defn get-canvas [] #^GLCanvas (:canvas *window*))
(defn get-canvas-size [] [(.getWidth (get-canvas)) (.getHeight (get-canvas))])
(defn set-canvas-size [w h] (.setSize (get-canvas) (Dimension. w h)))
(defn repaint [] (.repaint (get-canvas)))

(defn get-frame [] #^Frame (:frame *window*))
(defn set-title [title] (.setTitle (get-frame) title))

(defn get-state [] (:state *window*))

(defn- get-callbacks [] (:callbacks *window*))

;;;

(defn- update-state [state new-value]
  (if (not (identical? @state new-value)) (repaint)) ;we want to redraw if the state has been altered
  (dosync (ref-set state new-value)))

(defmacro- try-call [window k & args]
  `(binding [*window* ~window]
    (if (~k (get-callbacks))
      (update-state (get-state)
        ((~k (get-callbacks)) ~@args (deref (get-state)))))))

(defmacro- mouse-motion [window last-pos event k]
  `(let [x# (. ~event getX), y# (. ~event getY)
         [last-x# last-y#] (deref ~last-pos)
         delta# [(- x# last-x#) (- y# last-y#)]]
    (dosync (ref-set ~last-pos [x# y#]))
    (try-call ~window
      ~k [delta# [x# y#]])))

(defn- get-key [#^KeyEvent event]
  (let [key (.getKeyCode event)]
    (cond
      (= key KeyEvent/VK_UP) "UP"
      (= key KeyEvent/VK_DOWN) "DOWN"
      (= key KeyEvent/VK_LEFT) "LEFT"
      (= key KeyEvent/VK_RIGHT) "RIGHT"
      :else (KeyEvent/getKeyText key))))

;;;

(defn start-update-loop [hertz callback]
  (let [window *window*
        state  (get-state)
        period (/ 1e9 hertz)]
    (.start
      (Thread.
        (fn []
          (binding [*window* window]
            (loop []
              (let [time (clock)]
                (dosync (alter state callback))
                (repaint)
                (let [diff  (- (clock) time)
                      sleep (max 0 (- period diff))]
                  (Thread/sleep (long (/ sleep 1e6)) (long (rem sleep 1e6)))
                  (recur))))))))))

;;;

(defn start
  "Creates a window.  Supported callbacks are:
  :update         [[delta time] state]
  :display        [[delta time] state]
  :reshape        [[x y width height] state]
  :init           [state]
  :mouse-drag     [[[dx dy] [x y]] state]
  :mouse-move     [[[dx dy] [x y]] state]
  :mouse-up       [[x y] state]
  :mouse-click    [[x y] state]
  :mouse-down     [[x y] state]
  :key-type       [key state]
  :key-press      [key state]
  :key-release    [key state]"
  [callbacks initial-state]
  (let
    [frame (new Frame)
     profile (GLProfile/get GLProfile/GL2)
     cap (new GLCapabilities profile)
     state (ref initial-state)]

    '(doto cap
      (.setSampleBuffers true)
      (.setNumSamples 4)) ;anti-aliasing level

    (let [canvas (new GLCanvas cap)
          last-render (ref (clock))
          last-pos (ref [0 0])
          window (struct-map window-struct :canvas canvas :frame frame :state state :callbacks callbacks)]

      (doto canvas
        (.addGLEventListener
          (proxy [GLEventListener] []

            (display [#^GLAutoDrawable drawable]
              (let [current (clock)
                    delta (/ (- current @last-render) 1e9)
                    time [delta (/ current 1e9)]]
                (dosync (ref-set last-render current))
                (bind-gl drawable
                  (clear)
                  (try-call window
                    :update time)
                  (push-matrix
                    (binding [*window* window]
                      ((:display callbacks) time @state))))))

            (reshape [#^GLAutoDrawable drawable x y width height]
              (bind-gl drawable
                (viewport 0 0 width height)
                (try-call window
                  :reshape [x y width height])))

            (init [#^GLAutoDrawable drawable]
              (bind-gl drawable
                (. *gl* setSwapInterval 1) ;turn on v-sync
                (try-call window
                  :init)))))

        (.addMouseListener
          (proxy [MouseAdapter] []

            (mouseClicked [#^MouseEvent event]
              (try-call window
                :mouse-click [(.getX event) (.getY event)]))

            (mousePressed [#^MouseEvent event]
              (try-call window
                :mouse-down [(.getX event) (.getY event)]))

            (mouseReleased [#^MouseEvent event]
              (try-call window
                :mouse-up [(.getX event) (.getY event)]))))

        (.addMouseMotionListener
          (proxy [MouseMotionAdapter] []

            (mouseDragged [#^MouseEvent event]
              (mouse-motion window last-pos
                event :mouse-drag))

            (mouseMoved [#^MouseEvent event]
              (mouse-motion window last-pos
                event :mouse-move))))

        (.addKeyListener
          (proxy [KeyListener] []

            (keyTyped [#^KeyEvent event]
              (try-call window
                :key-type (get-key event)))

            (keyPressed [#^KeyEvent event]
              (try-call window
                :key-press (get-key event)))

            (keyReleased [#^KeyEvent event]
              (try-call window
                :key-release (get-key event))))))

      (doto frame
        (.addWindowListener
                (proxy [WindowAdapter] []
                  (windowClosing [event]
                    (. (new Thread
                      (fn []
                        (. frame dispose))) start))))
        (.add canvas)
        (.setSize 640 480)
        (.setFocusable true)
        (.show)))))
