;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.window
  (:use [clojure.contrib.def :only (defmacro-)])
  (:use [penumbra.opengl])
  (:use [penumbra.opengl.core])
  (:import (java.awt Frame Dimension))
  (:import (java.awt.event
              MouseAdapter MouseListener MouseEvent
              MouseMotionListener MouseMotionAdapter
              WindowListener WindowAdapter))
  (:import (javax.media.opengl GLEventListener GLCapabilities GLAutoDrawable GLProfile))
  (:import (javax.media.opengl.awt GLCanvas)))

;;;;;;;;;;;;;;;;;;;;;

(defn- clock [] (System/nanoTime))

(def #^GLCanvas *canvas* nil)

;;;;;;;;;;;;;;;;;;;;;

(defn get-canvas-size [] [(.getWidth *canvas*) (.getHeight *canvas*)])
(defn set-canvas-size [w h] (.setSize *canvas* (Dimension. w h)))
(defn repaint [] (.repaint *canvas*))

;;;;;;;;;;;;;;;;;;;;;

(defn- update-state [state new-value]
  (if (not (= @state new-value)) (repaint)) ;we want to redraw if the state has been altered
  (dosync (ref-set state new-value)))

(defmacro- try-call [canvas state fns k & args]
  `(binding [*canvas* ~canvas]
    (if (~k ~fns)
      (update-state ~state
        ((~k ~fns) ~@args (deref ~state))))))

(defmacro- mouse-motion [canvas last-pos state fns event k]
  `(let [x# (. ~event getX), y# (. ~event getY)
         [last-x# last-y#] (deref ~last-pos)
         delta# [(- x# last-x#) (- y# last-y#)]]
    (dosync (ref-set ~last-pos [x# y#]))
    (try-call ~canvas ~state ~fns
      ~k [delta# [x# y#]])))

(defn start [fns initial-state]
  (let
    [frame (new Frame)
     profile (GLProfile/get GLProfile/GL2)
     cap (new GLCapabilities profile)
     state (ref initial-state)]

    (doto cap
      (.setSampleBuffers true)
      (.setNumSamples 4)) ;anti-aliasing level

    (let [canvas (new GLCanvas cap)
          last-render (ref (clock))
          last-pos (ref [0 0])]

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
                  (try-call canvas state fns
                    :update time)
                  (binding [*canvas* canvas]
                    (push-matrix
                      ((:display fns) time @state))))))

            (reshape [#^GLAutoDrawable drawable x y width height]
              (bind-gl drawable
                (viewport 0 0 width height)
                (try-call canvas state fns
                  :reshape [x y width height])))

            (init [#^GLAutoDrawable drawable]
              (bind-gl drawable
                (. *gl* setSwapInterval 1) ;turn on v-sync
                (try-call canvas state fns
                  :init)))))

        (.addMouseListener
          (proxy [MouseAdapter] []

            (mouseClicked [#^MouseEvent event]
              (try-call canvas state fns
                :mouse-click [(.getX event) (.getY event)]))

            (mousePressed [#^MouseEvent event]
              (try-call canvas state fns
                :mouse-down [(.getX event) (.getY event)]))

            (mouseReleased [#^MouseEvent event]
              (try-call canvas state fns
                :mouse-up [(.getX event) (.getY event)]))))

        (.addMouseMotionListener
          (proxy [MouseMotionAdapter] []

            (mouseDragged [#^MouseEvent event]
              (mouse-motion canvas last-pos state fns
                event :mouse-drag))

            (mouseMoved [#^MouseEvent event]
              (mouse-motion canvas last-pos state fns
                event :mouse-move)))))

      (doto frame
        (.addWindowListener
                (proxy [WindowAdapter] []
                  (windowClosing [event]
                    (. (new Thread
                      (fn []
                        (. frame dispose))) start))))
        (.add canvas)
        (.setSize 640 480)
        (.show)))))
