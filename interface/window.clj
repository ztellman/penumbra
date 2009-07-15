;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.interface.window)

(import '(java.awt Frame Dimension)
	      '(java.awt.event MouseAdapter MouseListener
                         MouseEvent MouseMotionListener MouseMotionAdapter
                         WindowListener WindowAdapter)
	      '(javax.media.opengl GLCanvas GLEventListener GLCapabilities GL)
	      '(com.sun.opengl.util Animator))

(use 'penumbra.opengl.core 'penumbra.opengl.view 'penumbra.opengl.geometry)

;;;;;;;;;;;;;;

(set! *warn-on-reflection* true)

(defn- clock [] (System/nanoTime))

(def last-render (ref (clock)))
(def last-pos (ref [0 0]))
(def actions (ref []))

(def *canvas* (ref nil))

(defn get-canvas-size [] [(.getWidth @*canvas*) (.getHeight @*canvas*)])
(defn set-canvas-size [w h] (.setSize @*canvas* (Dimension. w h)))

(defn enqueue
  ([fun] (enqueue fun actions))
  ([fun list-ref] (dosync (alter list-ref #(conj % fun)))))

(defn execute
  ([] (execute actions))
  ([list-ref] (dosync (alter list-ref #(do (doseq [a %] (apply a)) [])))))

(defn start [fns initial-state]
  (let
    [frame (new Frame)
     cap (new GLCapabilities)
     state (atom initial-state)]

    (doto cap
      (.setSampleBuffers true)
      (.setNumSamples 4)) ;anti-aliasing level

    (let [canvas (new GLCanvas cap)
          animator (new Animator canvas)]

      (dosync (ref-set *canvas* canvas))

      (doto canvas
        (.addGLEventListener
          (proxy [GLEventListener] []

            (display [#^javax.media.opengl.GLAutoDrawable drawable]
              (let [current (clock)
                    delta (/ (- current @last-render) 1e9)
                    time [delta (/ current 1e9)]]
                (dosync (ref-set last-render current))
                (bind-gl drawable
                  (clear)
                  (execute)
                  (if (:update fns)
                    (reset! state ((:update fns) time @state)))
                  (if (:display fns)
                    (push-matrix ((:display fns) time @state))))))

            (displayChanged [drawable mode-change device-changed])

            (reshape [#^javax.media.opengl.GLAutoDrawable drawable x y width height]
              (bind-gl drawable
                (viewport 0 0 width height)
                (if (:reshape fns)
                  (reset! state ((:reshape fns) [x y width height] @state)))))

            (init [#^javax.media.opengl.GLAutoDrawable drawable]
              (bind-gl drawable
                (if (:init fns)
                  (reset! state ((:init fns) @state)))))))

        (.addMouseListener
          (proxy [MouseAdapter] []

            (mouseClicked [#^java.awt.event.MouseEvent event]
              (if (:mouse-click fns)
                (reset! state ((:mouse-click fns) [(. event getX) (. event getY)] @state))))

            (mousePressed [#^java.awt.event.MouseEvent event]
              (if (:mouse-down fns)
                (reset! state ((:mouse-down fns) [(. event getX) (. event getY)] @state))))

            (mouseReleased [#^java.awt.event.MouseEvent event]
              (if (:mouse-up fns)
                (reset! state ((:mouse-up fns) [(. event getX) (. event getY)] @state))))))

        (.addMouseMotionListener
          (proxy [MouseMotionAdapter] []

            (mouseDragged [#^java.awt.event.MouseEvent event]
              (let [x (. event getX), y (. event getY)
                    [last-x last-y] @last-pos
                    delta [(- x last-x) (- y last-y)]]
                (dosync (ref-set last-pos [x y]))
                (if (:mouse-drag fns)
                  (reset! state ((:mouse-drag fns) [delta [x y]] @state)))))

            (mouseMoved [#^java.awt.event.MouseEvent event]
              (let [x (. event getX), y (. event getY)
                    [last-x last-y] @last-pos
                    delta [(- x last-x) (- y last-y)]]
                (dosync (ref-set last-pos [x y]))
                (if (:mouse-move fns)
                  (reset! state ((:mouse-move fns) [delta [x y]] @state))))))))

      (doto frame
        (.addWindowListener
                (proxy [WindowAdapter] []
                  (windowClosing [event]
                    (. (new Thread
                      (fn []
                        (. animator stop)
                        (. frame dispose))) start))))
        (.add canvas)
        (.setSize 640 480)
        (.show))
      (. animator start))))
