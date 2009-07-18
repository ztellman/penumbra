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
	      '(javax.media.opengl GLCanvas GLEventListener GLCapabilities GL GLAutoDrawable)
	      '(com.sun.opengl.util Animator))

(use 'penumbra.opengl.core
     'penumbra.opengl.view
     'penumbra.opengl.geometry
     'clojure.contrib.def)

;;;;;;;;;;;;;;;;;;;;;

(defn- clock [] (System/nanoTime))

(def last-render (ref (clock)))
(def last-pos (ref [0 0]))
(def actions (ref []))

(def *canvas* (ref nil))

;;;;;;;;;;;;;;;;;;;;;

(defn get-canvas-size [] [(.getWidth #^GLCanvas @*canvas*) (.getHeight #^GLCanvas @*canvas*)])
(defn set-canvas-size [w h] (.setSize #^GLCanvas @*canvas* (Dimension. w h)))
(defn repaint [] (.repaint #^GLCanvas @*canvas*))

(defn enqueue
  ([fun] (enqueue fun actions))
  ([fun list-ref] (dosync (alter list-ref #(conj % fun)))))

(defn execute
  ([] (execute actions))
  ([list-ref] (dosync (alter list-ref #(do (doseq [a %] (apply a)) [])))))

;;;;;;;;;;;;;;;;;;;;;

(defn- update-state [state new-value]
  (if (not (= @state new-value)) (repaint))
  (dosync (ref-set state new-value)))

(defmacro- try-call [state fns k & args]
  `(if (~k ~fns)
    (update-state ~state
      ((~k ~fns) ~@args (deref ~state)))))

(defn start [fns initial-state]
  (let
    [frame (new Frame)
     cap (new GLCapabilities)
     state (ref initial-state)]

    (doto cap
      (.setSampleBuffers true)
      (.setNumSamples 4)) ;anti-aliasing level

    (let [canvas (new GLCanvas cap)]

      (dosync (ref-set *canvas* canvas))

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
                  (execute)
                  (try-call state fns
                    :update time)
                  (push-matrix
                    ((:display fns) time @state)))))

            (displayChanged [drawable mode-change device-changed])

            (reshape [#^GLAutoDrawable drawable x y width height]
              (bind-gl drawable
                (viewport 0 0 width height)
                (try-call state fns
                  :reshape [x y width height]))
              (repaint))

            (init [#^GLAutoDrawable drawable]
              (bind-gl drawable
                (try-call state fns
                  :init))
              (repaint))))

        (.addMouseListener
          (proxy [MouseAdapter] []

            (mouseClicked [#^MouseEvent event]
              (try-call state fns
                :mouse-click [(.getX event) (.getY event)]))

            (mousePressed [#^MouseEvent event]
              (try-call state fns
                :mouse-down [(.getX event) (.getY event)]))

            (mouseReleased [#^MouseEvent event]
              (try-call state fns
                :mouse-up [(.getX event) (.getY event)]))))

        (.addMouseMotionListener
          (proxy [MouseMotionAdapter] []

            (mouseDragged [#^MouseEvent event]
              (let [x (. event getX), y (. event getY)
                    [last-x last-y] @last-pos
                    delta [(- x last-x) (- y last-y)]]
                (dosync (ref-set last-pos [x y]))
                (try-call state fns
                  :mouse-drag [delta [x y]])))

            (mouseMoved [#^MouseEvent event]
              (let [x (. event getX), y (. event getY)
                    [last-x last-y] @last-pos
                    delta [(- x last-x) (- y last-y)]]
                (dosync (ref-set last-pos [x y]))
                (try-call state fns
                  :mouse-move [delta [x y]]))))))

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
