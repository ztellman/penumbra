(ns vellum.window)

(import '(java.awt Frame)
	      '(java.awt.event MouseAdapter MouseListener
                         MouseEvent MouseMotionListener MouseMotionAdapter
                         WindowListener WindowAdapter)
	      '(javax.media.opengl GLCanvas GLEventListener GL)
	      '(com.sun.opengl.util Animator))

(set! *warn-on-reflection* true)

(use 'vellum.opengl)

(defn clock [] (System/nanoTime))

(def last-render (ref (clock)))
(def last-pos (ref [0 0]))

;; Based on Dustin Withers' port of glxgears to clojure and Phil Hazelden's cloggle project
(defn start [funs]
  (let
    [frame (new Frame)
     canvas (new GLCanvas)
     animator (new Animator canvas)]
    (doto canvas
      (.addGLEventListener
        (proxy [GLEventListener] []

          (display [#^javax.media.opengl.GLAutoDrawable drawable]
            (let [current (clock)
                  delta (/ (- current @last-render) 1e9)]
              (dosync (ref-set last-render current))
              (if (:display funs)
                (bind-gl drawable
                  (clear)
                  (push-matrix
                    ((:display funs) delta (/ current 1e9)))))))

          (displayChanged [drawable mode-change device-changed])

          (reshape [#^javax.media.opengl.GLAutoDrawable drawable x y width height]
            (set-dimensions width height)
            (if (:reshape funs)
              (bind-gl drawable
                ((:reshape funs) x y width height))))

          (init [#^javax.media.opengl.GLAutoDrawable drawable]
            (if (:init funs)
              (bind-gl drawable
                ((:init funs)))))))

      (.addMouseListener
        (proxy [MouseAdapter] []

          (mouseClicked [#^java.awt.event.MouseEvent event]
            (if (:mouse-click funs) ((:mouse-click funs) (. event getX) (. event getY))))

          (mousePressed [#^java.awt.event.MouseEvent event]
            (if (:mouse-down funs) ((:mouse-down funs) (. event getX) (. event getY))))

          (mouseReleased [#^java.awt.event.MouseEvent event]
            (if (:mouse-up funs) ((:mouse-up funs) (. event getX) (. event getY))))))

      (.addMouseMotionListener
        (proxy [MouseMotionAdapter] []

          (mouseDragged [#^java.awt.event.MouseEvent event]
            (if (:mouse-drag funs)
              (let [x (. event getX), y (. event getY)
                    [last-x last-y] @last-pos
                    delta [(- x last-x) (- y last-y)]]
                (dosync (ref-set last-pos [x y]))
                ((:mouse-drag funs) delta [x y]))))

          (mouseMoved [#^java.awt.event.MouseEvent event]
            (if (:mouse-move funs)
              (let [x (. event getX), y (. event getY)
                    [last-x last-y] @last-pos
                    delta [(- x last-x) (- y last-y)]]
                (dosync (ref-set last-pos [x y]))
                ((:mouse-move funs) delta [x y])))))))

    (doto frame
      (.addWindowListener
              (proxy [WindowAdapter] []
                (windowClosing [event]
                  (. (new Thread
                    (fn []
                      (. animator stop)
                      (. frame dispose))) start))))
      (.add canvas)
      (.setSize 800 600)
      (.show))
    (. animator start)))
