(ns vellum.window)

(import '(java.awt Frame)
	      '(java.awt.event WindowListener WindowAdapter)
	      '(javax.media.opengl GLCanvas GLEventListener GL)
	      '(com.sun.opengl.util Animator))

(use 'vellum.opengl)

(defn clock [] (System/nanoTime))

(def last-render (ref (clock)))

;; Based on Dustin Withers' port of glxgears to clojure
(defn start [functions]
  (let
    [frame (new Frame)
     canvas (new GLCanvas)
     animator (new Animator canvas)]
    (. canvas
      (addGLEventListener
        (proxy [GLEventListener] []

          (display [#^javax.media.opengl.GLAutoDrawable drawable]
            (let [current (clock)
                  delta (/ (- current @last-render) 1e9)]
              (dosync (ref-set last-render current))
              (if (:display functions)
                (bind-gl drawable
                  (clear)
                  (push-matrix
                    ((:display functions) delta))))))

          (displayChanged [drawable mode-change device-changed])

          (reshape [#^javax.media.opengl.GLAutoDrawable drawable x y width height]
            (set-dimensions width height)
            (if (:reshape functions)
              (bind-gl drawable
                ((:reshape functions) x y width height))))

          (init [#^javax.media.opengl.GLAutoDrawable drawable]
            (if (:init functions)
              (bind-gl drawable
                ((:init functions))))))))

    (. frame
          (addWindowListener
            (proxy [WindowAdapter] []
              (windowClosing [event]
                (. (new Thread
                  (fn []
                    (. animator stop)
                    (. frame dispose))) start)))))
    (doto frame
      (.add canvas)
      (.setSize 800 600)
      (.show))
    (. animator start)))
