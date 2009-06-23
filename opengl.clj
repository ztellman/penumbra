(ns vellum)

(import '(java.awt Frame)
	      '(java.awt.event WindowListener WindowAdapter)
	      '(javax.media.opengl GLCanvas GLEventListener GL)
	      '(com.sun.opengl.util Animator))

(set! *warn-on-reflection* true)

(def #^GL *gl* nil)

(defmacro import-fn [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *gl* ~'~import-from ~@args#)))

;renamed functions
(import-fn glVertex3d vertex)
(import-fn glColor3d color)
(import-fn glRotated rotate)
(import-fn glTranslated translate)
(import-fn glScalef scale)
(import-fn glLoadIdentity load-identity)

;gl-style functions, which will likely be wrapped in some other function
(import-fn glBegin gl-begin)
(import-fn glEnd gl-end)
(import-fn glPushMatrix gl-push-matrix)
(import-fn glPopMatrix gl-pop-matrix)
(import-fn glMatrixMode gl-matrix-mode)
(import-fn glFrustrum gl-frustrum)
(import-fn glOrtho gl-ortho)
(import-fn glClear gl-clear)

(defmacro push-matrix [& args]
  `(do
    (gl-push-matrix)
    ~@args
    (gl-pop-matrix)))

(defmacro do-quads [& args]
  `(do
    (gl-begin GL/GL_QUADS)
    ~@args
    (gl-end)))

(defn set-ortho [left right bottom top near far]
  (gl-matrix-mode GL/GL_PROJECTION)
  (load-identity)
  (gl-ortho left right bottom top near far)
  (gl-matrix-mode GL/GL_MODELVIEW))

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
            (if (:display functions)
              (binding [*gl* (.getGL drawable)]
                (gl-clear GL/GL_DEPTH_BUFFER_BIT)
                (gl-clear GL/GL_COLOR_BUFFER_BIT)
                ((:display functions) nil))))

          (displayChanged [drawable mode-change device changed])

          (reshape [#^javax.media.opengl.GLAutoDrawable drawable x y width height]
            (if (:reshape functions)
              (binding [*gl* (.getGL drawable)]
                ((:reshape functions) x y width height))))

          (init [#^javax.media.opengl.GLAutoDrawable drawable]
            (if (:init functions)
              (binding [*gl* (.getGL drawable)]
                ((:init functions) nil)))))))
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

(defn reshape [x y width height]
  (set-ortho 0 width 0 height 1 100)
  (load-identity)
  (translate (/ width 2) (/ height 2) 0)
  (scale 1 1 -1))

(defn display [& args]
  (color 1 1 1)
  (do-quads
    (vertex 0 0 10)
    (vertex 0 10 10)
    (vertex 10 10 10)
    (vertex 10 0 10)))

(start {:reshape reshape :display display})
