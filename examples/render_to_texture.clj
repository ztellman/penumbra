(ns penumbra.examples.render-to-texture)

(import '(com.sun.opengl.util.texture TextureIO)
        '(java.io File))

(use 'penumbra.opengl.core 'penumbra.opengl.geometry 'penumbra.opengl.view 'penumbra.opengl.texture 'penumbra.window)


(def t (atom nil))

(defn square []
  (push-matrix
    (scale 6 6 1)
    (translate -0.5 -0.5 0)
    (draw-quads
      (texture 0 0) (vertex 0 0 0)
      (texture 1 0) (vertex 1 0 0)
      (texture 1 1) (vertex 1 1 0)
      (texture 0 1) (vertex 0 1 0))))

(defn bordered-square []
  (disable :texture-2d)
  (push-matrix (scale 1.05 1.05 1) (square))
  (enable :texture-2d)
  (square))

(defn init []
  (reset! t (create-2d-texture 512 512 (fn [_ [u v]] [1 0 0 1])))
  '(.bind (TextureIO/newTexture (File. "/Users/zach/Downloads/Devastator.jpg") false))
  (enable :texture-2d)
  (tex-parameter :texture-2d :texture-wrap-s :clamp)
  (tex-parameter :texture-2d :texture-wrap-t :clamp)
  (tex-parameter :texture-2d :texture-min-filter :nearest)
  (tex-parameter :texture-2d :texture-mag-filter :nearest)
  (tex-env :texture-env :texture-env-mode :replace))

(defn reshape [x y width height]
  (ortho-view 0 0 (* 10 (/ (float width) height)) 10 0 10)
  (load-identity)
  (translate (* 5 (/ (float width) height)) 5 -5))

(defn mouse-drag [[dx dy] _]
  )

(defn mouse-click [x y]
  (enqueue #(render-to-texture @t () (bordered-square))))

(defn display [delta time]
  '(render-to-texture @t () (bordered-square))
  (bordered-square))

(start {:display display, :mouse-drag mouse-drag, :reshape reshape, :init init, :mouse-click mouse-click})



