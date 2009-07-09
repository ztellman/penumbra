(ns penumbra.examples.render-to-texture)

(import '(com.sun.opengl.util.texture TextureIO)
        '(java.io File))

(use 'penumbra.opengl 'penumbra.window 'penumbra.texture)

(def t (atom nil))

(defn square []
  (draw-quads
    (texture 0 0) (vertex 0 0 0)
    (texture 1 0) (vertex 1 0 0)
    (texture 1 1) (vertex 1 1 0)
    (texture 0 1) (vertex 0 1 0)))

(defn init []
  (reset! t (create-2d-texture 32 32 (fn [_ [u v]] [u v 0 1])))
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

(defn display [delta time]
  (scale 4 4 4)
  (translate -0.5 -0.5 0)
  (square))

(start {:display display, :mouse-drag mouse-drag, :reshape reshape, :init init})



