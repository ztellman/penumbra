(ns vellum.main)

(use 'vellum.opengl 'vellum.window)

(defn reshape [x y width height]
  (ortho-view 0 width 0 height 1 100)
  (load-identity)
  (translate (/ width 2) (/ height 2) 0)
  (scale 1 1 -1))

(defn display [& args]
  (color 1 1 1)
  (translate
    (* 100 (Math/sin (/ (System/currentTimeMillis) 1000)))
    (* 100 (Math/cos (/ (System/currentTimeMillis) 1000)))
    0)
  (draw-quads
    (vertex 0 0 10)
    (vertex 0 10 10)
    (vertex 10 10 10)
    (vertex 10 0 10)))

(start {:reshape reshape :display display})
