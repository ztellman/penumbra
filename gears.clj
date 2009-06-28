(ns vellum.gears)

(use 'vellum.opengl 'vellum.window 'vellum.matrix)

;;;;;;;;;;;;;;;;;;;;;;;;
;Gear building functions

(defmacro mirror [divider & args]
  "given [A b c], yields [b c A c b]"
  `(do
    ~@args
    ~divider
    ~@(reverse args)))

(defn draw-gear-face [num-teeth low mid high]
  (let [increment (/ 90. num-teeth)]
    (draw-quads
      (rotate increment 0 0 1)
      (dotimes [idx (inc num-teeth)]
        (mirror (rotate increment 0 0 1)
          (vertex 0 low 0)
          (vertex 0 high 0))
        (vertex 0 low 0)
        (vertex 0 high 0)
        (rotate increment 0 0 1)
        (vertex 0 mid 0)
        (vertex 0 low 0)
        (mirror (rotate increment 0 0 1)
          (vertex 0 low 0)
          (vertex 0 mid 0))
        (vertex 0 low 0)
        (vertex 0 mid 0)
        (rotate increment 0 0 1)
        (vertex 0 high 0)
        (vertex 0 low 0)))))

(defn draw-gear-teeth [num-teeth mid high]
  (let [increment (/ 90. num-teeth)]
    (draw-quads
      (dotimes [idx num-teeth]
        (rotate increment 0 0 1)
        (mirror (rotate increment 0 0 1)
          (vertex 0 high 1)
          (vertex 0 high 0))
        (push-matrix
          (mirror (do (rotate increment 0 0 1) (translate 0 (- mid high) 0))
            (vertex 0 high 1)
            (vertex 0 high 0)))
        (rotate increment 0 0 1)
        (mirror (rotate increment 0 0 1)
          (vertex 0 mid 1)
          (vertex 0 mid 0))
        (push-matrix
          (mirror (do (rotate increment 0 0 1) (translate 0 (- high mid) 0))
            (vertex 0 mid 1)
            (vertex 0 mid 0)))))))

(defn draw-gear-hole [num-teeth radius]
  (let [increment (/ 180. num-teeth)]
    (draw-quads
      (dotimes [idx (* 2 num-teeth)]
        (mirror (rotate increment 0 0 1)
          (vertex 0 radius 0)
          (vertex 0 radius 1))))))

(defn draw-gear [num-teeth low mid high width]
  (material 0.8 0.2 0.2 1)
  (push-matrix
    (scale 1 1 width)
    (push-matrix
      (rotate 180 0 1 0)
      (draw-gear-face num-teeth low mid high))
    (push-matrix
      (draw-gear-teeth num-teeth mid high))
    (push-matrix
      (translate 0 0 1)
      (draw-gear-face num-teeth low mid high))
    (push-matrix
      (draw-gear-hole num-teeth low))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def gear (ref nil))

(defn init []
  (cull-back)
  (enable-cull-face)
(enable-auto-normals)
  (enable-depth-test)
  (shade-model :flat)
  (enable-anti-aliasing)
  (render-mode :fill)
  (set-list gear (draw-gear 20 0.5 3 4 2))
  (setup-light 0 [0 0 0]))

(defn reshape [x y width height]
  (frustum-view 90 (/ (double width) height) 1 100)
  (load-identity))

(defn display [delta time]
  (write (format "%d fps" (int (/ 1 delta))) 0 1)
  (translate 0 0 -10)
  (rotate 45 0 1 0)
  (rotate (* 20. (rem time 360)) 0 0 1)
  (color 1 0 0)
  (call-list gear))

(start {:reshape reshape :display display :init init})
