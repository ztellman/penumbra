(ns vellum.gears)

(use 'vellum.opengl 'vellum.window 'vellum.matrix)

(def gear (ref nil))

(defn draw-gear [num-teeth low high]
  (let [increment (/ 180. num-teeth)]
    (draw-line-strip
      (dotimes [idx num-teeth]
        (rotate increment 0 0 1)
        (vertex 0 high 0)
        (vertex 0 low 0)
        (rotate increment 0 0 1)
        (vertex 0 low 0)
        (vertex 0 high 0)))))

(defn init []
  (set-list gear (draw-gear 30 40 50)))

(defn reshape [x y width height]
  (ortho-view 0 width 0 height 1 100)
  (load-identity)
  (translate (/ width 2) (/ height 2) 0)
  (scale 1 1 -1))

(defn display [delta time]
  (write (format "%d fps" (int (/ 1 delta))) 0 1)
  (translate 0 0 10)
  (rotate (* 20. (rem time 360)) 0 0 1)
  (call-list gear))

(start {:reshape reshape :display display :init init})
