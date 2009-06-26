(ns vellum.gears)

(use 'vellum.opengl 'vellum.window 'vellum.matrix)

(defn gear [num-teeth low high]
  (let [increment (/ 360. num-teeth)]
    (draw-line-strip
      (dotimes [idx num-teeth]
        (rotate increment 0 0 1)
        (vertex 0 high 0)
        (vertex 0 low 0)
        (rotate increment 0 0 1)
        (vertex 0 low 0)
        (vertex 0 high 0)))))

(defn reshape [x y width height]
  (ortho-view 0 width 0 height 1 100)
  (load-identity)
  (translate (/ width 2) (/ height 2) 0)
  (scale 1 1 -1))

(defn display [delta]
  ;(write (format "%d fps" (int (/ 1 delta))) 0 1)
  (translate 0 0 10)
  (gear 60 40 50)
  (Thread/sleep 20))

(start {:reshape reshape :display display})
