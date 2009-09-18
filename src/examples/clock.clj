(use 'penumbra.window 'penumbra.opengl)

(defn init [state]
  (set-title "Clock example")
  (start-update-loop 1
    #(assoc % :seconds (- (:seconds %) 6)))
  (start-update-loop (/ 1 60)
    #(assoc % :minutes (- (:minutes %) 6)))
  (start-update-loop (/ 1 3600)
    #(assoc % :hours (- (:hours %) 20)))
  state)

(defn reshape [state]
  (ortho-view -1 -1 1 1 -1 1))

(defn display [_ state]
  (scale 0.5 0.5 1)
  (push-matrix
    (rotate (:hours state) 0 0 1)
    (color 1 1 1)
    (line-width 5)
    (draw-lines (vertex 0 0 0) (vertex 0 0.5 0)))
  (push-matrix
    (rotate (:minutes state) 0 0 1)
    (color 1 1 1)
    (line-width 2)
    (draw-lines (vertex 0 0 0) (vertex 0 1 0)))
  (push-matrix
    (rotate (:seconds state) 0 0 1)
    (color 1 0 0)
    (line-width 1)
    (draw-lines (vertex 0 0 0) (vertex 0 1 0))))

(start {:init init :display display} {:seconds 0 :minutes 0 :hours 0})