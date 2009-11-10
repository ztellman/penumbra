;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns examples.asteroids
  (:use [penumbra opengl window])
  (:use [penumbra.opengl geometry]))

;;asteroids

(defn sphere-vertices [lod]
  (let [rotate-x (rotation-matrix (/ 180 lod) 1 0 0)
        arc (take
             (inc lod)
             (iterate (partial apply-matrix rotate-x) [0 1 0 0]))
        rotate-y (rotation-matrix (/ 180 lod) 0 1 0)]
    (take
     (inc (* 2 lod))
     (iterate
      (partial map (partial apply-matrix rotate-y))
      arc))))

(defn dot
  ([u] (dot u u))
  ([u v] (apply + (map * u v))))

(defn normalize [v]
  (let [len (Math/sqrt (dot v v))]
    (map #(/ % len) v)))

(defn rand-vector []
  (normalize (map #(- % 1) (take 3 (repeatedly #(rand 2))))))

(defn offset-vertex [v vertex]
  (if (neg? (dot v (normalize vertex)))
    (map + vertex (repeat 0.05))
    (map - vertex (repeat 0.05))))

(defn offset-sphere [v vertices]
  (map
   (fn [arc] (map #(offset-vertex v %) arc))
   vertices))

(defn gen-asteroid-vertices [lod iterations]
  (nth
   (iterate
    #(offset-sphere (rand-vector) %)
    (sphere-vertices lod))
   iterations))

(defn gen-asteroid-geometry [lod iterations]
  (get-display-list
   (doseq [arcs (partition 2 1 (gen-asteroid-vertices lod iterations))]
    (draw-quad-strip
     (doseq [[a b] (map list (first arcs) (second arcs))]
       (apply vertex a) (apply vertex b))))))

(defn init-asteroids []
  (def asteroids (take 20 (repeatedly #(gen-asteroid-geometry 5 10)))))

;;;

(def *dim* [10 10])

(defn wrap
  "Makes the position wrap around from right to left, bottom to top"
  [pos]
  (map - (map #(mod %1 %2) (map + pos *dim*) (map (partial * 2) *dim*)) *dim*))

(defn half [x] (/ x 2))
(defn twice [x] (* x 2))

;;spaceship

(defn triangle []
  (push-matrix
    (vertex 0.4 0) (vertex -0.4 0) (vertex 0 1)))

(defn draw-fuselage [] ;;should be hung in the Louvre
  (draw-triangles
   (color 1 1 1)
   (push-matrix
     (translate 0 -0.5)
     (triangle))))

(defn draw-flame []
  (push-matrix
    (draw-triangles
     (rotate 180 0 0 1)
     (translate 0 0.5)
     (color 1 0 0) (scale 0.8 0.8) (triangle)
     (color 1 1 0) (scale 0.5 0.5) (triangle))))

(defn update-spaceship [dt ship]
  (let [p     (:position ship)
        v     (:velocity ship)
        theta (:theta ship)
        theta (condp key-pressed? nil
                :left  (+ theta (* 360 dt))
                :right (- theta (* 360 dt))
                theta)
        a     (let [theta (* theta (/ Math/PI 180))]
                (if (key-pressed? :up)
                  (map (partial * 3) [(- (Math/sin theta)) (Math/cos theta)])
                  [0 0]))
        v     (map + v (map * a (repeat dt))) 
        p     (wrap (map + p (map * v (repeat dt))))]
    (assoc ship
      :theta theta
      :position p
      :velocity v)))

(defn draw-spaceship [ship]
  (push-matrix
    (apply translate (:position ship))
    (rotate (:theta ship) 0 0 1)
    (call-display-list (:fuselage ship))
    (if (key-pressed? :up)
      (call-display-list (:flame ship)))))

(defn gen-spaceship []
  {:position [0 0]
   :size [0.5 0.5]
   :velocity [0 0]
   :theta 0
   :fuselage (get-display-list (draw-fuselage))
   :flame (get-display-list (draw-flame))})

;;particles

(defn textured-quad []
  (draw-quads
   (texture 0 0) (vertex 0 0)
   (texture 1 0) (vertex 1 0)
   (texture 1 1) (vertex 1 1)
   (texture 0 1) (vertex 0 1)))

(defn init-particles []
  (def particle-tex
    (let [tex (create-byte-texture 128 128)]
      (draw-to-texture
       tex
       (fn [_ pos]
         (let [i (Math/exp (* 16 (- (dot (map - pos [0.5 0.5])))))]
           [1 1 1 i])))
      tex))
  (def particle-quad
    (get-display-list (textured-quad))))

(defn draw-particle [position size tint]
  (with-enabled :texture-2d
    (bind-texture particle-tex)
    (push-matrix
      (apply color tint)
      (apply translate (map - position (map #(/ % 2) size)))
      (apply scale size)
      (call-display-list particle-quad))))

(defn gen-particle [position velocity size [r g b] lifespan]
  (let [birth (clock)
        elapsed #(/ (- (clock) birth) 1e9)
        position #(wrap (map + position (map * velocity (repeat (elapsed)))))]
    {:expired? #(> (/ (- (clock) birth) 1e9) lifespan)
     :position position
     :size     (constantly size)
     :render   #(draw-particle (position) size [r g b (- 1 (Math/pow (/ (elapsed) lifespan) 10))])}))

;;game loop

(defn init [state]
  (vsync true)
  (init-particles)
  (init-asteroids)
  (enable :blend)
  (blend-func :src-alpha :one-minus-src-alpha)
  (assoc state
    :spaceship (gen-spaceship)))

(defn reshape [[x y w h] state]
  (let [dim [(* (/ w h) 10) 10]]
    (frustum-view 45 (/ w h) 0.1 100)
    (load-identity)
    (translate 0 0 (* 2 (- (second dim))))
    (assoc state
      :dim dim)))

(defn key-press [key state]
  (if (= key " ")
    (let [ship (:spaceship state)
          theta (* (:theta ship) (/ Math/PI 180))
          tip (map half [(- (Math/sin theta)) (Math/cos theta)])]
      (assoc state
        :particles (conj
                    (:particles state)
                    (gen-particle (map + (:position ship) tip)
                                  (map (partial * 20) tip)
                                  [0.5 0.5] [0 0 1] 4))))
    state))

(defn update [[dt time] state]
  (binding [*dim* (:dim state)]
    (assoc state
      :spaceship (update-spaceship dt (:spaceship state))
      :particles (remove #((:expired? %)) (:particles state)))))

(defn display [_ state]
  (binding [*dim* (:dim state)]
    (doseq [p (:particles state)]
      ((:render p)))
    (draw-spaceship (:spaceship state))
    (repaint)))
   
(start
 {:reshape reshape, :display display, :init init, :update update, :key-press key-press} 
 {})