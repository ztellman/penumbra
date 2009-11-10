;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns examples.asteroids
  (:use [penumbra opengl window])
  (:use [penumbra.opengl geometry])
  (:use [clojure.contrib.seq-utils :only (separate)]))

;;;

(def *dim* [10 10])

(defn wrap
  "Makes the position wrap around from right to left, bottom to top"
  [pos]
  (map - (map #(mod %1 %2) (map + pos *dim*) (map (partial * 2) *dim*)) *dim*))

(defn dot
  ([u] (dot u u))
  ([u v] (apply + (map * u v))))

(defn intersects? [a b]
  (let [ra ((:radius a)), rb ((:radius b))
        pa ((:position a)), pb ((:position b))]
    (> (* (+ ra rb) (+ ra rb))
       (dot (map - pa pb)))))

(defn half [x] (/ x 2))
(defn twice [x] (* x 2))

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

(defn normalize [v]
  (let [len (Math/sqrt (dot v v))]
    (map #(/ % len) v)))

(defn rand-vector []
  (->> [0 0 1 1]
       (apply-matrix (rotation-matrix (rand 360) 1 0 0))
       (apply-matrix (rotation-matrix (rand 360) 0 1 0))))

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
  (def asteroid-meshes (take 20 (repeatedly #(gen-asteroid-geometry 4 25)))))

(defn gen-asteroid [initial radius direction speed]
  (let [birth (clock)
        elapsed #(/ (- (clock) birth) 1e9)
        asteroid (nth asteroid-meshes (rand-int 20))
        theta (* direction (/ Math/PI 180))
        [x y] (map (partial * speed) [(- (Math/sin theta)) (Math/cos theta)])
        position #(wrap (map + initial (map * [x y] (repeat (elapsed)))))]
    {:expired? #(< radius 0.25)
     :position position
     :radius (constantly radius)
     :render #(push-matrix
                (apply translate (position))
                (rotate (* speed (elapsed) 50) 1 0 0) (rotate theta 0 1 0)
                (scale radius radius radius)
                (color 0.6 0.6 0.6)
                (with-render-mode :wireframe
                  (call-display-list asteroid)))}))

;;spaceship

(defn draw-fuselage [] ;;should be hung in the Louvre
  (draw-triangles
   (color 1 1 1)
   (push-matrix
     (translate 0 -0.5)
     (vertex -0.4 -0.2) (vertex 0 0) (vertex 0 1)
     (vertex 0.4 -0.2) (vertex 0 0) (vertex 0 1))))

(defn draw-flame []
  (push-matrix
    (draw-triangles
     (rotate 180 0 0 1)
     (translate 0 0.5)
     (color 1 0 0) (scale 0.8 0.8) (vertex 0.4 0) (vertex -0.4 0) (vertex 0 1)
     (color 1 1 0) (scale 0.5 0.5) (vertex 0.4 0) (vertex -0.4 0) (vertex 0 1))))

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
    (if (key-pressed? :up)
      (call-display-list (:flame ship)))
    (call-display-list (:fuselage ship))))

(defn gen-spaceship []
  {:position [0 0]
   :radius 0.5
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

(defn draw-particle [position radius tint]
  (with-enabled :texture-2d
    (with-texture particle-tex
      (push-matrix
        (apply color tint)
        (apply translate (map - position (repeat (/ radius 2))))
        (scale radius radius)
        (call-display-list particle-quad)))))

(defn gen-particle [position velocity radius [r g b] lifespan]
  (let [birth (clock)
        elapsed #(/ (- (clock) birth) 1e9)
        position #(wrap (map + position (map * velocity (repeat (elapsed)))))]
    {:expired? #(> (/ (- (clock) birth) 1e9) lifespan)
     :position position
     :radius   (constantly radius)
     :render   #(draw-particle (position) radius [r g b (- 1 (Math/pow (/ (elapsed) lifespan) 10))])}))

;;game loop

(defn init [state]
  (vsync true)
  (init-particles)
  (init-asteroids)
  (enable :blend)
  (blend-func :src-alpha :one-minus-src-alpha)
  (assoc state
    :spaceship (gen-spaceship)
    :asteroids (take 10 (repeatedly #(gen-asteroid [0 0] 2 (rand 360) (+ 0.5 (rand 1.5)))))))

(defn reshape [[x y w h] state]
  (let [dim [(* (/ w h) 10) 10]]
    (frustum-view 90 (/ w h) 0.1 100)
    (load-identity)
    (translate 0 0 (- (second dim)))
    (assoc state
      :dim dim)))

(defn key-press [key state]
  (if (= key " ")
    (let [ship (:spaceship state)
          theta (* (:theta ship) (/ Math/PI 180))
          tip (map half [(- (Math/sin theta)) (Math/cos theta)])]
      (assoc state
        :bullets (conj
                  (:bullets state)
                  (gen-particle
                   (map + (:position ship) tip)
                   (map (partial * 30) tip)
                   0.66 [0 0 1] 2))))
    state))

(defn update [[dt time] state]
  (binding [*dim* (:dim state)]
    (let [spaceship (:spaceship state)
          bullets (:bullets state)
          asteroids (:asteroids state)
          [exploded remaining] (separate #(some (fn [b] (intersects? b %)) bullets) asteroids)
          bullets (remove #(or (some (fn [a] (intersects? a %)) asteroids) ((:expired? %))) bullets)
          particles (:particles state)]
      (assoc state
        :spaceship (update-spaceship dt spaceship)
        :particles (remove #((:expired? %)) particles)
        :bullets bullets
        :asteroids remaining))))

(defn display [_ state]
  (binding [*dim* (:dim state)]
    (doseq [p (concat (:particles state) (:asteroids state) (:bullets state))]
      ((:render p)))
    (draw-spaceship (:spaceship state))
    (repaint)))
   
(start
 {:reshape reshape, :display display, :init init, :update update, :key-press key-press} 
 {})