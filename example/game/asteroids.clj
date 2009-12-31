;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.game.asteroids
  (:use [penumbra opengl geometry])
  (:require [penumbra.app :as app])
  (:use [clojure.contrib.seq-utils :only (separate)]))

;;;

(def *dim* [10 10])

(defn wrap
  "Makes the position wrap around from right to left, bottom to top"
  [pos]
  (map - (map #(mod %1 %2) (map + pos *dim*) (map (partial * 2) *dim*)) *dim*))

(defn expired? [x] ((:expired? x)))
(defn render [x] ((:render x)))

(defn radius [x] (if (number? (:radius x)) (:radius x) ((:radius x))))
(defn position [x] (if (sequential? (:position x)) (:position x) ((:position x))))

(defn intersects? [a b]
  (let [min-dist (+ (radius a) (radius b))
        dist (map - (position a) (position b))]
    (> min-dist (length-squared dist))))

(defn rand-color [a b]
  (map #(+ %1 (rand (- %2 %1))) a b))


;;asteroids

(defn sphere-vertices
  [lod]
  (for [theta (range 0 361 (/ 360 lod))]
    (for [phi (range -90 91 (/ 180 (/ lod 2)))]
      (cartesian [theta phi 1]))))

(defn rand-vector []
  (normalize (cartesian [(rand 360) (rand 360) 1])))

(defn offset-vertex
  "Expand if on one side of a plane, contract if on the other"
  [v vertex]
  (if (neg? (dot v (normalize vertex)))
    (map + vertex (repeat 0.05))
    (map - vertex (repeat 0.05))))

(defn offset-sphere [v vertices]
  (map (fn [arc] (map #(offset-vertex v %) arc)) vertices))

(defn gen-asteroid-vertices
  "Procedurally generate perturbed sphere"
  [lod iterations]
  (let [s (iterate #(offset-sphere (rand-vector) %) (sphere-vertices lod))]
    (nth s iterations)))

(defn gen-asteroid-geometry [lod iterations]
  (create-display-list
   (doseq [arcs (partition 2 1 (gen-asteroid-vertices lod iterations))]
    (draw-quad-strip
     (doseq [[a b] (map list (first arcs) (second arcs))]
       (apply vertex a) (apply vertex b))))))

(defn init-asteroids []
  (def asteroid-meshes (doall (take 20 (repeatedly #(gen-asteroid-geometry 12 12))))))

(defn gen-asteroid [initial radius theta speed]
  (let [birth (app/clock)
        elapsed #(- (app/clock) birth)
        asteroid (nth asteroid-meshes (rand-int 20))
        [x y] (map (partial * speed) (cartesian theta))
        position #(wrap (map + initial (map * [x y] (repeat (elapsed)))))]
    {:expired? #(< radius 0.25)
     :position position
     :radius radius
     :render #(push-matrix
                (apply translate (position))
                (rotate (* speed (elapsed) -50) 1 0 0) (rotate theta 0 1 0)
                (scale radius radius radius)
                (color 0.6 0.6 0.6)
                (call-display-list asteroid))}))

;;particles

(defn textured-quad []
  (draw-quads
   (texture 0 0) (vertex -1 -1)
   (texture 1 0) (vertex 1 -1)
   (texture 1 1) (vertex 1 1)
   (texture 0 1) (vertex -1 1)))

(defn init-particles []
  (def particle-tex
    (let [tex (create-byte-texture 128 128)]
      (draw-to-texture
       tex
       (fn [_ pos]
         (let [i (Math/exp (* 16 (- (length-squared (map - pos [0.5 0.5])))))]
           [1 1 1 i])))
      tex))
  (def particle-quad
    (create-display-list (textured-quad))))

(defn draw-particle [position radius tint]
  (push-matrix
    (apply color tint)
    (apply translate position)
    (scale radius radius)
    (call-display-list particle-quad)))

(defn gen-particle [position theta speed radius [r g b] lifespan]
  (let [birth (app/clock)
        [x y] (map (partial * speed) (cartesian theta))
        elapsed #(- (app/clock) birth)
        position #(wrap (map + position (map * [x y] (repeat (elapsed)))))]
    {:expired? #(> (- (app/clock) birth) lifespan)
     :position position
     :radius radius
     :render #(draw-particle
               (position)
               radius
               [r g b (max 0 (- 1 (Math/pow (/ (elapsed) lifespan) 3)))])}))

;;spaceship

(defn draw-fuselage [] ;;should be hung in the Louvre
  (color 1 1 1)
  (draw-triangles
   (vertex -0.4 -0.5) (vertex 0 -0.4) (vertex 0 0.5)
   (vertex 0.4 -0.5) (vertex 0 -0.4) (vertex 0 0.5)))

(defn init-spaceship []
  (def fuselage (create-display-list (draw-fuselage))))

(defn fire-bullet [state]
  (let [ship (:spaceship state)]
    (assoc state
      :bullets
      (conj
       (:bullets state)
       (gen-particle
        (:position ship)
        (:theta ship)
        15 0.25 [0 0 1] 2)))))

(defn emit-flame [state]
  (if (app/key-pressed? :up)
    (let [ship (:spaceship state)
          offset (- (rand 30) 15)
          theta (+ 180 (:theta ship) offset)
          particles (:particles state)
          position (map + (:position ship) (cartesian [theta 0.3]))
          [theta speed] (polar (map + (:velocity ship) (cartesian theta)))]
      (assoc state
        :particles (conj particles
                         (gen-particle
                          position
                          theta
                          speed
                          0.2
                          (rand-color [1 0.5 0.7] [1 1 1])
                          (/ (Math/cos (radians (* 3 offset))) 3)))))
    state))

(defn update-spaceship [dt ship]
  (let [p     (:position ship)
        v     (:velocity ship)
        theta (:theta ship)
        theta (condp (fn [x _] (app/key-pressed? x)) nil
                :left  (rem (+ theta (* 360 dt)) 360)
                :right (rem (- theta (* 360 dt)) 360)
                theta)
        a     (if (app/key-pressed? :up)
                  (map (partial * 3) (cartesian theta))
                  [0 0])
        v     (map + v (map * a (repeat dt))) 
        p     (wrap (map + p (map * v (repeat dt))))]
    (assoc ship
      :theta theta
      :position p
      :velocity v)))

(defn draw-spaceship [ship]
  (push-matrix
    (apply translate (:position ship))
    (rotate (- (:theta ship) 90) 0 0 1)
    (call-display-list fuselage)))

(defn gen-spaceship []
  {:position [0 0]
   :radius 0.5
   :velocity [0 0]
   :theta 0
   :birth (app/clock)})

;;game state

(defn reset
  "Reset to initial game state."
  [state]
  (assoc state
    :spaceship (gen-spaceship)
    :asteroids (take 4 (repeatedly
                        #(let [theta (rand 360)
                               pos (cartesian [theta 2])]
                           (gen-asteroid pos 1 theta (rand)))))))

(defn split-asteroid
  "Turn asteroid into four sub-asteroids."
  [asteroid]
  (when (< 0.25 (radius asteroid))
    (take 4
      (repeatedly
        #(gen-asteroid
          (position asteroid)
          (/ (radius asteroid) 2)
          (rand 360) (/ 1.5 (radius asteroid)))))))

(defn gen-explosion
  "Create particles within a given color range."
  [num object [lo-color hi-color] speed]
  (take num
    (repeatedly
      #(gen-particle
        (position object)
        (rand 360) (rand speed) (+ 0.15 (rand 0.65))
        (rand-color lo-color hi-color)
        2))))

(defn explode-asteroids
  "Turn asteroid into sub-asteroids and explosion particles."
  [asteroids state]
  (assoc state
    :asteroids (concat
                 (:asteroids state)
                 (mapcat split-asteroid asteroids))
    :particles (concat
                 (:particles state)
                 (mapcat #(gen-explosion (* (radius %) 100) % [[1 0.5 0] [1 1 0.2]] 2) asteroids))))

(defn check-complete
  "Are all the asteroids gone?"
  [state]
  (if (zero? (count (:asteroids state)))
    (reset state)
    state))

(defn check-ship
  "Has the ship collided with any asteroids?"
  [state]
  (let [ship (:spaceship state)
        asteroids (:asteroids state)
        [hit missed] (separate #(intersects? ship %) asteroids)]
    (if (some #(intersects? ship %) asteroids)
      (assoc state
        :asteroids (concat missed (mapcat split-asteroid hit))
        :spaceship (gen-spaceship)
        :particles (concat (:particles state) (gen-explosion 300 ship [[0 0 0.6] [0.5 0.5 1]] 7)))
      state)))

(defn check-asteroids
  "Have the asteroids collided with any bullets?"
  [state]
  (let [bullets (:bullets state)
        asteroids (:asteroids state)
        collisions (for [a asteroids, b bullets :when (intersects? a b)] [a b])
        [hit missed] (separate (set (map first collisions)) asteroids)
        bullets (remove (set (map second collisions)) bullets)
        particles (:particles state)]
    (explode-asteroids
     hit
     (assoc state
       :particles (remove expired? particles)
       :bullets (remove expired? bullets)
       :asteroids missed))))

(defn update-collisions [state]
  (binding [*dim* (:dim state)]
    (-> state check-asteroids check-ship check-complete)))

;;game loop

(defn init [state]
  (app/set-title "Asteroids")
  (app/vsync true)
  (app/key-repeat false)
  (init-asteroids)
  (init-particles)
  (init-spaceship)
  (enable :blend)
  (blend-func :src-alpha :one-minus-src-alpha)
  (app/start-update-loop 20 update-collisions)
  (app/start-update-loop 50 emit-flame)
  (reset state))

(defn reshape [[x y w h] state]
  (let [dim [(* (/ w h) 10) 10]]
    (frustum-view 45 (/ w h) 0.1 100)
    (load-identity)
    (translate 0 0 (- (* 2.165 (second dim))))
    (assoc state
      :dim dim)))

(defn key-press [key state]
  (cond
   (= key " ") (fire-bullet state)
   (= key :escape) (do (app/pause) state)
   :else state))

(defn update [[dt time] state]
  (binding [*dim* (:dim state)]
    (assoc state
      :spaceship (update-spaceship dt (:spaceship state)))))

(defn display [[dt time] state]
  (binding [*dim* (:dim state)]
    (with-enabled :texture-2d
      (with-texture particle-tex
        (doseq [p (concat (:particles state) (:bullets state))]
          (render p))))
    (with-render-mode :wireframe
      (doseq [a (:asteroids state)]
        (render a)))
    (draw-spaceship (:spaceship state))
    (app/repaint)))

(defn start []
  (app/start
   {:reshape reshape, :display display, :init init, :update update, :key-press key-press} 
   {:spaceship (gen-spaceship)}))