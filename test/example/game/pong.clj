;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.game.pong
  (:use [penumbra opengl]
        [clojure.contrib.def])
  (:require [penumbra [app :as app] [text :as text]]))

(def ball-width 0.03)
(def ball-height 0.03)
(def paddle-width 0.02)
(def paddle-height 0.15)

;;;

(defn abs [x] (Math/abs x))

(defn intersect-1d [[a b] [x y]]
  (and (<= a y) (>= b x)))

(defn intersect-2d [[a b c d] [x y z w]]
  (and (intersect-1d [a c] [x z])
       (intersect-1d [b d] [y w])))

(defn reflector [[x y w h] f]
  (let [r [x y (+ x w) (+ y h)]]
    (fn [region velocity]
      (if (intersect-2d region r)
        (f velocity)
        velocity))))

(defvar boundaries
  [(reflector [0 -1 1 1] (fn [[x y]] [x (abs y)]))
   (reflector [0 1 1 1] (fn [[x y]] [x (- (abs y))]))
   (reflector [-1 0 1 1] (fn [[x y]] [(abs x) y]))
   (reflector [1 0 1 1] (fn [[x y]] [(- (abs x)) y]))])

(defn update-ball [state dt]
  (let [ball (concat (:p state) (map + (:p state) [ball-width ball-height]))
        v (reduce
           #(%2 ball %1) (:v state)
           (conj
            boundaries
            (reflector [0.01 (:left state) paddle-width paddle-height]
                       (fn [[x y]] [(abs x) y]))
            (reflector [(- 0.99 paddle-width) (:right state) paddle-width paddle-height]
                       (fn [[x y]] [(- (abs x)) y]))))]
    (assoc state
      :v v
      :p (map + (:p state) (map (partial * dt) v)))))

(defn draw-ball [pos]
  (push-matrix
   (apply translate pos)
   (draw-quads
    (vertex 0 0)
    (vertex ball-width 0)
    (vertex ball-width ball-height)
    (vertex 0 ball-height))))

;;;

(defn update-opponent-paddle [state]
  (let [[x y] (:p state)
        [vx vy] (:v state)
        dt (/ (- 1 x) vx)
        dy (- (+ y (* vy dt)) (:right state))]
    (assoc state
      :v-right (if (neg? vx) 0 (/ dy dt)))))

(defn update-player-paddle [state]
  (assoc state
    :v-left
    (cond
     (app/key-pressed? :up) -1
     (app/key-pressed? :down) 1
     :else 0)))

(defn update-paddle [dt v pos]
  (min (- 1 paddle-height) (max 0 (+ pos (* dt v)))))

(defn draw-paddle [x y]
  (push-matrix
   (translate x y)
   (draw-quads
    (vertex 0 0)
    (vertex paddle-width 0)
    (vertex paddle-width paddle-height)
    (vertex 0 paddle-height))))

;;;

(defn reset-game [state]
  (assoc state
    :v [0.4 0.8]
    :p [0.5 0.5]
    :left 0.5, :v-left 0.0
    :right 0.5, :v-right 0.0))

(defn init [state]
  (app/vsync! true)
  (app/title! "Pong")
  (app/periodic-update! 2 update-opponent-paddle)
  (reset-game state))

(defn reshape [[x y w h] state]
  (ortho-view 0 1 1 0 -1 1)
  state)

(defn update [[delta _] state]
  (-> state
      (update-player-paddle)
      (assoc :left (update-paddle delta (:v-left state) (:left state)))
      (assoc :right (update-paddle delta (:v-right state) (:right state)))
      (update-ball delta)))

(defn display [[delta _] state]
  (draw-ball (:p state))
  (draw-paddle 0.01 (:left state))
  (draw-paddle (- 0.99 paddle-width) (:right state))
  (app/repaint!))

;;pong works, yay.
(defn start []
  (app/start
   {:display display, :reshape reshape, :update update, :init init}
   {}))
