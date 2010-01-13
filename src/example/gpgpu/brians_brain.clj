;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.gpgpu.brians-brain
  (:use [penumbra opengl compute])
  (:require [penumbra.app :as app]
            [penumbra.text :as text]))

(defn init [state]

  (app/title! "Brian's Brain")
  (app/display-mode! 1024 768)
  (app/vsync! false)

  (defmap update-automata
    (let [cell (.a %)
          neighbors 0.]
      (when (= 0. cell)
        (convolve 1
          (when (= 1. (.a %))
            (+= neighbors 1.))))
      (cond
       (= 1. cell)      (color4 0. 0. 1. 0.5)
       (= 2. neighbors) (color4 1. 1. 1. 1.)
       :else            (color4 0. 0. 0. 0.))))

  (enable :texture-rectangle)

  (defmap colorize
    (color3 (.xyz %)))

  state)

(defn reshape [[x y w h] state]
  (let [tex (create-byte-texture :texture-rectangle w h)]
    (draw-to-texture!
     tex
     (fn [[x y] _]
       (if (zero? (rand-int 2))
         [1 1 1 1]
         [0 0 0 0])))
    (assoc state
     :tex tex)))

(defn key-press [key state]
  (cond
   (= :escape key) (app/pause!)))

(defn update [_ state]
  (update-in state [:tex] #(update-automata %)))

(defn display [[delta _] state]
  (blit! (colorize (:tex state)))
  (text/write-to-screen (format "%d fps" (int (/ 1 delta))) 0 0)  
  (app/repaint!))

(defn start []
  (app/start
   {:init init, :reshape reshape, :display display, :update update, :key-press key-press}
   {}))