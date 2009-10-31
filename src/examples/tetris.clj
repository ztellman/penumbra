;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns examples.tetris
  (:use [penumbra window opengl])
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:use [clojure.contrib.pprint]))

;;;

(defn rotate* [clockwise [x y]]
  (let [k (if clockwise -1 1)]
    [(* k y) (* (- k) x)]))

(defn translate* [[dx dy] [x y]]
  [(+ x dx) (+ y dy)])

(def one-way (repeat identity))
(def two-way (cycle [#(rotate* true %) #(rotate* false %)]))
(def four-way (repeat #(rotate* true %)))

(defn parse-shape
  "Takes the string representation of a shape, and turns it into a sequence of
  offsets from the shape's center."
  [shape]
  (let [even       #(if (even? %) % (dec %))
        lines     (map vector (map #(.trim %) (.split shape "\n")) (iterate inc 0))
        width     (even (count (ffirst lines)))
        height    (even (count lines))
        offset    [(/ width 2) (/ height 2)]
        positions (apply
                   concat
                   (map
                    (fn [[x y]] (partition 3 (interleave x (iterate inc 0) (repeat y))))
                    lines))
        filtered  (filter #(-> % first (= \X)) positions)
        relative  (map #(map - (rest %) offset) filtered)]
    relative))

(def tetras
     (map
      (fn [[a b c]] {:shape (parse-shape a) :color b :fns c})
      [["X
         X
         X
         X"
        [1 0 0]
        two-way]
       ["X..
         XXX"
        [0 1 0]
        four-way]
       ["XXX
         ..X"
        [0 0 1]
        four-way]
       ["XX
         XX"
        [1 1 0]
        one-way]
       [".XX
         XX."
        [1 0 1]
        two-way]
       [".X.
         XXX"
        [0 1 1]
        four-way]
       ["XX.
         .XX"
        [0.5 0.5 0.5]
        two-way]]))

;;;

(def width 10)
(def height 20)

(defn gen-tetra []
  (nth tetras (rand-int (count tetras))))

(defn initialize-state
  "Clears out the pit, and generates first two shapes."
  [state]
  (assoc state
    :blocks (apply vector (take height (repeat (apply vector (take width (repeat nil))))))
    :offset [(/ width 2) 0]
    :tetra  (gen-tetra)
    :next-tetra (gen-tetra)))

(defn next-block
  "Advances next-tetra to tetra, and generates new next-tetra."
  [state]
  (assoc state
    :next-tetra (gen-tetra)
    :offset [(/ width 2) 0]
    :tetra (:next-tetra state)))

(defn try-move
  "Applies transforms to tetra. If the new shape exceeds the boundaries of the pit or
  overlaps existing blocks, returns an unchanged state.  Otherwise, returns state with
  transformed tetra."
  [offset-transform shape-transform state]
  (let [shape   (map shape-transform (:shape (:tetra state)))
        offset  (offset-transform (:offset state))
        shape*  (filter
                 (fn [[x y]] (<= 0 y)) ;we don't care if it's too high
                 (map #(translate* offset %) shape))
        overlap (try
                 (some
                  identity
                  (map (fn [[x y]] (((:blocks state) y) x)) shape*))
                 (catch Exception e
                   true))]
    (if overlap
      state
      (assoc state
        :tetra
        (assoc (:tetra state)
          :shape shape)
        :offset
        offset))))

(defn add-to-blocks
  "Adds the current tetra to the pit, removes any full rows, and adds
  an equal number of rows to the top."
  [state]
  (let [tetra (:tetra state)
        color (:color tetra)
        shape (:shape tetra)
        offset (:offset state)
        blocks (reduce
                (fn [b [x y]] (assoc b y (assoc (b y) x color)))
                (:blocks state)
                (map #(translate* offset %) shape))
        cleared (filter #(not-every? identity %) blocks)
        padded (concat
                (take (- height (count cleared)) (repeat (take width (repeat nil))))
                cleared)]
    (assoc state
      :blocks (apply vector (map #(apply vector %) padded)))))

(defn descend
  "Moves the block down one step. If that's not possible, adds the tetra to the pit.
  If that's not possible, the tetra is above the pit, so the game is restarted."
  [state]
  (let [state* (try-move #(translate* [0 1] %) identity state)]
    (if (identical? state state*)
      (try
       (next-block (add-to-blocks state))
       (catch Exception e
         (initialize-state state)))
      state*)))

;;;

(defn init [state]
  (start-update-loop
   2
   (fn [state]
     (if (key-pressed? :down)
       (set-frequency 10)
       (set-frequency 2))
     (descend state)))
  (key-repeat true)
  state)

(defn reshape [[x y w h] state]
  (let [aspect (/ (float w) h)
        height (if (> 1 aspect) (/ 1.0 aspect) 1)
        aspect (max 1 aspect)]
    (ortho-view (- aspect) aspect height (- height) -1 1)
    state))

(defn key-press [key state] 
  (cond
   (= key :up)
   (assoc state
     :tetra
     (let [tetra (:tetra state)
           fns (:fns tetra)
           shape (:shape tetra)]
       (assoc tetra
         :fns (rest fns)
         :shape (:shape (:tetra (try-move identity #((first fns) %) state))))))
   (= key :left)
   (try-move #(translate* [-1 0] %) identity state)
   (= key :right)
   (try-move #(translate* [1 0] %) identity state)
   :else
   state))

(defn rectangle [x y]
  (push-matrix
   (translate x y 0)
   (dotimes [_ 4]
     (rotate 90 0 0 1)
     (vertex 0.5 0.5 0))))

(defn draw-bordered-block [col [x y]]
  (when (<= 0 y)
    (apply color col)
    (draw-quads (rectangle x y))
    (color 0 0 0)
    (draw-line-loop (rectangle x y))
    (color 1 1 1)))

(defn draw-tetra [tetra offset]
  (doseq [block (map #(translate* offset %) (:shape tetra))]
    (draw-bordered-block (:color tetra) block)))

(defn display [_ state]
  (scale (/ 2 width) (/ 2 height) 1)
  (scale 0.49 0.99 1)
  (push-matrix
   (scale 0.99 0.99 1)
   (translate (+ 0.5 (/ width -2)) (+ 0.5 (/ height -2)) 0)
   ;;draw shape
   (draw-tetra (:tetra state) (:offset state))
   ;;draw blocks in pit
   (doseq [[y row] (indexed (:blocks state))]
     (doseq [[x block] (indexed row)]
       (if block (draw-bordered-block block [x y])))))
  (translate (/ width -2) (/ height -2) 0)
  ;;draw border
  (draw-line-loop
   (vertex 0 0 0) (vertex width 0 0)
   (vertex width height 0) (vertex 0 height 0))
  ;;draw next shape
  (draw-tetra (:next-tetra state) [13 5]))

(start
 {:init init, :display display, :reshape reshape, :key-press key-press}
 (initialize-state {}))


