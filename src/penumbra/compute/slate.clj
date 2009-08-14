;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.interface.slate
  (:use [clojure.contrib.def :only (defmacro- defn-memo)])
  (:use [clojure.contrib.pprint])
  (:use [clojure.contrib.lazy-seqs :only (primes)])
  (:use [clojure.contrib.seq-utils :only (separate)])
  (:use [penumbra.opengl core geometry texture])
  (:import (java.util.concurrent Semaphore))
  (:import (javax.media.opengl
              GLPbuffer GLDrawableFactory GLEventListener
              GLCapabilities GLProfile GLAutoDrawable)))

;;;;;;;;;;;;;;;;;

(defn- prime-factors
  "returns prime factors of a number"
  ([n] (prime-factors primes [] n))
  ([primes factors n]
	 (let [p (first primes)]
	   (cond
		 (= n 1) factors
		 (zero? (rem n p)) (recur primes (conj factors p) (/ n p))
		 :else (recur (rest primes) factors n)))))

(defn rectangle [n]
  (let [factors   (prime-factors n)
        reordered (take (count factors) (interleave factors (reverse factors)))
        sqrt      (int (Math/sqrt n))
        divisor   (reduce #(if (>= sqrt (* %1 %2)) (* %1 %2) %1) 1 reordered)]
    [divisor (/ n divisor)]))

;;;;;;;;;;;;;;;;;

(defstruct slate-struct :p-buffer :queue :width :height :textures :texture-size)

(def *slate* nil)
(def *tex-mem-threshold* 10e6)
(def *tex-count-threshold* 10)

(defn repaint [slate]
  (.repaint #^GLPbuffer (:p-buffer slate)))

(defn enqueue [slate f]
  (dosync
    (alter (:queue slate)
      #(concat % (list f))))
  (repaint slate))

(defn execute [slate]
  (let [coll @(:queue slate)]
    (doseq [f coll]
      (push-matrix (f)))
    (dosync
      (alter (:queue slate)
        #(drop (count coll) %)))))

(defn invoke [slate f]
  (let [#^Semaphore s (Semaphore. 1)]
    (.acquire s)
    (enqueue slate (fn [] (f) (.release s)))
    (.acquire s)))

(defn destroy [slate]
  (enqueue slate
    (fn []
      (println "cleaning up" (count @(:textures slate)) "textures")
      (destroy-textures @(:textures slate))
      (.destroy #^GLPbuffer (:p-buffer slate)))))

;;;;;;;;;;;;;;;;;

(defn- draw* [w h]
  (gl-active-texture :texture0)
  (push-matrix
    (draw-quads
      (texture 0 h) (vertex 0 0 0)
      (texture w h) (vertex w 0 0)
      (texture w 0) (vertex w h 0)
      (texture 0 0) (vertex 0 h 0))))

(defn draw []
  (call-display-list @(:display-list *slate*)))

;;;;;;;;;;;;;;;;;;

(defn- separate-textures [textures]
  (separate #(and (not (persistent? %)) (>= 0 @(:refs %))) textures))

(defn cleanup-textures [slate]
  (let [[discard keep] (separate-textures @(:textures slate))]
    ;(println "discard:" (map #(:id %) discard))
    ;(println "keep:" (map #(:id %) keep))
    (if (< 0 (count discard))
      (destroy-textures discard))
    (dosync
      (let [textures (alter (:textures slate)
                        #(let [[d k] (separate-textures %)]
                          (vec (concat (drop (count discard) d) k))))]
        (ref-set (:texture-size slate) (reduce + (map sizeof textures)))))))

(defn add-texture [slate t]
  (let [textures @(:textures slate)
        texture-size @(:texture-size slate)]
    (if (or (> (count textures) *tex-count-threshold*)
            (> texture-size *tex-mem-threshold*))
      (cleanup-textures slate)))
  (dosync
    (alter (:texture-size slate)
      #(+ % (sizeof t)))
    (alter (:textures slate)
      #(conj % t))))

(defn create-slate
  ([] (create-slate 4096 4096))
  ([width height]
    (let
      [profile (GLProfile/get GLProfile/GL2GL3)
       cap (GLCapabilities. profile)]

      ;cap stuff goes here

      (let [p-buffer  (.. (GLDrawableFactory/getFactory profile) (createGLPbuffer cap nil width height nil))
            slate     (struct-map slate-struct
                        :p-buffer p-buffer :queue (ref '()) :textures (ref []) :texture-size (ref 0.)
                        :width width :height height
                        :display-list (atom nil))]

        (doto p-buffer
          (.addGLEventListener
            (proxy [GLEventListener] []

              (display [drawable]
                (bind-gl drawable
                  (binding [*slate* slate]
                    (execute slate))))

              (reshape [#^GLAutoDrawable drawable x y width height])

              (init [#^GLAutoDrawable drawable]
                (bind-gl drawable
                  (bind-frame-buffer (gen-frame-buffer))
                  (set-display-list (:display-list slate)
                    (draw* width height))
                  (viewport 0 0 width height)
                  (ortho-view 0 0 width height 0 1))))))
        slate))))

(defmacro with-slate
  [slate & body]
    `(invoke ~slate (fn [] ~@body)))

(defmacro with-blank-slate
  [& body]
  `(let [slate# (create-slate)]
    (with-slate slate#
      ~@body)
    (destroy slate#)))

