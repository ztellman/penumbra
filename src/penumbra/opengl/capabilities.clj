;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.capabilities
  (:require [penumbra.opengl.slate :as slate]
            [penumbra.opengl :as gl]
            [penumbra.opengl.texture :as tex]
            [penumbra.opengl.frame-buffer :as fb]
            [penumbra.data :as data])
  (:use [penumbra.opengl.core :only (enum enum-name gl-import- get-integer)]
        [clojure.contrib.def :only (defn-memo)]
        [clojure.contrib.combinatorics :only (cartesian-product)])
  (:import [java.nio IntBuffer]))

(defn- get-frame-buffer []
  (get-integer :framebuffer-binding))

(defn- valid-read-format? [type tuple format]
  (try
   (let [tex (tex/create-texture
              :target :texture-rectangle
              :dim [16 16]
              :internal-format format
              :pixel-format (tex/tuple->pixel-format tuple)
              :internal-type type)]
     ;;TODO: test that if we write to it, we can read back the same thing
     (data/destroy! tex)
     (map #(-> tex data/params %) [:internal-format :pixel-format :internal-type]))
   (catch Exception e
     false)))

(defn-memo read-format [type tuple]
  (let [candidates
        (filter
         #(and (= type (first %)) (= tuple (second %)))
         tex/internal-formats)]
    (some
     #(apply valid-read-format? %)
     candidates)))

(defn- valid-write-format? [type tuple format]
  (let [curr (get-frame-buffer)
        fb (fb/gen-frame-buffer)]
    (fb/bind-frame-buffer fb)
    (try
     (let [tex (tex/create-texture
                :target :texture-rectangle
                :dim [16 16]
                :internal-format format
                :pixel-format (tex/tuple->pixel-format tuple)
                :internal-type type)]
       (fb/attach tex 0)
       (try
        (when (fb/frame-buffer-ok?)
          [format (tex/tuple->pixel-format tuple) type])
        (catch Exception e
          false)
        (finally
         (when tex
           (data/destroy! tex)))))
     (catch Exception e
       false)
     (finally
      (fb/attach nil 0)
      (fb/destroy-frame-buffer fb)
      (fb/bind-frame-buffer curr)))))

(defn-memo write-format [type tuple]
  (let [formats (filter
                 #(and (= type (first %)) (= tuple (second %)))
                 tex/internal-formats)]
    (some
     #(apply valid-write-format? %)
     formats)))

(defn print-compatible-types []
  (let [permutations (cartesian-product [:float :int :unsigned-byte] (range 1 5))]
    (slate/with-slate
      (doseq [[type tuple] permutations]
        (println
          (name type) tuple
          "\n  R " (if-let [format (read-format type tuple)]
                     (first format)
                     "NONE")
          "\n  W " (if-let [format (write-format type tuple)]
                     (first format)
                     "NONE"))))))

