;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.framebuffer
  (:use [penumbra opengl])
  (:use [penumbra.opengl texture])
  (:require [penumbra.slate :as slate])
  (:use [penumbra.opengl.core :only (enum enum-name)])
  (:use [clojure.contrib.def :only (defn-memo)])
  (:use [clojure.contrib.combinatorics :only (cartesian-product)]))

(def internal-formats
  [[:float 1 :luminance32f-arb]
   [:float 2 :luminance-alpha32f-arb]
   [:float 3 :rgb32f-arb]
   [:float 4 :rgba32f-arb]
   [:float 1 :float-r32-nv]
   [:float 2 :float-rg32-nv]
   [:float 3 :float-rgb32-nv]
   [:float 4 :float-rgba32-nv]
   [:float 1 :alpha-float32-ati]
   [:float 1 :intensity-float32-ati]
   [:float 1 :luminance-float32-ati]
   [:float 2 :luminance-alpha-float32-ati]
   [:float 3 :rgb-float32-ati]
   [:float 4 :rgba-float32-ati]
   [:int 1 :luminance32i]
   [:int 2 :luminance-alpha32i]
   [:int 3 :rgb32i]
   [:int 4 :rgba32i]
   [:int 1 :alpha-integer]
   [:int 1 :luminance-integer]
   [:int 2 :luminance-alpha-integer]
   [:int 3 :rgb-integer]
   [:int 4 :rgba-integer]
   [:unsigned-byte 1 :luminance]
   [:unsigned-byte 2 :luminance-alpha]
   [:unsigned-byte 3 :rgb]
   [:unsigned-byte 4 :rgba]])

(def pixel-format
  {1 :luminance, 2 :luminance-alpha, 3 :rgb, 4 :rgba})

(defn- valid-read-format? [type tuple format]
  (try
   (let [tex (create-texture :texture-rectangle [16 16] format (pixel-format tuple) type tuple)]
     ;;TODO: test that if we write to it, we can read back the same thing
      (destroy-texture tex)
      [format (pixel-format tuple) type])
   (catch Exception e
     false)))

(defn-memo read-format [type tuple]
  (some
    #(apply valid-read-format? %)
    (filter
     #(and (= type (first %)) (= tuple (second %)))
     internal-formats)))

(defn- valid-write-format? [type tuple format]
  (let [curr (get-frame-buffer)
        fb (gen-frame-buffer)]
    (bind-frame-buffer fb)
    (try
     (let [tex (create-texture :texture-rectangle [16 16] format (pixel-format tuple) type tuple)]
        (attach-textures [] [tex])
        (if (frame-buffer-ok?)
          (do (when tex (destroy-texture tex)) [format (pixel-format tuple) type])
          (do (when tex (destroy-texture tex)) false)))
      (catch Exception e
        false)
      (finally
        (destroy-frame-buffer fb)
        (bind-frame-buffer curr)))))

(defn-memo write-format [type tuple]
  (some
    #(apply valid-write-format? %)
    (filter
      #(and (= type (first %)) (= tuple (second %)))
      internal-formats)))

(defn print-compatible-types []
  (let [permutations (cartesian-product [:float :int :unsigned-byte] (range 1 5))]
    (slate/with-slate (slate/create)
      (doseq [[type tuple] permutations]
        (println
          (name type) tuple
          "\n  R " (if-let [format (read-format type tuple)]
                   (first format)
                   "NONE")
          "\n  W " (if-let [format (write-format type tuple)]
                   (first format)
                   "NONE"))))))

