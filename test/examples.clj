;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns test.examples
  (:use [clojure.test])
  (:require [example.opengl.gears :as gears]
            [example.opengl.sierpinski :as sierpinski]
            [example.opengl.render-to-texture :as rtt]
            [example.opengl.marble :as marble]
            [example.game.tetris :as tetris]
            [example.game.asteroids :as asteroids]
            [example.gpgpu.mandelbrot :as mandelbrot]
            [example.gpgpu.convolution :as convolution]
            [example.gpgpu.brians-brain :as brian]))

(deftest run
  (testing "Gears"
    (gears/start))
  (testing "Sierpinski"
    (sierpinski/start))
  (testing "Render-to-Texture"
    (rtt/start))
  (testing "Marble"
    (marble/start))
  (testing "Tetris"
    (tetris/start))
  (testing "Asteroids"
    (asteroids/start))
  (testing "Mandelbrot"
    (mandelbrot/start))
  (testing "Convolution"
    (convolution/start))
  (testing "Brian's Brains"
    (brian/start)))