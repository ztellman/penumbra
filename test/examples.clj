;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns examples
  (:use [clojure.test])
  (:require [example.opengl.gears :as gears]
            [example.opengl.sierpinski :as sierpinski]
            [example.opengl.render-to-texture :as rtt]
            [example.opengl.marble :as marble]
<<<<<<< HEAD:test/examples.clj
=======
            [example.opengl.accumulate :as accumulate]
            [example.opengl.async :as gl-async]
>>>>>>> 47a9938... create the slate on the main thread, and call makeCurrent on the seceondary thread:test/examples.clj
            [example.game.tetris :as tetris]
            [example.game.asteroids :as asteroids]
            [example.gpgpu.mandelbrot :as mandelbrot]
            [example.gpgpu.convolution :as convolution]
            [example.gpgpu.brians-brain :as brian]))

(deftest run
<<<<<<< HEAD:test/examples.clj
=======
  '(testing "Async"
    (async/start))
  '(testing "Switch"
    (switch/start))
  '(testing "Nested"
    (nested/start))
>>>>>>> 47a9938... create the slate on the main thread, and call makeCurrent on the seceondary thread:test/examples.clj
  (testing "Gears"
    (gears/start))
  (testing "Sierpinski"
    (sierpinski/start))
  (testing "Render-to-Texture"
    (rtt/start))
  (testing "Marble"
    (marble/start))
<<<<<<< HEAD:test/examples.clj
=======
  (testing "Accumulate"
    (accumulate/start))
  (testing "Async"
    (gl-async/start))
>>>>>>> 47a9938... create the slate on the main thread, and call makeCurrent on the seceondary thread:test/examples.clj
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