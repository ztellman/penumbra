(ns tests.apps
  (:use [clojure.test])
  (:require [examples.gears :as gears])
  (:require [examples.sierpinski :as sierpinski])
  (:require [examples.render-to-texture :as rtt])
  (:require [examples.marble :as marble])
  (:require [examples.tetris :as tetris])
  (:require [examples.asteroids :as asteroids])
  (:require [examples.mandelbrot :as mandelbrot])
  (:require [examples.convolution :as convolution]))

(deftest applications
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
    (convolution/start)))