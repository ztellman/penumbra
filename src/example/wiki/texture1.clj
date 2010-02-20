(ns example.wiki.texture1
  (:use [penumbra.opengl])
  (:require [penumbra.app :as app]))

(defn init [state]
  (enable :texture-2d)
  (assoc state
    :texture (load-texture-from-file "/Users/zach/Desktop/dog.jpg")))

(defn display [_ state]
  (println (:texture state))
  (blit (:texture state)))

(defn start []
  (app/start {:init init :display display} {}))