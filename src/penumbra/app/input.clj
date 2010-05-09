;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.input
  (:use [clojure.contrib.seq :only [indexed]]
        [clojure.contrib.def :only [defvar]])
  (:require [penumbra.app.window :as window]
            [penumbra.app.event :as event])
  (:import [org.lwjgl.input Keyboard Mouse]))

;;;

(defprotocol InputHandler
  (init! [i] "Initialize the input handler.")
  (destroy! [i] "Clean up the input handlers.")
  (key-repeat! [i flag] "Sets whether a constantly pressed key triggers multiple key-press events.")
  (key-pressed? [i key] "Checks whether a key is currently pressed.")
  (button-pressed? [i button] "Checks whether a mouse button is currently presseed.")
  (mouse-location [i] "Returns the current location of the mouse.  [0 0] is the upper-left corner.")
  (handle-mouse! [i] "Handle mouse input.")
  (handle-keyboard! [i] "Mouse keyboard input."))

;;Keyboard

(defn- current-key []
  (let [char (Keyboard/getEventCharacter)
        key (Keyboard/getEventKey)
        name (Keyboard/getKeyName key)]
    [name
     (cond
      (= key Keyboard/KEY_DELETE) :delete
      (= key Keyboard/KEY_BACK) :back
      (= key Keyboard/KEY_RETURN) :return
      (= key Keyboard/KEY_ESCAPE) :escape
      (not= 0 (int char)) (str char)
      :else (-> name .toLowerCase keyword))]))

(defn- handle-keyboard [app pressed-keys]
  (Keyboard/poll)
  (loop [pressed-keys pressed-keys]
    (if (Keyboard/next)
      (let [[name key] (current-key)]
        (if (Keyboard/getEventKeyState)
          (do
            (if (Keyboard/isRepeatEvent)
              (event/publish! app :key-type key)
              (event/publish! app :key-press key))
            (recur (assoc pressed-keys name key)))
          (let [pressed-key (pressed-keys name)]
            (event/publish! app :key-type pressed-key)
            (when-not (Keyboard/isRepeatEvent)
              (event/publish! app :key-release pressed-key))
            (recur (dissoc pressed-keys name)))))
      pressed-keys)))

;;Mouse

(defn- mouse-button-name [button-idx]
  (condp = button-idx
    0 :left
    1 :right
    2 :center
    (keyword (str "mouse-" (inc button-idx)))))

(defn- handle-mouse [app mouse-buttons]
  (let [[w h] (window/size app)]
    (loop [mouse-buttons mouse-buttons]
      (Mouse/poll)
      (if (Mouse/next)
        (let [dw (Mouse/getEventDWheel)
              dx (Mouse/getEventDX), dy (- (Mouse/getEventDY))
              x (Mouse/getEventX), y (- h (Mouse/getEventY))
              button (Mouse/getEventButton)
              button? (not (neg? button))
              button-state (Mouse/getEventButtonState)]
          (when (not (zero? dw))
            (event/publish! app :mouse-wheel dw))
          (cond
           ;;mouse down/up 
           (and (zero? dx) (zero? dy) button?)
           (do
             (event/publish! app (if button-state :mouse-down :mouse-up) [x y] (mouse-button-name button))
             (if button-state
               (recur (assoc mouse-buttons (mouse-button-name button) [x y]))
               (let [loc (mouse-buttons button)]
                 (event/publish! app :mouse-click loc (mouse-button-name button))
                 (recur (dissoc mouse-buttons (mouse-button-name button))))))
           ;;mouse-move
           (and
            (empty? mouse-buttons)
            (or (not (zero? dx)) (not (zero? dy))))
           (do
             (event/publish! app :mouse-move [dx dy] [x y])
             (recur mouse-buttons))
           ;;mouse-drag
           :else
           (do
             (doseq [b (keys mouse-buttons)]
               (event/publish! app :mouse-drag [dx dy] [x y] b))
             (recur mouse-buttons))))
        mouse-buttons))))

;;;

(defn create [app]
  (let [keys (ref {})
        buttons (ref {})]
    (reify
     InputHandler
     (init! [_]
            (if (Keyboard/isCreated)
              (do
                (Keyboard/create)
                (Mouse/create))
              (dosync
               (doseq [key (keys @keys)]
                 (event/publish! app :key-release key))
               (doseq [[b loc] @buttons]
                 (event/publish! app :mouse-up loc b)
                 (event/publish! app :mouse-click loc b))
               (ref-set keys {})
               (ref-set buttons {}))))
     (destroy! [_]
               (Keyboard/destroy)
               (Mouse/destroy))
     (key-repeat! [_ flag] (Keyboard/enableRepeatEvents flag))
     (key-pressed? [_ key] ((-> @keys vals set) key))
     (button-pressed? [_ button] (@buttons button))
     (mouse-location [_] (let [[w h] (window/size app)]
                           [(Mouse/getX) (- h (Mouse/getY))]))
     (handle-mouse! [_] (dosync (alter buttons #(handle-mouse app %))))
     (handle-keyboard! [_] (dosync (alter keys #(handle-keyboard app %)))))))