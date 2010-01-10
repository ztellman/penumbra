;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.input
  (:use [clojure.contrib.seq-utils :only [indexed]]
        [clojure.contrib.def :only [defvar]]
        [penumbra.app.window :only [dimensions]]
        [penumbra.app.core])
  (:require [penumbra.app.event :as event])
  (:import [org.lwjgl.input Keyboard Mouse]))

;;;

(defn- publish! [hook & args]
  (apply event/publish! (list* (:event *app*) hook args)))

;;;


(defstruct input-struct
  :keys
  :mouse-buttons)

(defn create []
  (struct-map input-struct
    :keys (ref {})
    :mouse-buttons (ref {})))

(defn init
  ([]
     (init *input*))
  ([input]
     (Keyboard/create)
     (Mouse/create)
     (assoc input
       :keys (ref {})
       :mouse-buttons (ref {}))))

(defn resume!
  ([]
     (resume! *input*))
  ([input]
     (dosync
      (doseq [key @(:keys input)]
        (event/publish! :key-release key))
      (ref-set (:keys input) {}))))

(defn destroy
  ([]
     (destroy *input*))
  ([input]
     (Keyboard/destroy)
     (Mouse/destroy)
     input))

(defmacro with-input [input & body]
  `(binding [*input* ~input]
     ~@body))

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

(defn handle-keyboard! []
  (Keyboard/poll)
  (while (Keyboard/next)
   (let [[name key] (current-key)]
     (if (Keyboard/getEventKeyState)
       (do
         (dosync (alter (:keys *input*) #(assoc % name key)))
         (publish! :key-press key))
       (dosync
         (let [pressed-key (@(:keys *input*) name)]
           (alter (:keys *input*) #(dissoc % name key))
           (publish! :key-release pressed-key))))
     nil)))

;;Mouse

(defn- mouse-button-name [button-idx]
  (condp = button-idx
    0 :left
    1 :right
    2 :center
    (keyword (str "button" (inc button-idx)))))

(defn handle-mouse! []
  (let [[w h] (dimensions *window*)]
    (loop [buttons (vec (map #(Mouse/isButtonDown %) (range (Mouse/getButtonCount))))]
      (Mouse/poll)
      (when (Mouse/next)
        (let [dw (Mouse/getEventDWheel)
              dx (Mouse/getEventDX), dy (- (Mouse/getEventDY))
              x (Mouse/getEventX), y (- h (Mouse/getEventY))
              button (Mouse/getEventButton)
              button? (not (neg? button))
              button-state (Mouse/getEventButtonState)]
          (when (not (zero? dw))
            (publish! :mouse-wheel dw))
          (cond
           ;;mouse down/up 
           (and (zero? dx) (zero? dy) button?)
           (publish! (if button-state :mouse-down :mouse-up) [x y] (mouse-button-name button))
           ;;mouse-move
           (and (not-any? identity buttons) (or (not (zero? dx)) (not (zero? dy))))
           (publish! :mouse-move [dx dy] [x y])
           ;;mouse-drag
           :else
           (doseq [button-idx (map first (filter second (indexed buttons)))]
             (publish! :mouse-drag [dx dy] [x y] (mouse-button-name button-idx))))
          (if button?
            (recur (assoc buttons button button-state))
            (recur buttons)))))
    nil))

;;;

