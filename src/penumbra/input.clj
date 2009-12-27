;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.input
  (:use [clojure.contrib.seq-utils :only [indexed]])
  (:use [clojure.contrib.def :only [defvar]])
  (:import [org.lwjgl.input Keyboard Mouse]))

(defstruct input-struct
  :keys)

(defvar *input* nil
  "Holds current input state.")

(defvar *callback-handler* nil
  "Function that receives callbacks")

(defn create []
  (struct-map input-struct
    :keys (atom #{})))

(defn init
  ([]
     (init *input*))
  ([input]
     (Keyboard/create)
     (Mouse/create)))

(defn destroy
  ([]
     (destroy *input*))
  ([input]
     (Keyboard/destroy)
     (Mouse/destroy)))

(defmacro with-input [input & body]
  `(binding [*input* ~input]
     ~@body))

;;Keyboard

(defn key-pressed? [key]
  (@(:keys *input*) key))

(defn key-repeat [enabled]
  (Keyboard/enableRepeatEvents enabled))

(defn- current-key []
  (let [char (Keyboard/getEventCharacter)
        key (Keyboard/getEventKey)
        name (Keyboard/getKeyName key)]
    (cond
     (= key Keyboard/KEY_DELETE) :delete
     (= key Keyboard/KEY_BACK) :back
     (not= 0 (int char)) (str char)
     :else (-> name .toLowerCase keyword))))

(defn handle-keyboard []
  (Keyboard/poll)
  (while
   (Keyboard/next)
   (if (Keyboard/getEventKeyState)
     (do
       (swap! (:keys *input*) #(conj % (current-key)))
       (*callback-handler* :key-press (current-key)))
     (do
       (swap! (:keys *input*) #(disj % (current-key)))
       (*callback-handler* :key-release (current-key))))))

;;Mouse

(defn- mouse-button-name [button-idx]
  (condp = button-idx
    0 :left
    1 :right
    2 :center
    (keyword (str "button" (inc button-idx)))))

(defn handle-mouse []
  (loop [buttons (vec (map #(Mouse/isButtonDown %) (range (Mouse/getButtonCount))))]
    (Mouse/poll)
    (when (Mouse/next)
      (let [dw (Mouse/getEventDWheel)
            dx (Mouse/getEventDX), dy (Mouse/getEventDY)
            x (Mouse/getEventX), y (Mouse/getEventY)
            button (Mouse/getEventButton)
            button? (not (neg? button))
            button-state (Mouse/getEventButtonState)]
        (when (not (zero? dw))
          (*callback-handler* :mouse-wheel dw))
        (cond
         ;;mouse down/up 
         (and (zero? dx) (zero? dy) button?)
         (*callback-handler* (if button-state :mouse-down :mouse-up) [x y] (mouse-button-name button))
         ;;mouse-move
         (and (not-any? identity buttons) (or (not (zero? dx)) (not (zero? dy))))
         (*callback-handler* :mouse-move [dx dy] [x y])
         ;;mouse-drag
         :else
         (doseq [button-idx (map first (filter second (indexed buttons)))]
           (*callback-handler* :mouse-drag [dx dy] [x y] (mouse-button-name button-idx))))
        (if button?
          (recur (assoc buttons button button-state))
          (recur buttons))))))

;;;

