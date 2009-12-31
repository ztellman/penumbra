;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.core
  (:use [clojure.contrib.def :only [defvar]]))

(defvar *app* nil
  "Current application.")

(defvar *window* nil
  "Current window.")

(defvar *controller* nil
  "Current controller.")

(defvar *input* nil
  "Holds current input state.")

(defvar *callback-handler* nil
  "Function which routes to callbacks.")

(defvar *timer* nil
  "Timer which measures the passage of loop time.")

(defvar *hz* nil
  "Refresh rate of update-loop")

(defvar *update-stopped?* nil
  "Is update-loop stopped.")

