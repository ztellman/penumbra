;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opencl.core
  (:import (com.nativelibs4java.opencl CLContext CLDevice CLQueue OpenCL4Java))
  (:import (com.nativelibs4java.opencl.library OpenCLLibrary))
  (:import (java.lang.reflect Field))
  (:import (com.sun.jna.ptr IntByReference))
  (:use [clojure.contrib.def :only (defn-memo)]))

;;;

(def *context* nil)
(def *device* nil)
(def *queue* nil)
(def *profiling* false)
(def *events* nil)
(def *cl* (OpenCLLibrary/INSTANCE))

;;;

(defn into-array* [coll]
  (let [ary (make-array (.getClass (first coll)) (count coll))]
    (dotimes [i (count coll)]
      (aset ary i (nth coll i)))
    ary))

;;;

(defn-memo enum-name
  [enum-value]
  (let [fields (seq (.. *cl* (getClass) (getFields)))]
    (.getName #^Field (some #(if (= enum-value (.get #^Field % *cl*)) % nil) fields))))

(defn-memo enum [k]
  (let [cl (str "CL_" (.. (name k) (replace \- \_) (toUpperCase)))]
    (eval `(. OpenCLLibrary ~(symbol cl)))))


