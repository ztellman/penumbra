;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.opencl
  (:import (com.nativelibs4java.opencl CLContext CLDevice CLQueue OpenCL4Java CLEvent CLDevice$QueueProperties))
  (:import (com.nativelibs4java.opencl.library OpenCLLibrary))
  (:use [penumbra.opencl core]))

;;;

(defn get-platforms []
  (seq (OpenCL4Java/listPlatforms)))

(defn get-devices [platform]
  (seq (.listAllDevices platform true)))

(defn get-cpu-devices [platform]
  (seq (.listCPUDevices platform true)))

(defn get-gpu-devices [platform]
  (seq (.listGPUDevices platform true)))

(defn get-device [platform]
  (or (first (get-gpu-devices platform)) (first (get-cpu-devices platform))))

(defn enable-profiling [queue enabled]
  (.setProperty queue CLDevice$QueueProperties/ProfilingEnable enabled))

(defn enable-out-of-order [queue enabled]
  (.setProperty queue CLDevice$QueueProperties/OutOfOrderExecModeEnable enabled))

(defn create-context
  ([]
     (create-context [(get-device (first (get-platforms)))]))
  ([devices]
     (OpenCL4Java/createContext (into-array* devices))))

(defmacro with-device [device & body]
  `(binding [*device* ~device]
     ~@body))

(defmacro enqueue [& body]
  `(binding [*queue* (.createQueue *device* *context*)]
     (enable-profiling *queue* *profiling*)
     ~@body
     (dosync (alter *events* (fn [x#] (conj x# (.enqueueMarker *queue*)))))))

(defmacro execute [& body]
  `(binding [*queue* (.createQueue *device* *context*)
             *events* (ref [])]
     (enable-profiling *queue* *profiling*)
     ~@body
     (if (not (empty @*events*))
       (CLEvent/waitFor (into-array* @*events*)))
     (.finish *queue*)))

(defmacro profile [& body]
  `(do
     (binding [*profiling* true]
       (enable-profiling *queue* true)
       ~@body
       (enable-profiling *queue* false))))

(defmacro with-context [context & body]
  `(binding [*context* ~context
             *device* (first (.getDevices ~context))]
     (execute
       ~@body)))

(defmacro with-empty-context [& body]
  `(with-context (create-context)
     ~@body))

;;;


