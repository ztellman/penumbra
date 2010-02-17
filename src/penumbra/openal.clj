;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.openal
  (:use [penumbra.openal core])
  (:import [org.lwjgl.openal AL]
           [org.newdawn.slick.openal WaveData]
           [java.nio ByteBuffer IntBuffer FloatBuffer]))

(defn create []
  (AL/create))

(defn destroy []
  (AL/destroy))

(al-import- alListener al-listener)
(al-import- alSource source-array)
(al-import- alSourcef source-f)
(al-import- alSourcei source-i)
(al-import- alGenSources gen-sources)
(al-import- alGenBuffers gen-buffers)
(al-import- alBufferData buffer-data)
(al-import- alDeleteSources delete-sources)
(al-import- alDeleteBuffers delete-buffers)

(al-import alSourcePlay play)
(al-import alSourcePause pause)
(al-import alSourceStop stop)

(defn listener [property & args]
  (al-listener (enum property) (FloatBuffer/wrap (float-array args))))

(defn source [source property & args]
  (cond
   (< 1 (count args)) (source-array source (enum property) (FloatBuffer/wrap (float-array args)))
   (integer? (first args)) (source-i source (enum property) (first args))
   :else (source-f source (enum property) (first args))))

(defn gen-source []
  (let [ary (int-array 1)]
    (gen-sources (IntBuffer/wrap ary))
    (first ary)))

(defn gen-buffer []
  (let [ary (int-array 1)]
    (gen-buffers (IntBuffer/wrap ary))
    (first ary)))

(defn bind-buffer [src buf]
  (source-i src :buffer buf))

(defn load-wav-file [path]
  (let [wav (WaveData/create (java.io.FileInputStream. path))
        buf (gen-buffer)
        src (gen-source)]
    (try
     (buffer-data buf (.format wav) (.data wav) (.samplerate wav))
     (source src :buffer buf)
     src
     (finally
      (.dispose wav)))))

(comment (play (load-wav-file "/path/to/wav/file")))

