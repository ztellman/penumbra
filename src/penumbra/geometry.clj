;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.geometry
  (:use [clojure.contrib.lazy-seqs :only (primes)]
        [clojure.walk :only (postwalk-replace postwalk)]
        [clojure.contrib.def :only (defmacro-)]))

;;;

(defn prime-factors
  "Returns prime factors of a number"
  ([n]
     (loop [n (int n), primes primes, factors []]
       (let [factor (first primes)]
         (cond
          (= 1 n) factors
          (zero? (rem n factor)) (recur (int (/ n factor)) primes (conj factors factor))
          :else (recur n (next primes) factors))))))

(defn rectangle
  "Returns two values [x y], where x*y == n.
   x and y should be relatively close to each other in value."
  [n]
  (let [factors   (prime-factors n)
        reordered (take (count factors) (interleave factors (reverse factors)))
        sqrt      (int (Math/sqrt n))
        divisor   (reduce #(if (>= sqrt (* %1 %2)) (* %1 %2) %1) 1 reordered)]
    [divisor (/ n divisor)]))

(defn radians
  "Transforms degrees to radians."
  [x]
  (* (/ Math/PI 180.0) (double x)))

(defn degrees
  "Transforms radians to degrees."
  [x]
  (* (/ 180.0 Math/PI) (double x)))

;;;

(defprotocol CartesianMath
  (add [a b] "Equivalent to (map + a b).")
  (sub [a b] "Equivalent to (map - a b).")
  (mul [a b] "Equivalent to (map * a b) or (map * a (repeat b)) - b may be either a vector or a number.")
  (div [a b] "Equivalent to (map / a b) or (map / a (repeat b)) - b may be either a vector or a number.")
  (dot [a b] "The dot product of two vectors.")
  (modv [a b] "Equivalent to (map mod a b)")
  (mapv- [v f] [v f rest])
  (polar [v] "Converts vector to a cartesian representation."))

(defprotocol PolarMath
  (cartesian [p]))

(defn lerp [a b t]
  (add a (mul (sub b a) t)))

(defn length-squared [v]
  (dot v v))

(defn length [v]
  (Math/sqrt (length-squared v)))

(defn normalize [v]
  (div v (length v)))

(defn mapv [f & vs]
  (if (> (count vs) 1)
    (mapv- (first vs) f (rest vs))
    (if (number? (first vs))
      (mapv- (first vs) f [])
      (mapv- (first vs) f))))

;;;

(defmacro- tag-vars [types body]
  (let [types (into {} (map (fn [[k v]] [k (with-meta k (merge (meta k) {:tag v}))]) types))]
    (->> body
         (postwalk-replace types)
         (postwalk #(if (vector? %) (postwalk-replace (zipmap (vals types) (keys types)) %) %)))))

(defmacro- extend-numbers [& body]
  `(do
     (extend-type java.lang.Double ~@body)
     (extend-type java.lang.Integer ~@body)
     (extend-type java.lang.Float ~@body)
     (extend-type clojure.lang.Ratio ~@body)))

(defrecord Polar2 [#^double theta #^double r])

(extend-numbers
 PolarMath
 (cartesian [n] (cartesian (Polar2. n 1))))

(extend-numbers
 CartesianMath
 (add [a b] (+ a b))
 (sub [a b] (- a b))
 (mul [a b] (* a b))
 (div [a b] (/ a b))
 (dot [a b] (* a b))
 (modv [a b] (mod a b))
 (mapv- [n f] (f n))
 (mapv- [n f rest] (apply f (cons n rest)))
 (polar [n] (throw (Exception. "Can't get polar coordinate for 1-D value"))))

(tag-vars
 {v Vec2}
 (defrecord Vec2 [#^double x #^double y]
   CartesianMath
   (add [_ v] (Vec2. (+ x (.x v)) (+ y (.y v))))
   (sub [_ v] (Vec2. (- x (.x v)) (- y (.y v))))
   (mul [_ b]
        (if (number? b)
          (let [b (double b)]
            (Vec2. (* x b) (* y b)))
          (let [v b]
            (Vec2. (* x (.x v)) (* y (.y v))))))
   (div [_ b]
        (if (number? b)
          (let [b (double b)]
            (Vec2. (/ x b) (/ y b)))
          (let [v b]
            (Vec2. (/ x (.x v)) (/ y (.y v))))))
   (dot [_ v] (+ (* x (.x v)) (* y (.y v))))
   (modv [_ v] (Vec2. (mod x (.x v)) (mod y (.y v))))
   (mapv- [_ f]
          (Vec2. (double (f x)) (double (f y))))
   (mapv- [v f rest]
         (let [vs (cons v rest)]
           (Vec2. (double (apply f (map #(.x #^Vec2 %) vs)))
                  (double (apply f (map #(.y #^Vec2 %) vs))))))
   (polar [v] (Polar2. (degrees (Math/atan2 y x)) (length v)))))

(tag-vars
 {p Polar2}
 (extend-type Polar2
  PolarMath
  (cartesian [p]
   (let [theta (radians (.theta p))]
     (Vec2. (* (.r p) (Math/cos theta)) (* (.r p) (Math/sin theta)))))))

(defrecord Polar3 [#^double theta #^double phi #^double r])

(tag-vars
 {v Vec3}
 (defrecord Vec3 [#^double x #^double y #^double z]
   CartesianMath
   (add [_ v] (Vec3. (+ x (.x v)) (+ y (.y v)) (+ z (.z v))))
   (sub [_ v] (Vec3. (- x (.x v)) (- y (.y v)) (- z (.z v))))
   (mul [_ b] (if (number? b)
                (let [b (double b)]
                  (Vec3. (* x b) (* y b) (* z b)))
                (let [v b]
                  (Vec3. (* x (.x v)) (* y (.y v)) (* z (.z v))))))
   (div [_ b] (if (number? b)
                (let [b (double b)]
                  (Vec3. (/ x b) (/ y b) (/ z b)))
                (let [v b]
                  (Vec3. (/ x (.x v)) (/ y (.y v)) (/ z (.z v))))))
   (dot [_ v] (+ (* x (.x v)) (* y (.y v)) (* z (.z v))))
   (modv [_ v] (Vec3. (mod x (.x v)) (mod y (.y v)) (mod z (.x v))))
   (mapv- [_ f]
          (Vec3. (double (f x)) (double (f y)) (double (f z))))
   (mapv- [v f rest]
         (let [vs (cons v rest)]
           (Vec3. (double (apply f (map #(.x #^Vec3 %) vs)))
                  (double (apply f (map #(.y #^Vec3 %) vs)))
                  (double (apply f (map #(.z #^Vec3 %) vs))))))
   (polar [v]
          (let [len (length v)]
            (Polar3. (.theta (polar (Vec2. x z)))
                     (degrees (Math/asin (/ y len)))
                     len)))))

(tag-vars
 {p Polar3}
 (extend-type Polar3
  PolarMath
  (cartesian [p]
   (let [theta (radians (.theta p))
         phi (radians (- 90 (.phi p)))
         ts (Math/sin theta)
         tc (Math/cos theta)
         ps (Math/sin phi)
         pc (Math/cos phi)
         r (.r p)]
     (Vec3. (* r ps tc) (* r pc) (* r ps ts))))))

;;;

(defn vec2
  "Creates a 2-D vector."
  [x y]
  (Vec2. x y))

(defn polar2
  "Creates a 2-D polar coordinate.
   (polar theta) => (polar theta 1)"
  ([theta] (polar2 theta 1))
  ([theta r] (Polar2. theta r)))

(defn vec3
  "Creates a 3-D vector.
   (vec3 (vec2 x y) z) => (vec3 x y z)"
  ([#^Vec2 v z] (vec3 (.x v) (.y v) z))
  ([x y z] (Vec3. x y z)))

(defn polar3
  "Creates a 3-D polar coordinate.
   (polar3 theta phi) => (polar3 theta phi 1)"
  ([theta phi] (polar3 theta phi 1))
  ([theta phi r] (Polar3. theta phi r)))

(defn vec?
  "Returns true if 'v' is a vector."
  [v]
  (or
   (instance? Vec2 v)
   (instance? Vec3 v)))

(defn polar?
  "Returns true if 'p' is a polar coordinate."
  [p]
  (or
   (instance? Polar2 p)
   (instance? Polar3 p)))

(defmethod print-method penumbra.geometry.Vec3 [v writer]
  (.write writer (str "[ x=" (.x #^Vec3 v) ", y=" (.y #^Vec3 v) ", z=" (.z #^Vec3 v) " ]")))

(defmethod print-method penumbra.geometry.Polar3 [p writer]
  (.write writer (str "[ theta=" (.theta #^Polar3 p) ", phi=" (.phi #^Polar3 p) ", r=" (.r #^Polar3 p) " ]")))

(defmethod print-method penumbra.geometry.Vec2 [v writer]
  (.write writer (str "[ x=" (.x #^Vec2 v) ", y=" (.y #^Vec2 v) " ]")))

(defmethod print-method penumbra.geometry.Polar2 [p writer]
  (.write writer (str "[ theta=" (.theta #^Polar2 p) ", r=" (.r #^Polar2 p) " ]")))

;;;

(defn cross
  "Returns the cross product of two 3-D vectors."
  [#^Vec3 a #^Vec3 b]
  (Vec3. (- (* (.y a) (.z b)) (* (.z a) (.y b)))
         (- (* (.z a) (.x b)) (* (.x a) (.z b)))
         (- (* (.x a) (.y b)) (* (.y a) (.x b)))))

;;;

(defprotocol TransformMatrix
  (transform-matrix [#^Matrix4 a #^Matrix4 b] "Returns the product of two matrices.")
  (transform [#^Matrix4 m #^Vec3 v] "Returns a vector transformed by the matrix."))

(defmacro- transform-matrix* []
  (let [index (fn [m x y] (list (symbol (str ".m" x y)) m))]
    (list*
     `Matrix4.
     (map
      (fn [[i j]]
        (list* `+ (map (fn [k] (list `* (index 'a k i) (index 'b j k))) (range 4))))
      (for [i (range 4) j (range 4)] [i j])))))

(tag-vars
 {v Vec3
  a Matrix4
  b Matrix4}
 (defrecord Matrix4 [#^double m00 #^double m10 #^double m20 #^double m30
                     #^double m01 #^double m11 #^double m21 #^double m31
                     #^double m02 #^double m12 #^double m22 #^double m32
                     #^double m03 #^double m13 #^double m23 #^double m33]
   TransformMatrix
   (transform
    [_ v]
    (Vec3. (+ (* (.x v) m00) (* (.y v) m10) (* (.z v) m20) m30)
           (+ (* (.x v) m01) (* (.y v) m11) (* (.z v) m21) m31)
           (+ (* (.x v) m02) (* (.y v) m12) (* (.z v) m22) m32)))
   (transform-matrix
    [a b]
    (transform-matrix*))))

(defn identity-matrix []
  (Matrix4.
   1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 0 1))

(defn translation-matrix [x y z]
  (Matrix4.
   1 0 0 x
   0 1 0 y
   0 0 1 z
   0 0 0 1))

(defn scaling-matrix [x y z]
  (Matrix4.
   x 0 0 0
   0 y 0 0
   0 0 z 0
   0 0 0 1))

(tag-vars
 {m Matrix4}
 (defn normal-matrix [m]
   (Matrix4.
    (.m00 m) (.m10 m) (.m20 m) 0
    (.m01 m) (.m11 m) (.m21 m) 0
    (.m02 m) (.m12 m) (.m22 m) 0
    (.m03 m) (.m13 m) (.m23 m) 1)))

(defn rotation-matrix [theta x y z]
  (let [s (Math/sin (* Math/PI (/ theta 180)))
        c (Math/cos (* Math/PI (/ theta 180)))
        t (- 1 c)]
    (Matrix4.
     (+ c (* t x x))        (- (* t x y) (* s z))   (+ (* t x z) (* s y))   0
     (+ (* t x y) (* s z))  (+ (* t y y) c)         (- (* t y z) (* s x))   0
     (- (* t x z) (* s y))  (+ (* t y z) (* s x))   (+ (* t z z) c)         0
     0                      0                       0                       1)))


