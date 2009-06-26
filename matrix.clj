(ns vellum.matrix)

(defn sin [x] (Math/sin (* Math/PI (/ x 180))))
(defn cos [x] (Math/cos (* Math/PI (/ x 180))))

(defn identity-matrix []
  [1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 0 1])

(defn translation-matrix [x y z]
  [1 0 0 x
   0 1 0 y
   0 0 1 z
   0 0 0 1])

(defn scaling-matrix [x y z]
  [x 0 0 0
   0 y 0 0
   0 0 z 0
   0 0 0 1])

(defn rotation-matrix [theta x y z]
  (let [s (sin theta)
        c (cos theta)
        t (- 1 c)]
  [(+ c (* t x x))        (- (* t x y) (* s z))   (+ (* t x z) (* s y))   0
   (+ (* t x y) (* s z))  (+ (* t y y) c)         (- (* t y z) (* s x))   0
   (- (* t x z) (* s y))  (+ (* t y z) (* s x))   (+ (* t z z) c)         0
   0                      0                       0                       1]))

(defn index [m i j] (m (+ i (* j 4))))

(defn mult-matrix [a b]
  (let [indices (for [i (range 4) j (range 4)] [i j])]
    (vec
      (map
        (fn [[i j]]
          (apply +
            (for [k (range 4)]
              (* (index a k i) (index b j k)))))
        indices))))

(defn apply-matrix [m v]
  (map
    (fn [i]
      (apply +
        (map
          (fn [j] (* (v j) (index m j i)))
          (range 4))))
    (range 4)))


