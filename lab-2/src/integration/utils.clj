(ns integration.utils)

(defn const [x] 1)

(defn linear [x] (* 2 x))

(defn quadratic [x] (* 3 x x))

(defn complex [x] (if (not (== x 0))
                    (* x x (/ 1 (+ x 2)) (Math/sin (/ 1 x)))
                    0))