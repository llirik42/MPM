(ns integration.utils)

(defn const [_] 1)

(defn linear [x] (* 2 x))

(defn quadratic [x] (* 3 x x))

(defn complex [x] (if (== x 0) 0 (Math/sin (/ 1 x))))
