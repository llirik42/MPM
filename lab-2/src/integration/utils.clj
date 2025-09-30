(ns integration.utils)

(defn const [_] 1)

(defn linear [x] (* 2 x))

(defn quadratic [x] (* 3 x x))

(defn complex [x] (* (Math/sin x) (Math/sin x)))
