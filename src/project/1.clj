;; (ns project.1
;; ;;   (:require [incanter.charts :as charts]
;; ;;              [incanter.core :as core]
;; ;;              [incanter.stats :as stats])
;;   )
(def h 1)

(defn const [x] 1)

(defn linear [x] (* 2 x))

(defn quadratic [x] (* 3 x x))

(defn complex [x] (if (not (== x 0))
                    (* x x (/ 1 (+ x 2)) (Math/sin (/ 1 x)))
                    0))

(defn integral
   [f]
   (letfn [(integral-k
             [k]
             (if (> k 0)
               (let [f-prev (f (* h (- k 1)))
                     f-cur (f (* h k))
                     coeff (/ h 2)
                     delta (* coeff (+ f-prev f-cur))]
                 (+ (- k 1) delta))
               0))]
     (fn [x]
       (let [k (Math/round (double (/ x h)))]
         (integral-k k)))))

(println ((integral complex) 2000))
(println ((integral complex) 2000))
(println ((integral complex) 2000))
(println ((integral complex) 2000))

;; (println ((integral linear) 0))
;; (println ((integral linear) 1))
;; (println ((integral linear) 2))
;; (println ((integral linear) 3))

;; ((integral const) 10)

;; (time (integral 1000000))
;; (time (integral 1000001))
;; (time (integral 1000002))
;; (time (integral 1000003))

;; (println (Math/round (/ 10 0.12)))
;; (time (core/view (charts/function-plot integral 0 10000)))
