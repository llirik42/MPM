(ns integration.graphics
  (:require [integration.memo :as memo]
            [integration.lazy :as lazy]
            [integration.utils :as utils]
            [incanter.charts :as charts]
            [incanter.core :as core]))

(let [f utils/complex
      h 0.0001
      a -0.1
      b 0.1]
  (core/view (charts/function-plot (lazy/antiderivative f h) a b))
  (core/view (charts/function-plot (memo/antiderivative f h) a b)))
