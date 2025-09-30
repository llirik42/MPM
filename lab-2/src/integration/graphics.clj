(ns integration.graphics
  (:require [integration.memo :as memo]
            [integration.utils :as utils]
            [incanter.charts :as charts]
            [incanter.core :as core]))

(core/view (charts/function-plot (memo/antiderivative utils/complex 0.01) -10 10))
