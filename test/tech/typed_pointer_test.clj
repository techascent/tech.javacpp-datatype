(ns tech.typed-pointer-test
  (:require [tech.typed-pointer :as typed-pointer]
            [clojure.test :refer :all]
            [tech.datatype.base :as dtype]
            [think.resource.core :as resource])
  (:import [org.bytedeco.javacpp opencv_core]))


(println opencv_core/ACCESS_FAST)


(deftest typed-pointer-test
  (with-bindings {#'dtype/*error-on-slow-path* true}
    (resource/with-resource-context
      (let [byte-data (typed-pointer/make-pointer-of-type :uint8 [0 1 128 254 255])
            double-data (double-array 5)]
        (dtype/copy! byte-data double-data)
        (is (= [0 1 128 254 255]
               (mapv long double-data)))
        (dtype/copy! (typed-pointer/->ptr byte-data) double-data)
        (is (= [0 1 -128 -2 -1]
               (mapv long double-data)))))))
