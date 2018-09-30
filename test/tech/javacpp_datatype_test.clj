(ns tech.javacpp-datatype-test
  (:require [clojure.test :refer :all]
            [tech.javacpp-datatype :as jcpp-dtype]
            [tech.datatype.base :as base]
            [tech.datatype.marshal :as marshal]
            [think.resource.core :as resource])
  ;;required to load some of the javacpp help functions; they are small functions compiled into each
  ;;bound library.
  (:import [org.bytedeco.javacpp opencv_core]))

;;Force loading of the class to make unit tests work
(println opencv_core/ACCESS_FAST)


(deftest ensure-fast-copy-paths
  (resource/with-resource-context
    (with-bindings {#'base/*error-on-slow-path* true}
      (let [int-data (int-array [1 2 3 4 5 6])
            result (jcpp-dtype/make-pointer-of-type :float32 6)
            short-data (short-array 6)]
        (base/copy! int-data 0 result 0 6)
        (base/copy! result 0 short-data 0 6)
        (is (= [1 2 3 4 5 6]
               (vec short-data)))))))
