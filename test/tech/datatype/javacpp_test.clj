(ns tech.datatype.javacpp-test
  (:require [clojure.test :refer :all]
            [tech.datatype.javacpp :as jcpp-dtype]
            [tech.datatype.base :as base]
            [think.resource.core :as resource])
  ;;required to load some of the javacpp help functions; they are small functions
  ;;compiled into each bound library.
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

(deftest typed-pointers
  (resource/with-resource-context
    (let [src-data (range 255 235 -1)
          typed-ptr (jcpp-dtype/make-typed-pointer :uint8 src-data)
          result-data (short-array 20)
          byte-data (byte-array 20)
          signed-data (range -1 -21 -1)]
      (base/copy! typed-ptr result-data)
      (is (= (vec result-data)
             (vec src-data)))
      (base/copy! typed-ptr 0 byte-data 0 20 {:unchecked? true})
      (is (= (vec signed-data)
             (vec byte-data))))))
