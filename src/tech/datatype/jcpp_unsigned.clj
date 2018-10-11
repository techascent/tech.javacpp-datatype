(ns tech.datatype.jcpp-unsigned
  "A typed pointer is an abstract concept that must satisfy concepts such as the
  get-datatype and the ->ptr interfaces.  It is logically a tuple of base pointer and
  datatype.  In this way fat pointers can support unsigned datatypes even though the
  base javacpp pointers mirror the signed jvm primitive types."
  (:require [tech.datatype.javacpp :as jcpp-dtype]
            [tech.datatype.base :as base]
            [tech.datatype.java-primitive :as primitive]
            [tech.datatype.java-unsigned :as unsigned]
            [think.resource.core :as resource]
            [clojure.core.matrix.protocols :as mp])
  (:import [org.bytedeco.javacpp Pointer]
           [java.lang.reflect Field]))


(set! *warn-on-reflection* true)


(defprotocol PToPtr
  (->ptr [item]))


(defrecord TypedPointer [ptr datatype]
  base/PDatatype
  (get-datatype [this] datatype)

  base/PAccess
  (set-value! [item offset value]
    (base/set-value! (unsigned/->typed-buffer item) offset value))
  (set-constant! [item offset value elem-count]
    (base/set-constant! (unsigned/->typed-buffer item) offset value elem-count))
  (get-value [item offset]
    (base/get-value (unsigned/->typed-buffer item) offset))

  base/PCopyRawData
  (copy-raw->item! [item dest dest-offset options]
    (base/copy-raw->item! (unsigned/->typed-buffer item) dest dest-offset options))
  mp/PElementCount
  (element-count [item] (mp/element-count ptr))

  resource/PResource
  (release-resource [this]
    (resource/release-resource ptr))

  base/PContainerType
  (container-type [this] :typed-pointer)

  PToPtr
  (->ptr [_] ptr)

  primitive/PToBuffer
  (->buffer-backing-store [item]
    (primitive/->buffer-backing-store ptr))

  primitive/PToArray
  (->array [item] nil)
  (->array-copy [item] (primitive/->array-copy (unsigned/->typed-buffer item))))


(base/add-container-conversion-fn :typed-pointer :typed-buffer
                                  (fn [dst-container-type src-data]
                                    [(unsigned/->typed-buffer src-data) 0]))

(base/add-container-conversion-fn :javacpp-ptr :typed-buffer
                                  (fn [dst-container-type src-data]
                                    [(unsigned/->typed-buffer src-data) 0]))


(extend-type Pointer
  PToPtr
  (->ptr [item] item))


(defn ->typed-pointer
  [item datatype]
  (->TypedPointer (->ptr item) datatype))


(defn make-typed-pointer
  ([datatype elem-count-or-seq options]
   (let [jvm-type (unsigned/datatype->jvm-datatype datatype)
         elem-count-or-seq (if (or (number? elem-count-or-seq)
                                   (:unchecked? options))
                             elem-count-or-seq
                             (map #(base/cast % datatype)
                                  elem-count-or-seq))]
     (->TypedPointer (jcpp-dtype/make-pointer-of-type jvm-type elem-count-or-seq
                                                      {:unchecked? true})
                     datatype)))
  ([datatype elem-count-or-seq]
   (make-typed-pointer datatype elem-count-or-seq {})))
