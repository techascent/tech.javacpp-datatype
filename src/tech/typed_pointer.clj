(ns tech.typed-pointer
  "A typed pointer is an abstract concept that must satisfy concepts such as the
  get-datatype and the ->ptr interfaces.  It is logically a tuple of base pointer and
  datatype.  In this way fat pointers can support unsigned datatypes even though the
  base javacpp pointers mirror the signed jvm primitive types."
  (:require [tech.javacpp-datatype :as jcpp-dtype]
            [tech.datatype.base :as dtype]
            [tech.datatype.java-primitive :as primitive]
            [think.resource.core :as resource]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.macros :refer [c-for]])
  (:import [org.bytedeco.javacpp BytePointer ShortPointer
            IntPointer LongPointer FloatPointer DoublePointer
            Pointer]
           [java.nio Buffer ByteBuffer ShortBuffer IntBuffer
            LongBuffer FloatBuffer DoubleBuffer]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defprotocol PToPtr
  (->ptr [item]))

(def direct-conversion-pairs
  [[:uint8 :int8]
   [:uint16 :int16]
   [:uint32 :int32]
   [:uint64 :int64]])

(def direct-conversion-map
  (->> direct-conversion-pairs
       (mapcat (fn [[x y]]
                 [[x y]
                  [y x]]))
       (into {})))

(defn direct-conversion?
  [src-dtype dst-dtype]
  (or (= src-dtype dst-dtype)
      (and (signed-datatype? src-dtype)
           (signed-datatype? dst-dtype))
      (= (direct-conversion-map src-dtype) dst-dtype)))


;;Adding in support for unsigned types
(defmacro datatype->unsigned-max
  [datatype]
  (case datatype
    :uint8 (short 0xFF)
    :uint16 (int 0xFFFF)
    :uint32 (long 0xFFFFFFFF)
    :uint64 Long/MAX_VALUE))


(defmacro check
  [compile-time-max compile-time-min runtime-val]
  `(if (or (> ~runtime-val
                ~compile-time-max)
             (< ~runtime-val
                ~compile-time-min))
     (throw (ex-info "Value out of range"
                     {:min ~compile-time-min
                      :max ~compile-time-max
                      :value ~runtime-val}))
     ~runtime-val))







(defmacro datatype->unchecked-cast-fn
  [src-dtype dst-dtype val]
  (if (= src-dtype dst-dtype)
    val
    (case dst-dtype
      :uint8 `(bit-and (unchecked-short ~val) 0xFF)
      :uint16 `(bit-and (unchecked-int ~val) 0xFFFF)
      :uint32 `(bit-and (unchecked-long ~val) 0xFFFFFFFF)
      :uint64 `(unchecked-long ~val))))


(defmacro datatype->cast-fn
  [src-dtype dst-dtype val]
  (if (or (= src-dtype dst-dtype)
          (direct-conversion? src-dtype dst-dtype))
    val
    (case dst-dtype
      :uint8 `(datatype->unchecked-cast-fn ~src-dtype ~dst-dtype (check ~(short 0xff) 0 (short ~val)))
      :uint16 `(datatype->unchecked-cast-fn ~src-dtype ~dst-dtype (check ~(int 0xffff) 0 (int ~val)))
      :uint32 `(datatype->unchecked-cast-fn ~src-dtype ~dst-dtype (check ~(long 0xffffffff) 0 (long ~val)))
      :uint64 `(datatype->unchecked-cast-fn ~src-dtype ~dst-dtype ~val))))


(defmacro jvm->unsigned
  [dst-dtype val]
  (condp = dst-dtype
    :uint8 `(unchecked-byte ~val)
    :uint16 `(unchecked-short ~val)
    :uint32 `(unchecked-int ~val)
    :uint64 `(unchecked-long ~val)
    `~val))


(defmacro unsigned-cast-macro
  [dtype]
  {:from `(fn [val#]
            (unsigned->jvm ~dtype val#))
   :to `(fn [val#]
          (jvm->unsigned ~dtype val#))})


(def unsigned-types [:uint8 :uint16 :uint32 :uint64])
(def unsigned-type-set (set unsigned-types))


(defmacro unsigned-scalar-conversion-table-macro
  []
  (->> unsigned-types
       (map (fn [dtype]
              [dtype
               `(unsigned-cast-macro ~dtype)]))
       (into {})))


(def unsigned-scalar-conversion-table
  (unsigned-scalar-conversion-table-macro))


(defrecord TypedPointer [^Pointer ptr datatype]
  dtype/PDatatype
  (get-datatype [this] datatype)

  dtype/PAccess
  (set-value! [item offset value]
    (let [conv-fn (get-in unsigned-scalar-conversion-table [datatype :to])
          value (if conv-fn (conv-fn value) value)]
      (dtype/set-value! ptr offset value)))
  (set-constant! [item offset value elem-count]
    (let [conv-fn (get-in unsigned-scalar-conversion-table [datatype :to])
          value (if conv-fn (conv-fn value) value)]
      (dtype/set-constant! ptr offset value elem-count)))
  (get-value [item offset]
    (let [conv-fn (get-in unsigned-scalar-conversion-table [datatype :from])]
      (if conv-fn
        (conv-fn (dtype/get-value ptr offset))
        (dtype/get-value ptr offset))))

  dtype/PCopyRawData
  (copy-raw->item! [item dest dest-offset options]
    ()
    (dtype/copy! item 0 dest dest-offset (mp/element-count item) options)
    [dest (+ (long dest-offset) (long (mp/element-count item)))])

  mp/PElementCount
  (element-count [item] (mp/element-count ptr))

  resource/PResource
  (release-resource [this]
    (resource/release-resource ptr))

  dtype/PContainerType
  (container-type [this] :typed-pointer)

  PToPtr
  (->ptr [_] ptr))


(defn ->typed-pointer
  ^TypedPointer [item]
  (->TypedPointer (->ptr item)
                  (dtype/get-datatype item)))


(defmacro check-type
  [dtype val]
  `(let [val# ~val]
     (when-not (instance? ~dtype val#)
       (throw (ex-info "Point is not desired type"
                       {:desired-type ~dtype
                        :actual-type (type val#)})))
     val#))


(defn typed-ptr->byte-ptr
  ^BytePointer [^TypedPointer typed-ptr]
  (check-type BytePointer (->ptr typed-ptr)))

(defn typed-ptr->short-ptr
  ^ShortPointer [^TypedPointer typed-ptr]
  (check-type ShortPointer (->ptr typed-ptr)))

(defn typed-ptr->int-ptr
  ^IntPointer [^TypedPointer typed-ptr]
  (check-type IntPointer (->ptr typed-ptr)))

(defn typed-ptr->long-ptr
  ^LongPointer [^TypedPointer typed-ptr]
  (check-type LongPointer (->ptr typed-ptr)))

(defn typed-ptr->float-ptr
  ^FloatPointer [^TypedPointer typed-ptr]
  (check-type FloatPointer (->ptr typed-ptr)))

(defn typed-ptr->double-ptr
  ^DoublePointer [^TypedPointer typed-ptr]
  (check-type DoublePointer (->ptr typed-ptr)))


(defmacro typed-ptr->ptr
  [dtype typed-ptr]
  (condp = dtype
    :int8 `(typed-ptr->byte-ptr ~typed-ptr)
    :uint8 `(typed-ptr->byte-ptr ~typed-ptr)
    :int16 `(typed-ptr->short-ptr ~typed-ptr)
    :uint16 `(typed-ptr->short-ptr ~typed-ptr)
    :int32 `(typed-ptr->int-ptr ~typed-ptr)
    :uint32 `(typed-ptr->int-ptr ~typed-ptr)
    :int64 `(typed-ptr->long-ptr ~typed-ptr)
    :uint64 `(typed-ptr->long-ptr ~typed-ptr)
    :float32 `(typed-ptr->float-ptr ~typed-ptr)
    :float64 `(typed-ptr->double-ptr ~typed-ptr)))


(defn typed-ptr->byte-nio-buffer
  ^ByteBuffer [^TypedPointer typed-ptr]
  (jcpp-dtype/ptr->buffer (typed-ptr->byte-ptr typed-ptr)))

(defn typed-ptr->short-nio-buffer
  ^ShortBuffer [^TypedPointer typed-ptr]
  (jcpp-dtype/ptr->buffer (typed-ptr->short-ptr typed-ptr)))

(defn typed-ptr->int-nio-buffer
  ^IntBuffer [^TypedPointer typed-ptr]
  (jcpp-dtype/ptr->buffer (typed-ptr->int-ptr typed-ptr)))

(defn typed-ptr->long-nio-buffer
  ^LongBuffer [^TypedPointer typed-ptr]
  (jcpp-dtype/ptr->buffer (typed-ptr->long-ptr typed-ptr)))

(defn typed-ptr->float-nio-buffer
  ^FloatBuffer [^TypedPointer typed-ptr]
  (jcpp-dtype/ptr->buffer (typed-ptr->float-ptr typed-ptr)))

(defn typed-ptr->double-nio-buffer
  ^DoubleBuffer [^TypedPointer typed-ptr]
  (jcpp-dtype/ptr->buffer (typed-ptr->double-ptr typed-ptr)))


(defmacro typed-ptr->nio-buffer
  [dtype typed-ptr]
  (condp = dtype
    :int8 `(typed-ptr->byte-nio-buffer ~typed-ptr)
    :uint8 `(typed-ptr->byte-nio-buffer ~typed-ptr)
    :int16 `(typed-ptr->short-nio-buffer ~typed-ptr)
    :uint16 `(typed-ptr->short-nio-buffer ~typed-ptr)
    :int32 `(typed-ptr->int-nio-buffer ~typed-ptr)
    :uint32 `(typed-ptr->int-nio-buffer ~typed-ptr)
    :int64 `(typed-ptr->long-nio-buffer ~typed-ptr)
    :uint64 `(typed-ptr->long-nio-buffer ~typed-ptr)
    :float32 `(typed-ptr->float-nio-buffer ~typed-ptr)
    :float64 `(typed-ptr->double-nio-buffer ~typed-ptr)))


;;Build out the marshalling conversion table but only to array and back.



(def direct-conversion-pairs
  [[:uint8 :int8]
   [:uint16 :int16]
   [:uint32 :int32]
   [:uint64 :int64]])

(def direct-conversion-map
  (->> direct-conversion-pairs
       (mapcat (fn [[x y]]
                 [[x y]
                  [y x]]))
       (into {})))

(def full-datatype-list (vec (concat primitive/datatypes unsigned-types)))

(def full-conversion-sequence
  (->> (for [src-dtype full-datatype-list
             dst-dtype full-datatype-list]
         [src-dtype dst-dtype])))


(defn signed-datatype? [dtype] (not (unsigned-type-set dtype)))



(defmacro array->typed-ptr-conversion
  [src-dtype dst-dtype]
  (if (direct-conversion? src-dtype dst-dtype)
    `(fn [src-ary# src-offset# dst-buf# dst-offset# elem-count#]
       (marshal/copy! src-ary# src-offset#
                      (typed-ptr->nio-buffer ~dst-dtype dst-buf#) dst-offset#
                      elem-count#))
    `(fn [src-ary# src-offset# dst-buf# dst-offset# elem-count#]
       (let [elem-count# (long elem-count#)
             src-ary# (marshal/datatype->array-cast-fn ~src-dtype src-ary#)
             src-offset# (long src-offset#)
             dst-buf# (typed-ptr->nio-buffer ~dst-dtype dst-buf#)
             dst-offset# (long dst-offset#)]
         (c-for [idx# 0 (< idx# elem-count#) (inc idx#)]
                (.put dst-buf# (+ dst-offset# idx#)
                      (jvm->unsigned ~dst-dtype (aget src-ary#
                                                      (+ src-offset# idx#)))))))))


(defmacro typed-ptr->array-conversion
  [src-dtype dst-dtype]
  (if (direct-conversion? src-dtype dst-dtype)
    `(fn [src-buf# src-offset# dst-ary# dst-offset# elem-count#]
       (marshal/copy! (typed-ptr->nio-buffer ~src-dtype src-buf#) src-offset#
                      dst-ary# dst-offset#
                      elem-count#))
    `(fn [src-buf# src-offset# dst-ary# dst-offset# elem-count#]
       (let [elem-count# (long elem-count#)
             src-buf# (typed-ptr->nio-buffer ~src-dtype src-buf#)
             src-offset# (long src-offset#)
             dst-ary# (marshal/datatype->array-cast-fn ~dst-dtype dst-ary#)
             dst-offset# (long dst-offset#)]
         (c-for [idx# 0 (< idx# elem-count#) (inc idx#)]
                (aset dst-ary# (+ dst-offset# idx#)
                      (marshal/datatype->cast-fn
                       ~dst-dtype
                       (unsigned->jvm ~src-dtype (.get src-buf#
                                                       (+ src-offset# idx#))))))))))


(defmacro typed-ptr->typed-ptr-conversion
  [src-dtype dst-dtype]
  (if (direct-conversion? src-dtype dst-dtype)
    `(fn [src-buf# src-offset# dst-buf# dst-offset# elem-count#]
       (dtype/copy! (typed-ptr->nio-buffer ~src-dtype src-buf#) src-offset#
                    (typed-ptr->nio-buffer ~dst-dtype dst-buf#) dst-offset#
                    elem-count# {}))
    `(fn [src-buf# src-offset# dst-buf# dst-offset# elem-count#]
       (let [elem-count# (long elem-count#)
             src-buf# (typed-ptr->nio-buffer ~src-dtype src-buf#)
             src-offset# (long src-offset#)
             dst-buf# (typed-ptr->nio-buffer ~dst-dtype dst-buf#)
             dst-offset# (long dst-offset#)]
         (c-for [idx# 0 (< idx# elem-count#) (inc idx#)]
                (.put dst-buf# (+ dst-offset# idx#)
                      (jvm->unsigned
                       ~dst-dtype
                       (unsigned->jvm ~src-dtype
                                      (.get src-buf#
                                            (+ src-offset# idx#))))))))))


(defmacro build-full-conversion
  []
  {[:java-array :typed-pointer]
   (->> full-conversion-sequence
        ;;The arrays can only be the core jvm types
        (filter #(signed-datatype? (first %)))
        (map (fn [[src-dtype dst-dtype]]
               [[src-dtype dst-dtype]
                `(array->typed-ptr-conversion ~src-dtype ~dst-dtype)]))
        (into {}))
   [:typed-pointer :java-array]
   (->> full-conversion-sequence
        ;;Again, only jvm primitives for arrays
        (filter #(signed-datatype? (second %)))
        (map (fn [[src-dtype dst-dtype]]
               [[src-dtype dst-dtype]
                `(typed-ptr->array-conversion ~src-dtype ~dst-dtype)]))
        (into {}))
   [:typed-pointer :typed-pointer]
   (->> full-conversion-sequence
        (map (fn [[src-dtype dst-dtype]]
               [[src-dtype dst-dtype]
                `(typed-ptr->typed-ptr-conversion ~src-dtype ~dst-dtype)]))
        (into {}))})


(def conversion-table (build-full-conversion))

(doseq [[types conversions] conversion-table]
  (marshal/add-copy-operation (first types) (second types) conversions))


(defn make-pointer-of-type
  [dtype elem-count-or-data]
  (if (signed-datatype? dtype)
    (->TypedPointer (jcpp-dtype/make-pointer-of-type dtype elem-count-or-data) dtype)
    (let [is-num? (number? elem-count-or-data)
          elem-count (long (if is-num?
                             elem-count-or-data
                             (dtype/ecount elem-count-or-data)))
          retval (->TypedPointer (jcpp-dtype/make-pointer-of-type
                                  (direct-conversion-map dtype)
                                  elem-count)
                                 dtype)]
      (when-not is-num?
        (dtype/copy-raw->item! elem-count-or-data retval 0))
      retval)))
