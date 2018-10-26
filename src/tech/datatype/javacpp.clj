(ns tech.datatype.javacpp
  (:require [tech.datatype :as dtype]
            [tech.datatype.base :as dtype-base]
            [tech.datatype.jna :as dtype-jna]
            [tech.datatype.java-primitive :as primitive]
            [tech.datatype.java-unsigned :as unsigned]
            [clojure.core.matrix.protocols :as mp]
            [tech.resource :as resource])
  (:import [org.bytedeco.javacpp
            BytePointer IntPointer LongPointer DoublePointer
            Pointer PointerPointer FloatPointer ShortPointer
            Pointer$DeallocatorReference Loader]
           [java.lang.reflect Field]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defprotocol PToPtr
  "Anything convertible to a pointer that shares the backing store.  Datatypes do not
  have to match."
  (->ptr-backing-store [item]))

;;Necessary for testing
(comment
  (import org.bytedeco.javacpp.opencv_core)
  )


;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; disable the javacpp auto-gc system.  This causes spurious OOM errors
;; and runs the GC endlessly at times when the amount of C++ memory allocated
;; is large compared to the maximum java heap size.
(System/setProperty "org.bytedeco.javacpp.nopointergc" "true")


(extend-protocol dtype-base/PDatatype
  BytePointer
  (get-datatype [ptr] :int8)
  ShortPointer
  (get-datatype [ptr] :int16)
  IntPointer
  (get-datatype [ptr] :int32)
  LongPointer
  (get-datatype [ptr] :int64)
  FloatPointer
  (get-datatype [ptr] :float32)
  DoublePointer
  (get-datatype [ptr] :float64))


(defn make-pointer-of-type
  (^Pointer  [datatype size-or-data options]
   (-> (let [ary (dtype/make-array-of-type datatype size-or-data options)]
         (condp = datatype
           :int8 (BytePointer. ^bytes ary)
           :int16 (ShortPointer. ^shorts ary)
           :int32 (IntPointer. ^ints ary)
           :int64 (LongPointer. ^longs ary)
           :float32 (FloatPointer. ^floats ary)
           :float64 (DoublePointer. ^doubles ary)))
       resource/track))
  (^Pointer [datatype size-or-data]
   (make-pointer-of-type datatype size-or-data {})))


(defn make-empty-pointer-of-type
  ^Pointer [datatype]
  (condp = datatype
    :int8 (BytePointer.)
    :int16 (ShortPointer.)
    :int32 (IntPointer.)
    :int64 (LongPointer.)
    :float32 (FloatPointer.)
    :float64 (DoublePointer.)))


(defn- get-private-field [^Class cls field-name]
  (let [^Field field (first (filter
                             (fn [^Field x] (.. x getName (equals field-name)))
                             (.getDeclaredFields cls)))]
    (.setAccessible field true)
    field))

(defonce address-field (get-private-field Pointer "address"))
(defonce limit-field (get-private-field Pointer "limit"))
(defonce capacity-field (get-private-field Pointer "capacity"))
(defonce position-field (get-private-field Pointer "position"))
(defonce deallocator-field (get-private-field Pointer "deallocator"))


(defn offset-pointer
  "Create a 'fake' temporary pointer to use in api calls.  Note this function is
threadsafe while (.position ptr offset) is not."
  ^Pointer [^Pointer ptr ^long offset]
  (let [addr (.address ptr)
        pos (.position ptr)
        retval (make-empty-pointer-of-type (dtype/get-datatype ptr))]
    ;;Do not ever set position - this will fail in most api calls as the javacpp
    ;;code for dealing with position is incorrect.
    (.set ^Field address-field retval (+ addr
                                         (* (+ pos offset)
                                            (dtype-base/datatype->byte-size
                                             (dtype-base/get-datatype ptr)))))
    retval))


(defn duplicate-pointer
  ^Pointer [^Pointer ptr]
  (let [addr (.address ptr)
        pos (.position ptr)
        limit (.limit ptr)
        capacity (.capacity ptr)
        retval (make-empty-pointer-of-type (dtype/get-datatype ptr))]
    (.set ^Field address-field retval addr)
    (.set ^Field position-field retval pos)
    (.set ^Field limit-field retval limit)
    (.set ^Field capacity-field retval capacity)
    retval))


(defn set-pointer-limit-and-capacity
  ^Pointer [^Pointer ptr ^long elem-count]
  (.set ^Field limit-field ptr elem-count)
  (.set ^Field capacity-field ptr elem-count)
  ptr)


(defn release-pointer
  [^Pointer ptr]
  (.close ptr)
  (.deallocate ptr false)
  (.set ^Field deallocator-field ptr nil))


(defn ptr->buffer
  "Get a nio buffer from the pointer to use in other places.  Note this
  function is threadsafe while a raw .asBuffer call is not!!!
  https://github.com/bytedeco/javacpp/issues/155."
  [^Pointer ptr]
  (.asBuffer (duplicate-pointer
              (->ptr-backing-store ptr))))


(extend-type Pointer
  resource/PResource
  (release-resource [ptr] (release-pointer ptr))
  dtype-base/PAccess
  (set-value! [ptr ^long offset value]
    (dtype-base/set-value!
     (dtype-jna/->typed-pointer ptr)
     offset value))
  (set-constant! [ptr offset value elem-count]
    (dtype-base/set-constant! (dtype-jna/->typed-pointer ptr) offset value elem-count))
  (get-value [ptr ^long offset]
    (dtype-base/get-value (dtype-jna/->typed-pointer ptr) offset))
  mp/PElementCount
  (element-count [ptr] (.capacity ptr))
  dtype-base/PContainerType
  ;;Do jna buffer to take advantage of faster memcpy, memset, and
  ;;other things jna datatype bindings provide.
  (container-type [ptr] :jna-buffer)
  dtype-base/PCopyRawData
  (copy-raw->item! [raw-data ary-target target-offset options]
    (dtype-base/copy-raw->item! (dtype-jna/->typed-pointer raw-data) ary-target
                                target-offset options))

  PToPtr
  (->ptr-backing-store [item] item)

  dtype-jna/PToPtr
  (->ptr-backing-store [item]
    ;;Anything convertible to a pointer is convertible to a jna ptr too.
    (let [^Pointer item-ptr (->ptr-backing-store item)]
      (dtype-jna/make-jna-pointer (.address item-ptr))))

  primitive/PToBuffer
  (->buffer-backing-store [src]
    (ptr->buffer src))

  primitive/PToArray
  (->array [src] nil)
  (->array-copy [src]
    (primitive/->array-copy (unsigned/->typed-buffer src))))


(defn make-typed-pointer
  "This module no longer has a typed pointer, function provided to ease portability
to jna system."
  [datatype elem-seq-or-count & [options]]
  (dtype-jna/make-typed-pointer datatype elem-seq-or-count options))


(defn as-jpp-pointer
  "Create a jcpp pointer that shares the backing store with the thing.
Thing must implement tech.datatype.jna/PToPtr,
tech.datatype.base/PDatatype, and clojure.core.matrix.protocols/PElementCount."
  [item]
  (when (and (satisfies? dtype-jna/PToPtr item)
             (satisfies? dtype-base/PDatatype item)
             (satisfies? mp/PElementCount item))
    (let [address (-> (dtype-jna/->ptr-backing-store item)
                      dtype-jna/pointer->address)
          jvm-dtype (-> (dtype-base/get-datatype item)
                        (unsigned/datatype->jvm-datatype))
          n-elems (dtype-base/ecount item)]
      (-> (make-empty-pointer-of-type jvm-dtype)
          (offset-pointer address)
          (set-pointer-limit-and-capacity n-elems)))))
