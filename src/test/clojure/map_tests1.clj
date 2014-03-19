(ns map-tests1
  (:use [clojure.test :only [deftest is function?]])
  (:require
   [clojure-contracts-maps  :as cma :refer (define-map-get-accessor define-map-put-accessor define-map-accessors)]   [clojure-carp  :as carp :refer (surprise-exception)]
   [clojure-potrubi.utils.collections :as potrubi-utils-collections :refer (to-vector)]
   [clojure-potrubi.traces.trace  :as trace :refer (trace-mark trace-value trace-value-entr trace-value-exit macro-set-trace trace-configure)]
   
   [map-tests-get-data1  :as get-data1
    :refer (data-accessor-get-working
            data-accessor-get-dev
            data-accessor-get-none)]

   [map-tests-put-data1  :as put-data1
    :refer (data-accessor-put-working
            data-accessor-put-dev
            data-accessor-put-none)]

   [map-tests-both-data1  :as both-data1
    :refer (data-accessor-both-working
            data-accessor-both-dev
            data-accessor-both-none)]
   
   [map-tests-utils  :as utils
    :refer (prepare-args
            mapper-get-keys-as-set
            trace-get-accessor
            trace-put-accessor
            mapper-get-identity
            mapper-put-identity
            mapper-get-number-plus-7
            monitor-put-key-value-36
            monitor-get-key-value-1
            monitor-get-optional-arg1-telltale1
            base-fn1
            fn-test3)]))


(trace/trace-configure :first-telltale-format-specification "%-40s")

(cma/define-mnemonics
  key-mnemonics {:key-is-a-constrained-map [map? (every? keyword? (keys arg0)) (every? number? (vals arg0))]}
  map-mnemonics {:map-keyword-keys [map? (every? keyword? (keys arg0))]})

(def data-accessor-get-special [])

(def data-accessor-put-special [])

(def data-accessor-both-special [])

(def data-get-accessor-active data-accessor-get-working)
;;(def data-get-accessor-active data-accessor-get-dev)
;;(def data-get-accessor-active data-accessor-get-special)
;;(def data-get-accessor-active data-accessor-get-none)


(def data-put-accessor-active data-accessor-put-working)
;;(def data-put-accessor-active data-accessor-put-dev)
;;(def data-put-accessor-active data-accessor-put-special)
;;(def data-put-accessor-active data-accessor-put-none)

;;(def data-both-accessor-active data-accessor-both-working
;;(def data-both-accessor-active data-accessor-both-dev)
;;(def data-both-accessor-active data-accessor-both-special)
;;(def data-both-accessor-active data-accessor-both-none)


(def get-admin-keys #{:test-status
                      :return-value
                      :default
                      :input-map})

(def get-accessor-args
  (array-map :key-name nil
              :key-contract nil
              :map-contract 'map-contract
              :default 'default
              :telltale 'telltale
              :monitor 'monitor
              :monitor-args 'monitor-args
              :mapper 'mapper))


(def get-valid-keys (into #{} (concat get-admin-keys (keys  get-accessor-args))))


(def put-admin-keys #{:test-status
                      :return-value
                      :input-map
                      :input-value})


(def put-accessor-args
  (array-map :key-name nil
              :key-contract nil
              :map-contract 'map-contract
              :telltale 'telltale
              :monitor 'monitor
              :monitor-args 'monitor-args
              :mapper 'mapper))




(def put-valid-keys (into #{} (concat put-admin-keys (keys put-accessor-args))))



(deftest test-define-get-accessors1
  (doall (for [{:as opt-args} data-get-accessor-active] 
           (let [

                 _  (trace-mark "test-define-get-accessors1" "OPT-ARGS" opt-args)
                 
                 _ (assert (every? get-valid-keys (keys opt-args)))

                 test-value (get opt-args :test-value)
                 test-status (get opt-args :test-status)
                 return-value (get opt-args :return-value)
                 input-map (get opt-args :input-map)
                 input-value (get opt-args :input-value)
                 

                 args-vector (to-vector input-map)

                 get-args (prepare-args opt-args get-accessor-args)

                 _  (trace-mark "test-define-get-accessors1" "GET-ARGS" get-args)
                 
                 get-form `(define-map-get-accessor ~@get-args)

                 fn-get (eval get-form)
                 
                 
                 ]

             (trace-mark "test-define-get-accessors1" "OPT-ARGS" opt-args "GET-FORM" get-form)
             
             (condp = test-status
               :ok (is (= return-value (apply fn-get args-vector)))
               :not (is (not= return-value (apply fn-get args-vector)))
               :error (is (thrown? AssertionError (apply fn-get args-vector)))
               :exception (is (thrown? Exception (apply fn-get args-vector)))
               (surprise-exception test-status "test-status is what?"))))))




(deftest test-define-put-accessors1
  (doall (for [{:as opt-args} data-put-accessor-active]
           (let [

                 _  (trace-mark "test-define-put-accessors1" "OPT-ARGS" opt-args)

                 _ (assert (every? put-valid-keys (keys opt-args)))

                 test-value (get opt-args :test-value)
                 test-status (get opt-args :test-status)
                 return-value (get opt-args :return-value)
                 input-map (get opt-args :input-map)
                 input-value (get opt-args :input-value)

                 args-vector (list input-map input-value)


                 put-args (prepare-args opt-args put-accessor-args)

                 _  (trace-mark "test-define-put-accessors1" "PUT-ARGS" put-args)
                 
                 put-form `(define-map-put-accessor ~@put-args)

                 fn-put (eval put-form)
                 
                 ]

             (trace-mark "test-define-put-accessors1" "OPT-ARGS" opt-args "PUT-FORM" put-form)
             
             (condp = test-status
               :ok (is (= return-value (apply fn-put args-vector)))
               :not (is (not= return-value (apply fn-put args-vector)))
               :error (is (thrown? AssertionError (apply fn-put args-vector)))
               :exception (is (thrown? Exception (apply fn-put args-vector)))
               (surprise-exception test-status "test-status is what?"))))))





