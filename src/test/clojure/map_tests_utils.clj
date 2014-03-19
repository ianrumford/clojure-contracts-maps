(ns map-tests-utils
  (:require
   [clojure-carp  :as carp :refer (surprise-exception)]
   [clojure-potrubi.traces.trace  :as trace :refer (trace-mark trace-value trace-value-body trace-value-entr trace-value-exit)]
   ))

(defn prepare-args
  [opt-args args-map]
  {:pre [(map? opt-args) (map? args-map)] :post [(coll? %)]}
  ;;(trace-value-entr opt-args "prepare-args"  "OPT-ARGS")
  ;;(trace-value-entr args-map "prepare-args" "ARGS-MAP" )
  (remove nil?
          (apply concat
                 (for [[arg-keyword arg-symbol] args-map :when (contains? opt-args arg-keyword)]
                   (do
                       ;;(trace-value-body arg-symbol "prepare-args" "ARG-KEYWORD" arg-keyword "ARG-SYMBOL" )
                     [arg-symbol (get opt-args arg-keyword)])))))

(defn mapper-get-keys-as-set
  [m]
  (doall (println ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MAPPER FOR SET m" m))

  (into #{} (keys m)))

(defn trace-get-accessor
  [& args]
  (doall (println "TRACE-GET-ACCESOR" "ARGS" (count args) args)))

(defn trace-put-accessor
  [& args]
  (doall (println "TRACE-PUT-ACCESOR" "ARGS" (count args) args)))

(defn mapper-get-identity
  [value]
  (doall (println "MAPPER-GET-IDENTITY" "VALUE" (class value) value))
  value)

(defn mapper-put-identity
  [value]
  (doall (println "MAPPER-PUT-IDENTITY" "VALUE" (class value) value))
  value)


(defn mapper-get-number-plus-7
  [value]
  (doall (println "MAPPER-GET-NUMBER-PLUS-7" "VALUE" (class value) value))
  (+ 7 value))

(def base-fn1
  (fn [x]
    (trace-mark "THIS IS FN1: x" (class x) x)
    x))


(def fn-test3 base-fn1)

(defn monitor-get-key-value-1
  [key-name key-value arg-map & args]
  {:pre [(= 1 key-value)]}
  (doall (println "MONITOR-GET-KEY-VALUE-1" "KEY-NAME" key-name "KEY-VALUE" key-value "ARG-MAP" arg-map  "ARGS" (count args) args))
  "ensure result is ignored")


(defn monitor-put-key-value-36
  [key-name key-value arg-map new-map & args]
  {:pre [(= 36 key-value)]}
  (doall (println "MONITOR-PUT-KEY-VALUE-36" "KEY-NAME" key-name "KEY-VALUE" key-value "ARG-MAP" arg-map "NEW-MAP" new-map "ARGS" (count args) args))
  :ensure-result-is-ignored)

(defn monitor-get-optional-arg1-telltale1
  [key-name key-value arg-map opt-arg1 & args]
  {:pre [(keyword? opt-arg1) (= :telltale1 opt-arg1)]}
  (doall (println "MONITOR-GET-OPTIONAL-ARG1-TELLTALE1" "KEY-NAME" key-name "KEY-VALUE" key-value "ARG-MAP" arg-map  "opt-arg1" opt-arg1 "ARGS" (count args) args))
  "ensure result is ignored")
