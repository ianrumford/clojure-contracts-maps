(ns clojure-contracts-maps
  (:import (java.lang Exception Class AssertionError ))
  (:require [clojure-contracts-sugar :as ccs]
            [clojure-contracts-sugar.manifests :as ccs-manifests :refer (manifest-aspect-form-key-suck manifest-aspect-form-key-spit)]
            [clojure-contracts-sugar.aspects.forms :as ccs-aspects-forms :refer (is-aspect-form? merge-aspect-forms)]
            [clojure-contracts-sugar.utils.utils :as ccs-utils :refer (to-vector)]

            [clojure-contracts-sugar.utils.walk-forms :as ccs-walkies
             :refer (normalise-walk-forms-replace-maps
                     normalise-walk-forms-keyed-replace-maps
                     is-walk-forms-replace-map?
                     is-walk-forms-replace-maps?
                     is-walk-forms-keyed-replace-map?
                     is-walk-forms-keyed-replace-maps?)]

            [clojure-contracts-sugar.utils.makros :as utils-makros :refer (define-fn-validation-from-predicate)]

            [clojure-contracts-sugar.utils.state-steps-pipeline :as utils-pipeline
             :refer (is-state-pipeline-step?
                     is-state-pipeline-state?
                     state-pipeline-get-step
                     state-pipeline-get-step-args
                     state-pipeline-get-step-args-form-or-last-result
                     state-pipeline-get-result
                     state-pipeline-add-result
                     state-step-apply-wrappers
                     state-step-define-fn-form
                     state-step-walk-keyed-forms
                     state-step-walk-form
                     state-step-apply-contract-aspects
                     state-step-ensure-result-is-vector
                     state-step-wrap-try-catch
                     state-step-call-form
                     state-pipeline-get-result-by-id
                     run-state-pipeline
                     state-summary-view)]

            [clojure-potrubi.utils.names :as potrubi-utils-names :refer (resolve-name-from-any resolve-name-from-args resolve-symbol-from-args)]
            [clojure-carp :as carp :refer (surprise-exception trace-exception contract-exception report-contract-failure)]))

;; ******************************************
;; BEG: sugar macros for mnemonic definitions
;; ******************************************

(defn- normalise-mnemonic-definition
  [mnemonic-definition]
  {:post [(vector? %) (= 1 (count %)) (vector? (first %))]}
  (let [normal-definition (cond
                           (keyword? mnemonic-definition) [[mnemonic-definition]]
                           (vector? mnemonic-definition) [mnemonic-definition]
                           :else (surprise-exception mnemonic-definition "normalise-mnemonic-definition" "mnemonic-defintion is wat?"))]
    normal-definition))

(defmacro define-mnemonics
  [& {:syms [map-mnemonics key-mnemonics] :as opt-args}]
  (let [;; no unexpected parameters?
        _ (assert (every? #{'map-mnemonics 'key-mnemonics} (keys opt-args)))

        map-definitions# (if map-mnemonics
                           (into {} (map (fn [[k v]] {k {manifest-aspect-form-key-suck (normalise-mnemonic-definition v)}}) map-mnemonics)))

        key-definitions# (if key-mnemonics
                           (into {} (map (fn [[k v]] {k {manifest-aspect-form-key-suck (normalise-mnemonic-definition v) manifest-aspect-form-key-spit (normalise-mnemonic-definition v)}}) key-mnemonics)))

        all-definitions# (merge map-definitions# key-definitions#)

        definition-form# `(ccs/configure-contracts-store ~'aspect-mnemonic-definitions ~all-definitions#)]

    `(do
       ~definition-form#
       nil)))

;; ******************************************
;; FIN: sugar macros for mnemonic definitions
;; ******************************************

;; *********************************
;; BEG: map contract and constraints
;; *********************************

(def map-contract-default-constraints ['map?])

(defn normalise-map-contract
  [map-contract]
  {:post [(is-aspect-form? %)]}
  (let [all-constraints (cond
                         (nil? map-contract) map-contract-default-constraints
                         (or (symbol? map-contract) (keyword? map-contract)) (into [] (concat map-contract-default-constraints (list map-contract)))
                         (vector? map-contract) (into [] (concat map-contract-default-constraints map-contract))
                         :else (surprise-exception map-contract "normalise-map-contract" "map-contract is wat?"))

        _ (assert (vector? all-constraints))

        map-aspect-form {manifest-aspect-form-key-suck {0 all-constraints} manifest-aspect-form-key-spit  {0 all-constraints}}]

    map-aspect-form))

(defn normalise-getter-contracts
  [key-contract map-contract]
  (let [suck-map-aspect-form (select-keys (normalise-map-contract map-contract) [manifest-aspect-form-key-suck])

        key-aspect-forms (cond
                          (or (keyword? key-contract) (symbol? key-contract) (vector? key-contract))
                          (let [vector-key-contract (to-vector key-contract)]
                            [ {:spit {0 vector-key-contract}} {:suck {1 vector-key-contract} :spit {0 vector-key-contract}}])

                          (nil? key-contract) [ {} {:suck {1 nil }}]

                          :else (surprise-exception key-contract "define-map-get-accessor" "key-contract is wat?"))

        all-aspect-forms (into [] (map
                                   (fn [form] (merge-aspect-forms suck-map-aspect-form form))
                                   key-aspect-forms))]
    all-aspect-forms))

(defn normalise-putter-contracts
  ([key-contract map-contract]
     (let [;; map contract only applied to result i.e. spit new-map
           map-aspect-form (select-keys (normalise-map-contract map-contract) [manifest-aspect-form-key-spit])

           ;; just force a signature with a map as arg0
           ;; the key contract is applied to the wrapped key value fn
           key-aspect-forms [{:suck {0 ['map?]  1 nil }}]

           all-aspect-forms (into [] (map
                                      (fn [form] (merge-aspect-forms map-aspect-form form))
                                      key-aspect-forms))]
       all-aspect-forms)))

;; *********************************
;; FIN: map contract and constraints
;; *********************************

;; ********************************
;; BEG: common state step functions
;; ********************************

(defn state-step-apply-monitor
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :valid-keys #{:form :monitor :monitor-args :monitor-optional-args :monitor-result :monitor-value}
                   :resolve-keys (list  :form)
                   :default-values {:form state-pipeline-get-result}
                   )

        source-form (get step-args :form)

        monitor (get step-args :monitor)

        monitor-form (if-not monitor
                       source-form
                       (let [monitor-args (get step-args :monitor-args)
                             _ (assert (or (nil? monitor-args) (coll? monitor-args)))

                             monitor-optional-args (get step-args :monitor-optional-args)
                             monitor-optional-args (if monitor-optional-args (to-vector monitor-optional-args))

                             ;; symbol to be the result of the monitor form
                             monitor-result (get step-args :monitor-result)
                             ;; it may be needed in the call to the monitor
                             monitor-value (get step-args :monitor-value)
                             call-form (concat (list* monitor monitor-args)  monitor-optional-args)

                             ;; let-form (if monitor-result

                             result-form (if-not monitor-value
                                           (remove nil? (list 'do call-form monitor-result))
                                           (remove nil? (list 'let [monitor-value source-form] call-form monitor-result)))]
                             result-form))

        updated-state (state-pipeline-add-result state monitor-form)]

    updated-state))

;; ********************************
;; FIN: common state step functions
;; ********************************

;; *****************************
;; BEG: get state step functions
;; *****************************

(defn state-step-make-base-form-get-body
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args state)

        ;; all keys expected?
        _ (assert (every? #{:map-name :key-name :key-leaf :dynamic-default :static-default} (keys step-args)))

        key-name (get step-args :key-name)
        key-leaf (get step-args :key-leaf)
        map-name (get step-args :map-name)
        dynamic-default (get step-args :dynamic-default)
        static-default (get step-args :static-default)

        get-base-form# (cond
                        (or (symbol? key-name) (keyword? key-name)) (list 'get map-name key-name)
                        (vector? key-name) (list 'get-in map-name key-name)
                        :else (surprise-exception key-name "key-name is what?"))

        default-form# (cond
                       (and dynamic-default static-default) (list 'or dynamic-default static-default)
                       dynamic-default dynamic-default
                       static-default static-default
                       :else nil)

        default-form# (remove nil? (list default-form#))

        key-leaf-form# (cond
                        key-leaf (list*  key-leaf (concat get-base-form# (list {})) default-form#)
                        :else (concat get-base-form# default-form#))

        form-body-get# key-leaf-form#

        updated-state (state-pipeline-add-result state form-body-get#)]

    updated-state))

(defn state-step-make-base-form-put-body
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :valid-keys #{:map-name :key-name :key-value :key-leaf}
                   :resolve-keys (list :key-value))

        key-name (get step-args :key-name)
        key-value (get step-args :key-value)
        key-leaf (get step-args :key-leaf)
        map-name (get step-args :map-name)

        put-base-form# (cond
                        (or (symbol? key-name) (keyword? key-name)) (list 'assoc map-name key-name key-value)
                        (vector? key-name) (list 'assoc-in map-name key-name key-value)
                        :else (surprise-exception key-name "key-name is what?"))

        key-leaf-form# put-base-form#

        form-body-put# key-leaf-form#

        updated-state (state-pipeline-add-result state form-body-put#)]

    updated-state))

;; *****************************
;; FIN: get state step functions
;; *****************************

;; *****************
;; BEG: get accessor
;; *****************

(def get-accessor-arglist '[default telltale monitor monitor-args mapper map-contract key-name key-contract key-leaf])
(def get-accessor-syms (into #{} get-accessor-arglist))

(def get-accessor-symbol-arg-map 'arg0)
(def get-accessor-symbol-arg-default 'arg1)
(def get-accessor-symbol-monitor-value 'monitor-value)

(def get-accessor-sig-one-arg [get-accessor-symbol-arg-map])
(def get-accessor-sig-two-arg [get-accessor-symbol-arg-map get-accessor-symbol-arg-default])

;; NOTE - these will be rewritten - hence arg0 not arg1

(def get-accessor-symbol-walk-map 'arg0)
(def get-accessor-symbol-walk-default 'arg0)

(def get-accessor-walk-symbol-maps
  {:spit {'key-value '%
          'map-value get-accessor-symbol-walk-map}
   :suck {'map-value get-accessor-symbol-walk-map
          'key-value get-accessor-symbol-walk-default}})

(defn form-map-get-accessor
  "make the form to define the get accessor
   the generated fn takes a map and, optional default value
   returns the value in the map, the dynamic default or static default"
  ([ & {:syms [default telltale monitor monitor-args mapper map-contract key-name key-leaf key-contract] :as opt-args}]

     (let [;; no unexpected arguments?
           _ (assert (every? get-accessor-syms (keys opt-args)))

           get-aspect-forms (normalise-getter-contracts key-contract map-contract)

           ;;symbol-uniq# (gensym "") ;; a number
           string-prefix# (resolve-name-from-args "getccm-" (gensym "") "-")

           symbol-fn-get# (resolve-symbol-from-args string-prefix# "fn-get")

           forms-make-fn-get# (state-pipeline-get-result
                               (run-state-pipeline
                                {}

                                {:fn state-step-make-base-form-get-body
                                 :id :one-arg-form
                                 :args {:key-name key-name
                                        :key-leaf key-leaf
                                        :map-name get-accessor-symbol-arg-map
                                        ;;;;;;;:dynamic-default get-accessor-symbol-arg-default
                                        :static-default default}}

                                {:fn state-step-make-base-form-get-body
                                 :id :two-arg-form
                                 :args {:key-name key-name
                                        :key-leaf key-leaf
                                        :map-name get-accessor-symbol-arg-map
                                        :dynamic-default get-accessor-symbol-arg-default
                                        :static-default default}}

                                {:fn state-step-apply-wrappers
                                 :id :one-arg-form-wrapped
                                 :args {:wrappers mapper
                                        :form (fn [state] (state-pipeline-get-result-by-id state :one-arg-form))}}

                                {:fn state-step-apply-wrappers
                                 :id :two-arg-form-wrapped
                                 :args {:wrappers mapper
                                        :form (fn [state] (state-pipeline-get-result-by-id state :two-arg-form))}}

                                {:fn state-step-apply-monitor
                                 :id :one-arg-form-monitor
                                 :args {:form (fn [state] (state-pipeline-get-result-by-id state :one-arg-form-wrapped))
                                        :monitor monitor
                                        :monitor-args [key-name get-accessor-symbol-monitor-value get-accessor-symbol-arg-map]
                                        :monitor-optional-args monitor-args
                                        :monitor-value get-accessor-symbol-monitor-value
                                        :monitor-result get-accessor-symbol-monitor-value
                                        }}

                                {:fn state-step-apply-monitor
                                 :id :two-arg-form-monitor
                                 :args {:form (fn [state] (state-pipeline-get-result-by-id state :two-arg-form-wrapped))
                                        :monitor monitor
                                        :monitor-args [key-name get-accessor-symbol-monitor-value get-accessor-symbol-arg-map]
                                        :monitor-optional-args monitor-args
                                        :monitor-value get-accessor-symbol-monitor-value
                                        :monitor-result get-accessor-symbol-monitor-value
                                        }}

                                {:fn state-step-define-fn-form
                                 :id :get-fn-form
                                 :args {:name symbol-fn-get#
                                        :arities {get-accessor-sig-one-arg (fn [state] (state-pipeline-get-result-by-id state :one-arg-form-monitor))
                                                  get-accessor-sig-two-arg (fn [state] (state-pipeline-get-result-by-id state :two-arg-form-monitor))}}}

                                {:fn state-step-walk-keyed-forms
                                 :id :get-constraints
                                 :args {:keyed-replace-maps get-accessor-walk-symbol-maps
                                        :keyed-forms get-aspect-forms}}

                                {:fn state-step-apply-contract-aspects
                                 :args {:form (fn [state] (state-pipeline-get-result-by-id state :get-fn-form))
                                        :aspects (fn [state] (state-pipeline-get-result-by-id state :get-constraints))}}

                                {:fn (fn [state]
                                       (if-not telltale
                                         (state-step-ensure-result-is-vector state)
                                         (let [resolved-telltale# (str telltale)
                                               symbol-fn-telltale# (resolve-symbol-from-args string-prefix# "fn-telltale")]
                                           ;; sub pipeline
                                           (run-state-pipeline
                                            state
                                            {:fn state-step-wrap-try-catch
                                             :args {
                                                    ;;:wrapper-name symbol-fn-telltale#
                                                    :target-form state-pipeline-get-result
                                                    :target-name symbol-fn-get#
                                                    :target-arities
                                                    {get-accessor-sig-one-arg `(report-contract-failure  ~get-accessor-symbol-arg-map "KEY" ~key-name "REASON" ~resolved-telltale#)
                                                     get-accessor-sig-two-arg `(report-contract-failure  ~get-accessor-symbol-arg-map "KEY" ~key-name "DEFAULT" ~get-accessor-symbol-arg-default "REASON" ~resolved-telltale#)}}
                                             }))))}

                                {:fn state-step-ensure-result-is-vector}

                                ))

           ;; stop here

           forms-get-final# forms-make-fn-get#]

       forms-get-final#)))

(defmacro define-canonical-map-get-accessor
  "define the get function
   the generated fn takes a map and, optional default value
   returns the value in the map, the dynamic default or static default"
  ([ & {:as opt-args}]
     (let [forms-get# (apply form-map-get-accessor (apply concat opt-args))]
       `(do
          ~@(for [c# forms-get#] c#)))))

(defmacro define-map-get-accessor
  "define the get function
   the generated fn takes a map and, optional default value
   returns the value in the map, the dynamic default or static default"
  ([key-name key-contract & {:as opt-args}]
     (let [all-args# (assoc opt-args 'key-name key-name 'key-contract key-contract)
           form-get# (list* `define-canonical-map-get-accessor (apply concat all-args#))]
       `(do
          ~form-get#))))

(defmacro compose-map-get-accessor
  "composes the get accessor using a pre-created leaf function
   the generated fn takes a map and, optional default value
   returns the value in the map, the dynamic default or static default"
  ([key-name key-leaf & {:syms [default telltale monitor mapper map-contract key-contract] :as opt-args}]
     (let [compose-args# (assoc opt-args 'key-name key-name 'key-leaf key-leaf)
           form-get# (list* `define-canonical-map-get-accessor (apply concat compose-args#))]
       `(do
          ~form-get#))))

;; *****************
;; FIN: get accessor
;; *****************

;; *****************
;; BEG: put accessor
;; *****************

(def put-accessor-arglist '[telltale monitor monitor-args mapper map-contract key-name key-contract key-leaf])
(def put-accessor-syms (into #{} put-accessor-arglist))

(def put-accessor-symbol-arg-map 'arg0)
(def put-accessor-symbol-new-map 'new-map)
(def put-accessor-symbol-update-map-form 'update-map-form)
(def put-accessor-symbol-key-value 'arg1)
(def put-accessor-symbol-arg-map-key-value 'arg-map-key-value)
(def put-accessor-symbol-wrapped-value 'wrapped-value)
(def put-accessor-symbol-wrapped-value-form 'wrapped-value-form)
(def put-accessor-symbol-monitor-form 'monitor-form)

(def put-accessor-sig-two-arg [put-accessor-symbol-arg-map put-accessor-symbol-key-value])

(def put-accessor-symbol-walk-map 'arg0)
(def put-accessor-symbol-walk-default 'arg0)

(def put-accessor-walk-symbol-maps
  {:spit {'key-value '%
          'map-value put-accessor-symbol-walk-map}
   :suck {'map-value put-accessor-symbol-walk-map}})

(defn form-map-put-accessor
  "make the form to define the put accessor
   returns the form"
  ([ & {:syms [default telltale monitor monitor-args mapper map-contract key-name key-leaf key-contract] :as opt-args}]

     (let [;; no unexpected arguments?
           _ (assert (every? put-accessor-syms (keys opt-args)))

           key-aspect-forms [{:suck {0 nil} :spit {0 (to-vector key-contract)}}]

           put-aspect-forms (normalise-putter-contracts key-contract map-contract)

            string-prefix# (resolve-name-from-args "putccm-" (gensym "") "-")

           symbol-fn-put# (resolve-symbol-from-args string-prefix# "fn-put")

           forms-make-fn-put# (state-pipeline-get-result
                               (run-state-pipeline
                                {}

                                ;; apply the wrappers to the key-value
                                {:fn state-step-apply-wrappers
                                 :id :key-value-wrapped
                                 :args {:wrappers mapper
                                        :form put-accessor-symbol-arg-map-key-value
                                        }}

                                ;; create an anonymous function the key-contract can be applied to
                                {:fn state-step-define-fn-form
                                 :id :key-target-fn
                                 :args {:arities {put-accessor-symbol-arg-map-key-value state-pipeline-get-result}}}

                                {:fn state-step-walk-keyed-forms
                                 :id :key-target-constraints
                                 :args {:keyed-replace-maps put-accessor-walk-symbol-maps
                                        :keyed-forms key-aspect-forms}}

                                ;; apply contract to the key fn
                                {:fn state-step-apply-contract-aspects
                                 :id :key-ctx-fn
                                 :args {
                                        :form (fn [state] (state-pipeline-get-result-by-id state :key-target-fn))
                                        :aspects (fn [state] (state-pipeline-get-result-by-id state :key-target-constraints))}}

                                ;; call the key target fn to get wrapped value with contract applied
                                {:fn state-step-call-form
                                 :id :wrap-value
                                 :args {
                                        :form-args [put-accessor-symbol-key-value]
                                        }}

                                ;; update the map
                                {:fn state-step-make-base-form-put-body
                                 :id :update-map
                                 :args {:key-name key-name
                                        :key-leaf key-leaf
                                        :map-name put-accessor-symbol-arg-map
                                        :key-value put-accessor-symbol-wrapped-value}}

                                {:fn state-step-apply-monitor
                                 :id :call-monitor
                                 :args {
                                        :form put-accessor-symbol-new-map
                                        ;;:form nil
                                        :monitor monitor
                                        :monitor-args [key-name put-accessor-symbol-wrapped-value put-accessor-symbol-arg-map put-accessor-symbol-new-map]
                                        :monitor-result put-accessor-symbol-new-map
                                        }}

                                ;; create the let form for the put fn
                                {:fn state-step-walk-form
                                 :id :put-let-form
                                 :args {:replace-map
                                        {put-accessor-symbol-wrapped-value-form (fn [state] (state-pipeline-get-result-by-id state :wrap-value))
                                         put-accessor-symbol-update-map-form (fn [state] (state-pipeline-get-result-by-id state :update-map))
                                         put-accessor-symbol-monitor-form (fn [state] (state-pipeline-get-result-by-id state :call-monitor))}
                                        :form `(let [~put-accessor-symbol-wrapped-value ~put-accessor-symbol-wrapped-value-form
                                                     ~put-accessor-symbol-new-map ~put-accessor-symbol-update-map-form]
                                                 ~put-accessor-symbol-monitor-form
                                                 ;;~put-accessor-symbol-new-map

                                                 )}}

                                ;; target fn for map contract
                                {:fn state-step-define-fn-form
                                 :id :put-target-fn
                                 :args {:name symbol-fn-put#
                                        :arities {put-accessor-sig-two-arg (fn [state] (state-pipeline-get-result-by-id state :put-let-form))}}}

                                ;; constraints for the complete put accessor
                                {:fn state-step-walk-keyed-forms
                                 :id :put-target-constraints
                                 :args {:keyed-replace-maps put-accessor-walk-symbol-maps
                                        :keyed-forms put-aspect-forms }}

                                ;; apply contract to the put fn
                                {:fn state-step-apply-contract-aspects
                                 :id :put-ctx-fn
                                 :args {
                                        :form (fn [state] (state-pipeline-get-result-by-id state :put-target-fn))
                                        :aspects (fn [state] (state-pipeline-get-result-by-id state :put-target-constraints))}}

                                {:fn state-step-ensure-result-is-vector}

                                ))

           ;; stop here

           forms-put-final# forms-make-fn-put#]

       forms-put-final#)))

(defmacro define-canonical-map-put-accessor
  "define the put function
   the generated fn takes a map and, optional default value
   returns the value in the map, the dynamic default or static default"
  ([ & {:as opt-args}]
     (let [forms-put# (apply form-map-put-accessor (apply concat opt-args))]
       `(do
          ~@(for [c# forms-put#] c#)))))

(defmacro define-map-put-accessor
  "define the put function
   the generated fn takes a map and the new value for the key
   returns the updated (new) map"
  ([key-name key-contract & {:as opt-args}]
     (let [all-args# (assoc opt-args 'key-name key-name 'key-contract key-contract)
           form-put# (list* `define-canonical-map-put-accessor (apply concat all-args#))]
       `(do
          ~form-put#))))

(defmacro compose-map-put-accessor
  "composes the put accessor using a pre-created leaf function
   the generated fn takes a map and new value for the key
   returns the updated (new) map"
  ([key-name key-leaf & {:syms [default telltale monitor mapper map-contract key-contract] :as opt-args}]
     (let [compose-args# (assoc opt-args 'key-name key-name 'key-leaf key-leaf)
           form-put# (list* `define-canonical-map-put-accessor (apply concat compose-args#))]
       `(do
          ~form-put#))))

;; *****************
;; FIN: put accessor
;; *****************

;; ****************************************
;; BEG: put and get accessors - convenience
;; ****************************************

(def put-and-get-accessor-arglist '[
                                    key-name

                                    telltale monitor monitor-args mapper map-contract key-contract
                                    get-telltale get-monitor get-monitor-args get-mapper get-map-contract get-key-contract
                                    put-telltale put-monitor put-monitor-args put-mapper put-map-contract put-key-contract

                                    get-key-leaf
                                    put-key-leaf

                                    name put-name get-name

                                    default]
)

(def put-and-get-accessor-syms (into #{} put-and-get-accessor-arglist))

(def common-parameter-names '[key-name key-contract])

(def get-parameters-mapping
  '{monitor [get-monitor monitor]
    monitor-args [get-monitor-args monitor-args]
    telltale [get-telltale telltale]
    key-contract [get-key-contract key-contract]
    map-contract [get-map-contract map-contract]})

(def put-parameters-mapping
  '{monitor [put-monitor monitor]
    monitor-args [put-monitor-args monitor-args]
    telltale [put-telltale telltale]
    key-contract [put-key-contract key-contract]
    map-contract [put-map-contract map-contract]})

(defn map-parameters
  [opt-args arg-map]
  {:pre [(map? opt-args) (map? arg-map)] :post [(map? %)]}
  (into {} (map
            (fn [[tgt-key src-keyz]]
              (let [src-keys (to-vector src-keyz)
                    tgt-value (reduce
                               (fn [s src-key] (or s (get opt-args src-key)))
                               nil
                               src-keys)]
                (if tgt-value [tgt-key tgt-value])))
            arg-map)))

(defn form-map-accessors
  "convenience macro to define both put and get accessors in one call"
  ([& {:as opt-args}]
     (let [;; no unexpected arguments?
           _ (assert (every? put-and-get-accessor-syms (keys opt-args)))

           work-args (dissoc opt-args 'name 'put-name 'get-name)

           common-parameters (select-keys work-args common-parameter-names)

           get-specific-parameters (map-parameters work-args get-parameters-mapping)
           put-specific-parameters (map-parameters work-args put-parameters-mapping)

           get-parameters (merge common-parameters get-specific-parameters)
           put-parameters (merge common-parameters put-specific-parameters)

           key-name (get opt-args 'key-name)
           base-name (get opt-args 'name)

           name-get (or (get work-args 'get-name)
                        (and base-name (resolve-symbol-from-args "get-" (resolve-name-from-any base-name)))
                        (resolve-symbol-from-args "get-" (resolve-name-from-any key-name)))

           name-put (or (get work-args 'put-name)
                        (and base-name (resolve-symbol-from-args "put-" (resolve-name-from-any base-name)))
                        (resolve-symbol-from-args "put-" (resolve-name-from-any key-name)))

           _ (assert (symbol? name-get))
           _ (assert (symbol? name-put))

           form-get `(def ~name-get (define-canonical-map-get-accessor ~@(apply concat get-parameters)))
           form-put `(def ~name-put (define-canonical-map-put-accessor ~@(apply concat put-parameters)))

           forms-put-and-get [form-get form-put]]

       forms-put-and-get)))

(defmacro define-canonical-map-accessors
  "convenience macro to define both put and get accessors in one call"
  ([& {:as opt-args}]
     (let [forms-put-and-get# (apply form-map-accessors (apply concat opt-args))]
       `(do
          ~@(for [c# forms-put-and-get#] c#)))))

(defmacro define-map-accessors
  "convenience macro to define both put and get accessors in one call"
  ([key-name key-contract  & {:as opt-args}]
     (let [all-args# (assoc opt-args 'key-name key-name 'key-contract key-contract)
           forms-put-and-get# (list* `define-canonical-map-accessors (apply concat all-args#))]
       `(do
          ~forms-put-and-get#))))

(defmacro define-map-put-and-get-accessors
  "convenience macro to define both put and get accessors in one call"
  ([accessor-name key-name key-contract  & {:as opt-args}]
     (let [all-args# (assoc opt-args 'name accessor-name  'key-name key-name 'key-contract key-contract)
           forms-put-and-get# (list* `define-canonical-map-accessors (apply concat all-args#))]
       `(do
          ~forms-put-and-get#))))

;; ****************************************
;; FIN: put and get accessors - convenience
;; ****************************************

