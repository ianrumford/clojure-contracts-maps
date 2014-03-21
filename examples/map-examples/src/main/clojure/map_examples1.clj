(ns map-examples1
  (:require [clojure-contracts-maps :as cma
             :refer (define-map-get-accessor
                      define-map-put-accessor
                      compose-map-get-accessor
                      define-map-accessors
                      define-mnemonics)]))

;; Helper for accessor examples expected to work.  Returns the expected result, else fails

(defn will-work
  [expected-result fn-accessor & fn-args]
  (assert (= expected-result (apply fn-accessor fn-args)))
  (println "will-work" "worked as expected" "expected-result" expected-result "fn-accessor" fn-accessor "fn-args" fn-args)
  expected-result)

;; Helper for accessor examples expected to fail.  Catches the expected AssertionError, else fails.

(defn will-fail
  [fn-accessor & fn-args]
  (try
    (do
      (apply fn-accessor fn-args)
      (assert (println "will-fail" "DID NOT FAIL" "did not cause AssertionError" "fn-accessor" fn-accessor "fn-args" fn-args)))
    (catch AssertionError e
      (println "will-fail" "failed as expected" "fn-accessor" fn-accessor "fn-args" fn-args))))

;; Wrapper to run all tests
(defn run-all-tests
 [& args]

;; Example - a getter for a key with a numeric value

;; This example shows how to define a getter function that ensures the
;; returned value of key :a is a number:
 
(def get-key-a (define-map-get-accessor :a :number))

;; Explcitly call the function

(get-key-a {:a 1})
;; =>
1

;; But lets use the will-work helper to ensure the result is as expected

(will-work 1 get-key-a {:a 1}) 
;; =>
1

;; Example - a getter with a static default for key :d
 
(def get-key-d (define-map-get-accessor :d :number default 42))

;; This will work

(will-work 42 get-key-d {})
;; =>
42

;; Note the key is present but its value of nil will fail the :number contract

(will-fail get-key-d {:d nil})

;; Example - a getter called with a dynamic, per call, default for key :d

;; The key's value, if present, alway takes precedence over any default

(will-work 55 get-key-d {:d 55} 99)
;; =>
55

;; The static default (if supplied) is used if the key is not present

(will-work 42 get-key-d {}) 
;; =>
42

;; But a per-call dynamic default take precedence over the static one

(will-work 99 get-key-d {} 99)
;; =>
99

(will-work 567 get-key-d {} 567)
;; =>
567

;; Example - define a putter for the value of key :d which must be a number

(def put-key-d (define-map-put-accessor :d :number))

;; Create a new map with the new value for key :d

(def map-with-old-value-of-d {:d 99})

(def map-with-new-value-of-d (put-key-d map-with-old-value-of-d 123))

;; Using the getter on the updated map will return the new value of :d

(will-work 123 get-key-d map-with-new-value-of-d)
;; =>
123

;; The old map is of course unchanged

(will-work 99 get-key-d map-with-old-value-of-d)
;; =>
99

;; Example - a getter with a telltale
 
(def get-key-d (define-map-get-accessor :d :number default 42 telltale "The value of key :d was not a number"))

;; The call to get-key-d below will fail with an asertion error

(will-fail get-key-d {:d "value of d must be a string else will fail"}) 
;; =>  will fail with message something like:
;; Contract Failure Value >class clojure.lang.PersistentArrayMap< >{:d "value of d must be a string else will fail"}< REASON The value of key :d was not a number

;; Example - a putter with a telltale
 
(def put-key-e (define-map-put-accessor :e :string telltale "The new value of key :e was not a string"))

;; The call to put-key-e below will fail with an asertion error

(will-fail put-key-e {:e ":e is always a string"} 123) 
;; =>  will fail with message something like:
;; Contract Failure Value >class clojure.lang.PersistentArrayMap< >{:d ":e is always a string"}< REASON The new value of key :e was not a string

;; Example - a getter for a positive numeric key value

(def get-key-m (define-map-get-accessor :m [:number :pos] telltale ":m must be a positive number"))

;; This works

(will-work 3 get-key-m {:m 3})
;; =>
3

;; But this will fail

(will-fail get-key-m {:m -3})
;; => And should produce a message like:
;; Contract Failure Value >class clojure.lang.PersistentArrayMap< >{:m -3}< KEY :m REASON :m must be a positive number

;; Example - a getter to ensure a key's value is a map with keyword keys and numeric values

;; Note the constraint form uses arg0 to refer to the passed map

(def get-key-n (define-map-get-accessor :n [:map (every? keyword? (keys arg0)) (every? number? (vals arg0))] telltale ":n must be a map with keywords keys and numeric values"))

;; This works

(will-work {:a 1 :b 2 :c 3} get-key-n {:n {:a 1 :b 2 :c 3}})
;; =>
{:a 1 :b 2 :c 3}

;; But this will fail

(will-fail get-key-n {:n {"x" 1 "y" 2 "z" 3}})
;; => And should produce a message like:
;; Contract Failure Value >class clojure.lang.PersistentArrayMap< >{:n {"x" 1, "y" 2, "z" 3}}< KEY :n REASON :n must be a map with keywords keys and numeric values

;; Example - a custom predicate to ensure a map's keys are keywords and values are numeric

(defn is-map-with-keyword-keys-and-numeric-values?
  [source-map]
  {:pre [(map? source-map) (every? keyword? (keys source-map)) (every? number? (vals source-map))]}
  source-map)

;; Example - a rich getter using a custom predicate

(def get-key-p (define-map-get-accessor :p is-map-with-keyword-keys-and-numeric-values?  telltale ":p failed predicate is-map-with-keyword-keys-and-numeric-values?"))

;; This works

(will-work {:a 1 :b 2 :c 3} get-key-p {:p {:a 1 :b 2 :c 3}})
;; =>
{:a 1 :b 2 :c 3}

;; But this will fail

(will-fail get-key-p {:p {"x" 1 "y" 2 "z" 3}})
;; => And should produce a message like:
;; Contract Failure Value >class clojure.lang.PersistentArrayMap< >{:p {"x" 1, "y" 2, "z" 3}}< KEY :p REASON :p failed predicate is-map-with-keyword-keys-and-numeric-values?

;; Define a custom mnemonic map-special ensuring a map with keyword keys and numeric values.

;; Note the mnemonic is suitable for a both a getter and putter i.e it has the same *suck* and *spit* constraints

(define-mnemonics
  key-mnemonics {:key-value-is-a-map-with-numeric-values [map? (every? number? (vals arg0))]})

;; Example - a getter with a custom mnemonic for the key

;; Use the :key-value-is-a-map-with-numeric-values mnemonic for the key contract 
;; to ensure the key's value is a map with numeric values.

(def get-key-q (define-map-get-accessor :q :key-value-is-a-map-with-numeric-values telltale ":q failed contract key-value-is-a-map-with-numeric-values"))

;; This works

(will-work {:a 1 :b 2 :c 3} get-key-q {:q {:a 1 :b 2 :c 3}})
;; =>
{:a 1 :b 2 :c 3}

;; But this will fail

(will-fail get-key-q {:q {:a :one :b :two :c :three}})
;; => And should produce a message like:
;; Contract Failure Value >class clojure.lang.PersistentArrayMap< >{:q {:a :one :b :two :c :three}}< KEY :q REASON :q failed contract key-value-is-a-map-with-numeric-values

;; Example - applying a contract to the map itself

;; Define the mnemonics

(define-mnemonics
  map-mnemonics {:map-with-keyword-keys [map? (every? keyword? (keys arg0))]}

  key-mnemonics {:key-is-a-map-with-positive-numeric-values [map? (every? number? (vals arg0)) (every? pos? (vals arg0))] }) 

;; Use both contracts

(def get-key-q (define-map-get-accessor :q :key-is-a-map-with-positive-numeric-values 
                 map-contract :map-with-keyword-keys telltale ":q failed key contract key-is-a-map-with-positive-numeric-values or map contract map-with-keyword-keys"))

;; This works

(will-work {:a 1 :b 2 :c 3} get-key-q {:q {:a 1 :b 2 :c 3}})
;; =>
{:a 1 :b 2 :c 3}

;; But this will fail as the value of :a is -1

(will-fail get-key-q {:q {:a -1 :b 2 :c 3}})
;; => And should produce a message like:
;; Contract Failure Value >class clojure.lang.PersistentArrayMap< >{:q {:a -1 :b 2 :c 3}}}< KEY :q REASON :q failed key contract :key-is-a-map-with-positive-numeric-values or map contract map-with-keyword-keys

;; Example - putters with mappers

(def put-key-f (define-map-put-accessor :f :number  mapper (fn [s] (count (str s))) telltale ":f must be a number"))

;; These will work

(will-work {:f 6} put-key-f {} "6chars")
(will-work {:f 7} put-key-f {} :7chars)

;; Example - getters with mappers

;; Define some mnemonics. Note a key contract mnemonic is applied to the
;; map. Also mnemonics are composed

(define-mnemonics
  key-mnemonics {:map-with-keyword-keys [map? (every? keyword? (keys arg0))]

                 :map-with-positive-numeric-values [map? (every? number? (vals arg0)) (every? pos? (vals arg0))]

                 :key-is-a-map-with-keyword-keys-and-postive-numeric-values [:map-with-keyword-keys :map-with-positive-numeric-values]

                 :collection-of-keywords [(coll? arg0) (every? keyword? arg0)]

                 :collection-of-numeric-values [(coll? arg0) (every? number? arg0)]

                 :collection-of-positive-numeric-values [:collection-of-numeric-values (every? pos? arg0)]})


(def get-key-g (define-map-get-accessor :g
                 :key-is-a-map-with-keyword-keys-and-postive-numeric-values
                 map-contract :map-with-keyword-keys telltale ":g must be a map with keyword keys and postivie values"))

;; Define extra getters for the keys and values of :g's map,
;; both of which must be collections (coll?) of keywords or positive
;; numeric values

(def get-key-g-keys (define-map-get-accessor :g :collection-of-keywords
                      map-contract :map-with-keyword-keys mapper (fn [m] (keys m)) telltale ":g keys must be a collection"))

(def get-key-g-vals (define-map-get-accessor :g :collection-of-positive-numeric-values
                      map-contract :map-with-keyword-keys mapper (fn [m] (vals m)) telltale ":g values must be a collection"))

;; Test data

(def test-map1 {:g {:a 1 :b 2 :c 3}})
(def test-map1-g-keys (keys (:g test-map1)))
(def test-map1-g-vals (vals (:g test-map1)))

;; Test the keys

(will-work test-map1-g-keys get-key-g-keys test-map1)
;; =>
'(:a :c :b)

;; Test the values

(will-work test-map1-g-vals get-key-g-vals test-map1)
;; =>
'(1 3 2)

;; Another getter to sum the values

(def get-key-g-sum-vals (define-map-get-accessor :g :number mapper (fn [m] (apply + (vals m))) telltale ":g values sum must be a number"))

(will-work 6 get-key-g-sum-vals test-map1)
;; =>
6

;; Example - a getter with a monitor

;; This is the monitor function

(defn monitor-get-key-j
  [key-name key-value arg-map & opt-args]
  (println "monitor-get-key-j" "key-name" key-name "key-value" key-value "arg-map" arg-map "opt-args" (count opt-args) opt-args))

(def get-key-j (define-map-get-accessor :j :number monitor monitor-get-key-j monitor-args ["opt arg1" 2 :three]))

;; The getter works as usual

(will-work 456 get-key-j {:j 456})
;; =>
456
;; And should display the monitor message:
;; monitor-get-key-j key-name :j key-value 456 arg-map {:j 456} opt-args 3 (opt-arg1 2 :three)

;; Example - a putter with a monitor

;; This is the monitor function

(defn monitor-put-key-k
  [key-name key-value arg-map new-map]
  (println "monitor-put-key-k" "key-name" key-name "key-value" key-value "arg-map" arg-map "new-map" new-map))

(def put-key-k (define-map-put-accessor :k :string monitor monitor-put-key-k))

;; The putter works as usual

(will-work {:k "new value of key :k"} put-key-k {:k "old value of key :k"} "new value of key :k")
;; =>
{:k "new value of key :k"}
;; And should produce message like:
;; monitor-put-key-k key-name :j key-value 456 arg-map {:k "old value of key :k"} new-map {:k "new value of key :k"}

;; Example - Explicit Multi-Level Getters

;; This example shows how to define a getter function that ensures the
;; returned value of multi-level key [:a :b :c] is a string. It also supplies a
;; static default.

(def get-key-abc (define-map-get-accessor [:a :b :c] :string default "static default for multilevel key [:a :b :c]" telltale "The value of multilevel key [:a :b :c] must be a string"))

(will-work "value for multilevel key [:a :b :c]" get-key-abc {:a {:b {:d 4 :c "value for multilevel key [:a :b :c]"}}})
;; =>
"value for multilevel key [:a :b :c]"

;; The below will fail as [:a :b :c] is not a string

(will-fail get-key-abc {:a {:b {:d "value of [:a :b :d]" :c 99}}})
;; => message something like
;; Contract Failure Value >class clojure.lang.PersistentArrayMap< >{:a {:b 99}}< KEY :a :b REASON The value of multilevel key [:a :b :c] must be a string

;; Static Defaults work as expected
(will-work "static default for multilevel key [:a :b :c]" get-key-abc {})
;; =>
"static default for multilevel key [:a :b :c]"

;; Dynnamic Defaults work as expected
(will-work "dynamic default for multilevel key [:a :b :c]" get-key-abc {} "dynamic default for multilevel key [:a :b :c]")
;; =>
"dynamic default for multilevel key [:a :b :c]"

;; This will also work because, although the map does not have enough levels, the static default will be returned

(will-work "static default for multilevel key [:a :b :c]" get-key-abc {:a {:b "value of b is not a map so key c can not exist"}})
;; =>
"static default for multilevel key [:a :b :c]"

;; Example - Explicit Multi-Level Putters

;; This example shows how to define a putter function for the string multilevel key [:a :b]:
 
(def put-key-ab (define-map-put-accessor [:a :b] :string telltale "The value of multilevel key [:a :b] must be a string"))

(will-work {:a {:b "cd"}}  put-key-ab {:a {:b "ab"}} "cd") 
;; =>
{:a {:b "cd"}}

;; The below will fail as [:a :b] is not a string

(will-fail put-key-ab {:a {:b "ab"}} 99)
;; => message something like
;; Contract Failure Value >class clojure.lang.PersistentArrayMap< >{:a {:b "ab"}}< KEY :a :b REASON The value of multilevel key [:a :b] must be a string

;; Example - composing a getter

;; Define the leaf accessor for key :z

(def get-key-z (define-map-get-accessor :z :string default "static default for key :z" telltale "The value of :z must be a string"))

;; Compose the leaf with a keys [:a :b]

(def get-key-z-from-ab (compose-map-get-accessor [:a :b] get-key-z))

;; This will work

(will-work "multilevel key [:a :b :z] value" get-key-z-from-ab {:a {:b {:d 4 :z "multilevel key [:a :b :z] value"}}})
;; =>
"multilevel key [:a :b :z] value"

;; The below will fail as [:a :b :z] is not a string

(will-fail get-key-z-from-ab {:a {:b {:d "value of [:a :b :d]" :z 99}}})

;; Defaults are hierachical and the "leafiest" one wins

(will-work "static default for key :z" get-key-z-from-ab {})
;; =>
"static default for key :z"

(will-work "dynamic default for key :z" get-key-z-from-ab {} "dynamic default for key :z")
;; =>
"dynamic default for key :z"

;; Compose the leaf with keys [:p :q] *and* use a mapper to return a number

(def get-key-z-from-pq (compose-map-get-accessor [:p :q] get-key-z  mapper (fn [x] (count x)) key-contract :number))

(will-work 20 get-key-z-from-pq {:p {:q {:z "a string of 20 chars"}}} )
;; =>
20

;; Example - defining both accessors together

;; The base name of the accessors has been provided: "the-v-key"

(define-map-accessors :v is-map-with-keyword-keys-and-numeric-values? name the-v-key telltale ":p failed predicate is-map-with-keyword-keys-and-numeric-values?")

;; This getter will work

(will-work {:a 1 :b 2 :c 3} get-the-v-key {:v {:a 1 :b 2 :c 3}})
;; =>
{:a 1 :b 2 :c 3}

;; But this will fail as expected

(will-fail get-the-v-key {:v {"x" 1 "y" 2 "z" 3}})

;; This putter will work

(will-work {:v {:a 1 :b 2 :c 3}} put-the-v-key {} {:a 1 :b 2 :c 3})
;; =>
{:v {:a 1 :b 2 :c 3}}

;; Example - defining both accessors using the key's name to name the accessors

;; Note since no name parameters have been provided, the getter and
;; putter will derived from the key's name and be called get-x and put-x respectively.

(define-map-accessors :x is-map-with-keyword-keys-and-numeric-values?
  get-telltale "the value of :x or a default was not a map with keyword keys and numeric values"
  put-telltale "the new value of :x must be a map with keyword keys and numeric values"
  )

;; This getter will work

(will-work {:a 1 :b 2 :c 3} get-x {:x {:a 1 :b 2 :c 3}})
;; =>
{:a 1 :b 2 :c 3}

;; But this will fail as expected

(will-fail get-x {:x {"x" 1 "y" 2 "z" 3}})

;; This putter will work

(will-work {:x {:a 1 :b 2 :c 3}} put-x {} {:a 1 :b 2 :c 3})
;; =>
{:x {:a 1 :b 2 :c 3}}

;; Close the wrapper
)

;; prevent an error from lein run
(defn -main
  [& args]
  ;;(profiling/profile :info :Arithmetic (dotimes [n 1] (run-all-tests args)))
  (dotimes [n 1] (run-all-tests args)))
