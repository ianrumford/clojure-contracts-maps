(ns map-tests-get-data1
  (:require
   [map-tests-utils  :as utils
    :refer (

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
            fn-test3)]
   ))




(def data-accessor-get-working
  [
   ;; basic tests
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 }
   {:test-status :not :key-name :a :key-contract :number :input-map {:a 1} :return-value 2 }
   {:test-status :error :key-name :a :key-contract :number :input-map {:a "string1"} }
   {:test-status :ok :key-name :a :key-contract :string :input-map {:a "string1"} :return-value "string1"}
   {:test-status :error :key-name :b :key-contract :string :input-map {:a "string1"} :return-value "string1"}

   ;; basic tests with telltales
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 :telltale "get; :a; telltale"}
   {:test-status :not :key-name :a :key-contract :number :input-map {:a 1} :return-value 2 :telltale "get; :a; telltale"}
   {:test-status :error :key-name :a :key-contract :number :input-map {:a "string1" :telltale "get; :a; telltale"} }
   {:test-status :ok :key-name :a :key-contract :string :input-map {:a "string1"} :return-value "string1" :telltale "get; :a; telltale"}
   {:test-status :error :key-name :b :key-contract :string :input-map {:a "string1"} :return-value "string1" :telltale "get; :a; telltale"}
   
   ;; static default - not used
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 :default 99 }
   {:test-status :ok :key-name :b :key-contract :string :input-map {:a 1 :b "b1"} :return-value "b1" :default "b2"}

   ;; static default - not used; telltale
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 :default 99 :telltale "get; :a"}
   {:test-status :ok :key-name :b :key-contract :string :input-map {:a 1 :b "b1"} :return-value "b1" :default "b2" :telltale "get; :b"}
   
   ;; static default - used
   {:test-status :ok :key-name :c :key-contract :number :input-map {:a 1} :return-value 99 :default 99}
   {:test-status :ok :key-name :d :key-contract :string :input-map {:a 1 :b "b1"} :return-value "d1" :default "d1"}

   ;; static default - used; telltale
   {:test-status :ok :key-name :c :key-contract :number :input-map {:a 1} :return-value 99 :default 99 :telltale "get; :c"}
   {:test-status :ok :key-name :d :key-contract :string :input-map {:a 1 :b "b1"} :return-value "d1" :default "d1" :telltale "get; :d"}

   ;; static default - not used; wrong type
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 :default "s1" }
   {:test-status :ok :key-name :b :key-contract :string :input-map [ {:b "sb"}] :return-value "sb" :default 1}

   ;; static default - used; wrong type
   {:test-status :error :key-name :c :key-contract :number :input-map {:a 1} :return-value 1 :default "s1" }
   {:test-status :error :key-name :d :key-contract :string :input-map [ {:b "sb"}] :return-value "sb" :default 1}

   ;; dynamic default - used
   {:test-status :ok :key-name :c :key-contract :number :input-map [ {:a 1} 99] :return-value 99}
   {:test-status :ok :key-name :d :key-contract :string :input-map [ {:b "sb"} "sd"] :return-value "sd"}

   ;; dynamic default - used; wrong type
   {:test-status :error :key-name :c :key-contract :number :input-map [ {:a 1} "sa"] :return-value "sa"}
   {:test-status :error :key-name :d :key-contract :string :input-map [ {:b "sb"} 1 ] :return-value 1}

   ;; dynamic and static default - dynamic over static
   {:test-status :ok :key-name :c  :key-contract :string :input-map [ {:a 1} "dc"] :return-value "dc" :default "sc"}

   ;; explicit predicate - note the quoting (')
   {:test-status :ok :key-name :a :key-contract `number? :input-map {:a 1} :return-value 1 }
   {:test-status :ok :key-name :b :key-contract `string? :input-map [ {:b "vb"} "db"] :return-value "vb" :default "sb"}

   ;; map contracts
   {:test-status :ok :key-name :c :key-contract :string :input-map [ {:a "a1" } "dc"] :return-value "dc" :default "sc" :map-contract :map}
   {:test-status :ok :key-name :c :key-contract :string :input-map [ {:a "a1" } "dc"] :return-value "dc" :default "sc" :map-contract 'map?}
   {:test-status :ok :key-name :c :key-contract :string :input-map [ {:a "a1" } "dc"] :return-value "dc" :default "sc" :map-contract [:map]}
   {:test-status :ok :key-name :c :key-contract :string :input-map [ {:a "a1" } "dc"] :return-value "dc" :default "sc" :map-contract ['map?]}
   {:test-status :ok :key-name :c :key-contract :string :input-map [ {:a "a1" } "dc"] :return-value "dc" :default "sc" :map-contract '[:map (every? keyword? (keys arg0)) (every? string? (vals arg0))] }
   {:test-status :error :key-name :c :key-contract :string :input-map [ {:a :not-a-string} "dc"] :return-value "dc" :default "sc" :map-contract '[:map (every? keyword? (keys arg0)) (every? string? (vals arg0))] }
   {:test-status :error :key-name :c :key-contract :string :input-map [ {"not-a-keyword" "a1"} "dc"] :return-value "dc" :default "sc" :map-contract '[:map (every? keyword? (keys arg0)) (every? string? (vals arg0))] }
   
   ;; monitor
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 :monitor `trace-get-accessor}
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 :monitor `monitor-get-key-value-1}

   ;; monitor with optional args
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 :monitor `monitor-get-key-value-1 :monitor-args :anything-u-like}
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 :monitor `monitor-get-optional-arg1-telltale1 :monitor-args [:telltale1 :any-other-args]}
   
   ;; mapper - identity
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 1 :monitor `trace-get-accessor :mapper `mapper-get-identity}
   {:test-status :ok :key-name :a :key-contract :number :input-map {:a 1} :return-value 8 :monitor `trace-get-accessor :mapper `mapper-get-number-plus-7}

   ;; single mapper
   {:test-status :ok :key-name :g :key-contract :set :input-map {:g {:a 1 :b 2 :c 3}} :mapper (fn [m] (into #{} (keys m))) :return-value #{:a :b :c}}
   {:test-status :ok :key-name :g :key-contract :number :input-map {:g {:a 1 :b 2 :c 3}} :mapper (fn [m] (apply + (vals m)) ) :return-value 6}
   {:test-status :error :key-name :g :key-contract :set :input-map {:g {:a 1 :b 2 :c 3}} :mapper (fn [m] (apply + (vals m)) ) :return-value 6}

   ;; multiple mappers
   {:test-status :ok :key-name :g :key-contract :number :input-map {:g {:a 1 :b 2 :c 3}} :mapper [(fn [s] (count s)) (fn [m] (into #{} (keys m))) ] :return-value 3}
   {:test-status :error :key-name :g :key-contract :set :input-map {:g {:a 1 :b 2 :c 3}} :mapper [(fn [s] (count s)) (fn [m] (into #{} (keys m))) ] :return-value 3}
   {:test-status :ok :key-name :g :key-contract :true :input-map {:g {:a 1 :b 2 :c 3}} :mapper [(fn [c] (pos? c)) (fn [s] (count s)) (fn [m] (into #{} (keys m))) ] :return-value true}
   {:test-status :error :key-name :g :key-contract :false :input-map {:g {:a 1 :b 2 :c 3}} :mapper [(fn [c] (pos? c)) (fn [s] (count s)) (fn [m] (into #{} (keys m))) ] :return-value true}
   
   ;; rich constraints
   {:test-status :ok :key-name :a :key-contract [:number :pos] :input-map {:a 1} :return-value 1}
   {:test-status :ok :key-name :n :key-contract [:map '(every? keyword? (keys key-value)) '(every? number? (vals key-value))] :input-map {:n {:a 1 :b 2 :c 3} } :return-value {:a 1 :b 2 :c 3}}

   
   ])

(def data-accessor-get-dev [])


(def data-accessor-get-none (list))
