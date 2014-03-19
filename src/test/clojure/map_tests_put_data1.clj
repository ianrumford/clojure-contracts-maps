(ns map-tests-put-data1
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
            
            base-fn1
            fn-test3)]
   ))



(def data-accessor-put-working
  [

   ;; simplest case - new key & value
   {:test-status :ok :key-name :a :key-contract `number? :input-value 1 :return-value {:a 1}  :input-map {}}

   ;; fail on value type
   {:test-status :error :key-name :a :key-contract :string :input-value 1 :return-value {:a 1}  :input-map {}}

   ;; overwrite existing value
   {:test-status :ok :key-name :a :key-contract :number  :input-value 1 :return-value {:a 1}  :input-map {:a 2}}

   ;; two level map - starting empty
   {:test-status :ok :key-name [:a :b] :key-contract `number? :input-value 2 :return-value {:a {:b 2}}  :input-map {}}

   ;; fails - not map
   {:test-status :error :key-name :a :key-contract `number? :input-value 1 :return-value {:a 1}  :input-map :b}
   
   ;; fails with telltale
   {:test-status :error :key-name :a :key-contract `number? :input-value "wrongtype" :return-value {:a 1}  :input-map {} :telltale "a should be number"}
   
   ;; this will fail ClassCastException becuase the value of :a is not a map
   {:test-status :exception :key-name [:a :b] :key-contract `number? :input-value 2 :return-value {:a {:b 2}}  :input-map {:a 1}}

   ;; map contracts
   {:test-status :ok :key-name :c :key-contract :string :input-map {} :input-value "c1" :return-value {:c "c1"}  :map-contract :map}
   {:test-status :ok :key-name :c :key-contract :string :input-map {} :input-value "c1" :return-value {:c "c1"}  :map-contract 'map?}
   {:test-status :ok :key-name :c :key-contract :string :input-map {} :input-value "c1" :return-value {:c "c1"}  :map-contract [:map]}
   {:test-status :ok :key-name :c :key-contract :string :input-map {} :input-value "c1" :return-value {:c "c1"}  :map-contract '[(every? keyword? (keys arg0)) (every? string? (vals arg0))]}

   {:test-status :error :key-name :c :key-contract :string :input-map {} :input-value "c1" :return-value {:c  "c1"}  :map-contract '[(every? string? (keys arg0)) (every? string? (vals arg0))]}
   
   ;; single mapper
   {:test-status :ok :key-name :a :key-contract :number :input-map {} :input-value "6chars" :mapper (fn [s] (count s) ) :return-value {:a 6} }
   {:test-status :error :key-name :a :key-contract :string :input-map {} :input-value "6chars" :mapper (fn [s] (count s) ) :return-value {:a 6} }
   {:test-status :ok :key-name :a :key-contract :set :input-map {} :input-value [:x :y :z] :mapper (fn [v] (into #{} v) ) :return-value {:a #{:x :y :z}} }
   {:test-status :error :key-name :a :key-contract :vector :input-map {} :input-value [:x :y :z] :mapper (fn [v] (into #{} v) ) :return-value {:a #{:x :y :z}} }


   ;; multiple mappers
   {:test-status :ok :key-name :a :key-contract :number :input-map {} :input-value "6chars" :mapper [(fn [c] (* c c)) (fn [s] (count s) )] :return-value {:a 36} }
   {:test-status :not :key-name :a :key-contract :number :input-map {} :input-value "6chars" :mapper [(fn [c] (* c c)) (fn [s] (count s) )] :return-value {:a 6} }
   {:test-status :ok :key-name :a :key-contract :keyword :input-map {} :input-value "6chars" :mapper [(fn [c] (get [:a :b :c :d :e :f :g :h :i :j] c)) (fn [s] (count s) )] :return-value {:a :g} }
   {:test-status :not :key-name :a :key-contract :keyword :input-map {} :input-value "6chars" :mapper [(fn [c] (get [:a :b :c :d :e :f :g :h :i :j] c)) (fn [s] (count s) )] :return-value {:a :h} }
   {:test-status :error :key-name :a :key-contract :number :input-map {} :input-value "6chars" :mapper [(fn [c] (get [:a :b :c :d :e :f :g :h :i :j] c)) (fn [s] (count s) )] :return-value {:a :g} }

   ;; monitors
   {:test-status :ok :key-name :a :key-contract :number :input-map {} :input-value "6chars" :mapper [(fn [c] (* c c)) (fn [s] (count s) )] :return-value {:a 36} :monitor `monitor-put-key-value-36}


   ;; composed
  
   
   ])

(def data-accessor-put-dev
  [

   ;; fail with telltale
   ;;{:test-status :error :key-name :a :key-contract `number? :input-value "wrongtype" :return-value {:a 1}  :input-map {} :telltale "a should be number"}

   ;;{:test-status :ok :key-name :d :key-contract `number? :input-value 123 :return-value {:d 123}  :input-map {}}
   ;;{:test-status :ok :key-name :d :key-contract `number? :input-value 123 :return-value {:d 123}  :input-map {} :telltale "key d must be a number"}

   {:test-status :ok :key-name :d :key-contract `number? :input-value 123 :return-value {:d 123}  :input-map {} :telltale "key d must be a number"}
   
   ;; two level map overwrite existing 1st level with map of 2nd level
   ;;FAILS???
   ;; this will fail ClassCastException becuase the value of :a is not a map

   ;;{:test-status :exception :key-name [:a :b] :key-contract `number? :input-value 2 :return-value {:a {:b 2}}  :input-map {:a 1}}
   ;;{:test-status :ok :key-name [:a :b] :key-contract `number? :input-value 2 :return-value {:a {:b 2}}  :input-map {:a 1}}

   ;;{:test-status :ok :key-name [:a :b] :key-contract `number? :input-value 2 :return-value {:a {:b 2}}  :input-map {:a 1} :telltale "telltale :a exist but not map"}
   
   ;;{:test-status :ok :key-name [:a :b] :key-contract `number? :input-value 2 :return-value {:a {:b 2}}  :input-map {}} ;; works though when empty start
   
   ;; FAILS??? {:test-status :ok :key-name [:a :b] :key-contract `number? :input-value 2 :return-value {:a {:b 2}}  :input-map {:a 1}}

   ;; use an explicit predicate for the constraint
   ;;{:test-status :ok :key-name :b :default "string3" :key-contract `string? :input-value [ {:a "string1"} "string2"] :return-value "string2"}

   
   ])

(def data-accessor-put-none (list))
