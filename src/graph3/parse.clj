(ns graph3.parse
  (:require [instaparse.core :as insta]
            [clojure.string :refer [join]]))

;;;;;;;;;;;;;;;;;;;;;;;
;; text -> function  ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn- function-equal?
  ([f g] (every?
          (fn [[x y]] (= (double x) (double y)))
          ;; compare values just on the integerss in [-100 100]
          (map (fn [z] [(f z) (g z)]) (range -100 100))))
  ([f g & more]
   (every?  (fn [[f g]]
              (function-equal? f g))
            (partition 2 (list* f g more)))))

(defn- get-parser []
  (insta/parser (slurp "src/graph3/grammar")))

(defn- higher-order
  "Factory method to enable function arithmetic
  (like in functional analysys).
  op should be a function such as +,-,* etc."
  [op]
  (fn 
    ([f] f)
    ([f g] (fn [x] (op (f x)(g x))))
    ([f g & args] (reduce higher-order (higher-order f g) args))))

(defn fn-add [f & more] (apply (higher-order +) f more))

(defn fn-minus [f & more] (apply (higher-order -) f more))

(defn fn-mul [f & more] (apply (higher-order *) f more))

(defn fn-div [f & more] (apply (higher-order /) f more))

(defn- negate-fn [f] (fn [x] (- ( f x))))

(defn- monomial [power] (fn [x] (Math/pow x power)))

(defn- const [value] (constantly value))

;;;;;;;;;;;;
;; parse  ;;
;;;;;;;;;;;;

(defn- handle-op [id]
  (fn [& args]
    {:type id :items (vec args)}))

(def ^:private special-fn-table
  {"cos" #( Math/cos %)
   "sin" #(Math/sin %)
   "tan" #( Math/tan %)
   "exp" #(Math/exp %)
   "log" #(Math/log %)})

(def ^:private parse-map-base
  {:number (fn [& args] (read-string (apply str args))) 
   :const (fn [x] {:type :const :value x})
   :identity (fn [& _] {:type :identity})
   :exponential (fn [v pow] {:type :exponential :value v :power pow})
   :special-fn (fn [x v]
                 {:type :special-fn :id x :value v})})

(defn- f-op-handler [id]
  (fn [x & more]
    ;; branching based on arity
    (if more
      {:type id :items (vec (cons x more))}
      x)))

(def ^:private parse-map-f-op
  (let
      [ops [:f-add :f-minus :f-mul :f-div ]]
    (merge parse-map-base
           (zipmap ops (map f-op-handler ops))
           {:f-negate (fn [x] {:type :f-negate :value x})})))

(def ^:private parse-map
  (merge parse-map-base parse-map-f-op))

(defn- parse-string-1 [s]
  (let [p (get-parser)]
    (p s)))

(defn- parse-string
  "Parses s and apply transform to the result."
  [s]
  (->> s parse-string-1 (insta/transform parse-map)))

(defmulti data->fn :type)

(defmethod data->fn :const [{value :value}]
  (constantly value))

(defmethod data->fn :identity [_] identity)

(defn- math-power [x n] (Math/pow x n))

(defmethod data->fn :exponential[{:keys [value power]}]
  (comp (fn [x] (math-power x power)) (data->fn value)))

(defmethod data->fn :f-add [{:keys [items]}]
  (apply fn-add (map data->fn items)))

(defmethod data->fn :f-minus [{:keys [items]}]
  (apply fn-minus (map data->fn items)))

(defmethod data->fn :f-mul [{:keys [items]}]
  (apply fn-mul (map data->fn items)))

(defmethod data->fn :f-div [{:keys [items]}]
  (apply fn-div (map data->fn items)))

(defmethod data->fn :f-negate [{:keys [value]}]
  (negate-fn (data->fn value)))

(defmethod data->fn :special-fn [{:keys [id value]}]
  (let [f (special-fn-table id)]
    (comp f (data->fn value))))

(defn string->fn
  "Converts s (string) to fn, or returns nil on parse error."
  [s]
  (let [res (parse-string s)]
    (when-not (insta/failure? res)
      (-> res second data->fn))))



