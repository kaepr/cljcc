(ns cljcc.tacky
  (:require
   [clojure.pprint :as pp]
   [instaparse.core :as insta]
   [cljcc.parser :as p]))

(def counter "Global integer counter for generating unique identifier names." (atom 0))

(defn create-identifier
  "Returns a unique identifier. Used for generating tacky variable names."
  ([]
   (create-identifier "tmp"))
  ([identifier]
   (let [n @counter
         _ (swap! counter #(+ % 1))]
     (str identifier "." n))))

(defn variable
  ([]
   {:type :variable
    :value (create-identifier)})
  ([^String identifier]
   {:type :variable
    :value (create-identifier identifier)}))

(defn constant [^String v]
  {:type :constant
   :value (Long. v)})

(defn unop-operator [^String unop]
  (condp = unop
    "~" :complement
    "-" :negate))

(defn unary-instruction [unop src dst]
  {:type :unary
   :unary-operator unop
   :dst dst
   :src src})

(defn return-instruction [val]
  {:type :return
   :val val})

(defn exp-instructions [exp]
  (when-let [expr (second exp)]
    (condp = (first expr)
      :constant {:val (constant (second expr))}
      :unop-exp (let [inner (exp-instructions (nth expr 2))
                      dst (variable)
                      src (:val inner)
                      unop (unop-operator (second (second expr)))
                      inst (unary-instruction unop  src dst)]
                  {:val dst
                   :instructions (conj (:instructions inner) inst)}))))

(defn ret-instructions [exp]
  (let [e (exp-instructions exp)
        val (:val e)
        instructions (:instructions e)]
    (conj instructions (return-instruction val))))

(defn statement-transform [_ret-keyword exp]
  {:instructions (reverse (ret-instructions exp))})

(defn tacky-generate [ast]
  (reset! counter 0)
  (insta/transform {:statement statement-transform} ast))

(comment

  (reset! counter 0)

  (pp/pprint (tacky-generate (p/parse "int main(void) {return -~-8;}")))

  (pp/pprint
   (exp-instructions [:exp [:constant "2"]]))

  (pp/pprint
   (exp-instructions [:exp [:constant "2"]]))

  (pp/pprint
   (exp-instructions [:exp [:unop-exp [:unop "-"] [:exp [:constant "2"]]]]))

  (def ex-exp
    [:exp
     [:unop-exp
      [:unop "-"]
      [:exp
       [:unop-exp
        [:unop "~"]
        [:exp [:unop-exp [:unop "-"] [:exp [:constant "8"]]]]]]]])

  (def ex-ret
    [:statement "return"
     [:exp
      [:unop-exp
       [:unop "-"]
       [:exp
        [:unop-exp
         [:unop "~"]
         [:exp [:unop-exp [:unop "-"] [:exp [:constant "8"]]]]]]]]])

  (pp/pprint
   (exp-instructions ex-exp))

  (pp/pprint
   (ret-instructions ex-ret))

  (def exprg
    "int main(void) {
     return -(~(-8));
   }")

  (pp/pprint (parse "int main(void) {return 2;}"))

  (pp/pprint (parse exprg)))
