(ns cljcc.analyzer
  (:require [cljcc.lexer :as l]
            [clojure.pprint :as pp]
            [cljcc.util :as u]
            [cljcc.parser :as p]))

(defn- unique-identifier
  ([] (unique-identifier "analyzer"))
  ([identifier] (u/create-identifier! identifier)))

(declare resolve-block)

(defn- resolve-exp [e mp]
  (condp = (:exp-type e)
    :constant-exp e
    :variable-exp (if (contains? mp (:identifier e))
                    (p/variable-exp-node (:name (get mp (:identifier e))))
                    (throw (ex-info "Undeclared variable seen." {:variable e})))
    :assignment-exp (let [left (:left e)
                          right (:right e)
                          op (:assignment-operator e)
                          left-var? (= :variable-exp (:exp-type left))]
                      (if left-var?
                        (p/assignment-exp-node (resolve-exp left mp)
                                               (resolve-exp right mp)
                                               op)
                        (throw (ex-info "Invalid lvalue." {:lvalue e}))))
    :binary-exp (p/binary-exp-node (resolve-exp (:left e) mp)
                                   (resolve-exp (:right e) mp)
                                   (:binary-operator e))
    :unary-exp (p/unary-exp-node (:unary-operator e) (resolve-exp (:value e) mp))
    :conditional-exp (p/conditional-exp-node (resolve-exp (:left e) mp)
                                             (resolve-exp (:middle e) mp)
                                             (resolve-exp (:right e) mp))
    (throw (ex-info "Analyzer error. Invalid expression type" {:exp e}))))

(defn- resolve-optional-exp [e var-mp]
  (if (nil? e)
    e
    (resolve-exp e var-mp)))

(defn- resolve-declaration [d mp]
  (if (and (contains? mp (:identifier d)) (:from-current-block (get mp (:identifier d))))
    (throw (ex-info "Analyzer error. Duplicate variable declaration." {:declaration d}))
    (let [ident (:identifier d)
          unique-name (unique-identifier ident)
          updated-mp (assoc mp ident {:name unique-name
                                      :from-current-block true})
          init (when (:initial d) (resolve-exp (:initial d) updated-mp))]
      (if init
        {:declaration (p/declaration-node unique-name init)
         :variable-map updated-mp}
        {:declaration (p/declaration-node unique-name)
         :variable-map updated-mp}))))

(defn- copy-variable-map [var-mp]
  (zipmap (keys var-mp)
          (map (fn [m]
                 (update m :from-current-block (fn [_] false)))
               (vals var-mp))))

(defn- resolve-for-init [for-init var-mp]
  (if (= (:type for-init) :declaration)
    (resolve-declaration for-init var-mp)
    (resolve-optional-exp for-init var-mp)))

(defn- resolve-statement [s mp]
  (condp = (:statement-type s)
    :return (p/return-statement-node (resolve-exp (:value s) mp))
    :expression (p/expression-statement-node (resolve-exp (:value s) mp))
    :if (if (:else-statement s)
          (p/if-statement-node (resolve-exp (:condition s) mp)
                               (resolve-statement (:then-statement s) mp)
                               (resolve-statement (:else-statement s) mp))
          (p/if-statement-node (resolve-exp (:condition s) mp)
                               (resolve-statement (:then-statement s) mp)))
    :break s
    :continue s
    :while (p/while-statement-node (resolve-exp (:condition s) mp)
                                   (resolve-statement (:body s) mp))
    :do-while (p/do-while-statement-node (resolve-exp (:condition s) mp)
                                         (resolve-statement (:body s) mp))
    :for (let [new-var-map (copy-variable-map mp)
               for-init (resolve-for-init (:init s) new-var-map)
               new-var-map (if (:declaration for-init)
                             (:variable-map for-init)
                             new-var-map) ; updates new-var-map so that include possible
                                          ; variable declaration
               for-init (if (:declaration for-init)
                          (:declaration for-init)
                          for-init)
               condition (resolve-optional-exp (:condition s) new-var-map)
               post (resolve-optional-exp (:post s) new-var-map)
               body (resolve-statement (:body s) new-var-map)]
           (p/for-statement-node for-init condition post body))
    :compound (let [updated-mp (copy-variable-map mp)]
                (p/compound-statement-node (:block (resolve-block (:block s) updated-mp))))
    :empty (p/empty-statement-node)
    (throw (ex-info "Analyzer error. Invalid statement." {:statement s}))))

(defn- resolve-block-item [item mp]
  (let [type (:type item)]
    (cond
      (= type :declaration) (let [v (resolve-declaration item mp)]
                              {:block-item (:declaration v)
                               :variable-map (:variable-map v)})
      (= type :statement) {:block-item (resolve-statement item mp)
                           :variable-map mp}
      :else (throw (ex-info "Analyzer Error. Invalid statement/declaration." {item item})))))

(defn- resolve-block
  ([block]
   (resolve-block block {}))
  ([block var-mp]
   (reduce (fn [acc block-item]
             (let [v (resolve-block-item block-item (:variable-map acc))]
               {:block (conj (:block acc) (:block-item v))
                :variable-map (:variable-map v)}))
           {:block []
            :variable-map var-mp}
           block)))

(defn- annotate-label [n l]
  (assoc n :label l))

(defn- label-statement
  ([s]
   (label-statement s nil))
  ([s current-label]
   (let [s-type (:statement-type s)]
     (cond
       (= s-type :break) (if (nil? current-label)
                           (throw (ex-info "break statement outside of loop" {}))
                           (p/break-statement-node current-label))
       (= s-type :continue) (if (nil? current-label)
                              (throw (ex-info "continue statement outside of loop" {}))
                              (p/continue-statement-node current-label))
       (= s-type :while) (let [new-label (unique-identifier "while_label")
                               l-body (label-statement (:body s) new-label)
                               l-while  (p/while-statement-node (:condition s) l-body)]
                           (annotate-label l-while new-label))
       (= s-type :do-while) (let [new-label (unique-identifier "do_while_label")
                                  l-body (label-statement (:body s) new-label)
                                  l-do-while (p/do-while-statement-node (:condition s) l-body)]
                              (annotate-label l-do-while new-label))
       (= s-type :for) (let [new-label (unique-identifier "for_label")
                             l-body (label-statement (:body s) new-label)
                             l-for (p/for-statement-node (:init s) (:condition s) (:post s) l-body)]
                         (annotate-label l-for new-label))
       (= s-type :if) (if (:else-statement s)
                        (p/if-statement-node (:condition s)
                                             (label-statement (:then-statement s) current-label)
                                             (label-statement (:else-statement s) current-label))
                        (p/if-statement-node (:condition s)
                                             (label-statement (:then-statement s) current-label)))
       (= s-type :compound) (let [update-block-f (fn [item]
                                                   (if (= (:type item) :statement)
                                                     (label-statement item current-label)
                                                     item))
                                  new-block (map update-block-f (:block s))]
                              (p/compound-statement-node new-block))
       (= s-type :return) s
       (= s-type :expression) s
       (= s-type :empty) s
       :else (throw (ex-info "invalid statement reached during loop labelling." {}))))))

(defn- resolve-loop-labels [body]
  (let [f (fn [item]
            (if (= :statement (:type item))
              (label-statement item)
              item))
        new-body (map f body)]
    new-body))

(defn- validate-function [f]
  (let [body (resolve-loop-labels (:block (resolve-block (:body f))))]
    (assoc f :body body)))

(comment

  (resolve-block
   [{:type :declaration
     :identifier "a",
     :initial {:type :exp, :exp-type :constant-exp, :value 1}}
    {:type :statement,
     :statement-type :return,
     :value {:type :exp, :exp-type :constant-exp, :value 0}}])

  ())

(defn validate [ast]
  (map validate-function ast))

(defn- validate-from-src [s]
  (u/reset-counter!)
  (-> s
      l/lex
      p/parse
      validate))

(comment

  (pp/pprint
   (validate-from-src
    "int main (void) {
int i = 0;
    int j = 0;
    int k = 1;
    for (i = 100 ;;) {
        int i = 1;
        int j = i + k;
        k = j;
    }

    return k == 101 && i == 0 && j == 0;
}"))

  (pp/pprint
   (validate-from-src
    "int main (void) {
int x = 1 + x;
}"))

  ())
