(ns cljcc.analyze.label-loops
  (:require [cljcc.parser :as p]
            [cljcc.exception :as exc]
            [cljcc.analyze.resolve :as r]
            [cljcc.util :as util]
            [malli.dev.pretty :as pretty]))

(defn- unique-identifier [identifier]
  (util/create-identifier! identifier))

(defn- annotate-label [m label]
  (assoc m :label label))

(defn- label-statement
  ([s]
   (label-statement s nil))
  ([{:keys [statement-type] :as s} current-label]
   (condp = statement-type
     :break (if (nil? current-label)
              (exc/analyzer-error "break statement outside of loop" s)
              (p/break-statement-node current-label))
     :continue (if (nil? current-label)
                 (exc/analyzer-error "continue statement outside of loop" s)
                 (p/continue-statement-node current-label))
     :while (let [new-label (unique-identifier "while_label")
                  l-body (label-statement (:body s) new-label)
                  l-while  (p/while-statement-node (:condition s) l-body)]
              (annotate-label l-while new-label))
     :do-while (let [new-label (unique-identifier "do_while_label")
                     l-body (label-statement (:body s) new-label)
                     l-do-while (p/do-while-statement-node (:condition s) l-body)]
                 (annotate-label l-do-while new-label))
     :for (let [new-label (unique-identifier "for_label")
                l-body (label-statement (:body s) new-label)
                l-for (p/for-statement-node (:init s) (:condition s) (:post s) l-body)]
            (annotate-label l-for new-label))
     :if (if (:else-statement s)
           (p/if-statement-node (:condition s)
                                (label-statement (:then-statement s) current-label)
                                (label-statement (:else-statement s) current-label))
           (p/if-statement-node (:condition s)
                                (label-statement (:then-statement s) current-label)))
     :compound (let [update-block-f (fn [item]
                                      (if (= (:type item) :statement)
                                        (label-statement item current-label)
                                        item))
                     new-block (mapv update-block-f (:block s))]
                 (p/compound-statement-node new-block))
     :return s
     :expression s
     :empty s
     (exc/analyzer-error "invalid statement reached during loop labelling." s))))

(defn- label-loop-function-body [fn-declaration]
  (let [statement? (fn [x] (= :statement (:type x)))
        labelled-body (mapv (fn [block-item]
                              (if (statement? block-item)
                                (label-statement block-item)
                                block-item))
                            (:body fn-declaration))]
    (assoc fn-declaration :body labelled-body)))

(defn label-loops
  "Annotates labels on looping constructs.

  Parameter:
    program: List of declarations / blocks"
  [program]
  (let [fn-declaration? (fn [x] (= :function (:declaration-type x)))]
    (mapv (fn [block]
            (if (fn-declaration? block)
              (label-loop-function-body block)
              block))
          program)))

(comment

  (-> "./test-programs/example.c"
      slurp
      p/parse-from-src
      r/resolve-program)

  (-> "./test-programs/example.c"
      slurp
      p/parse-from-src
      r/resolve-program
      label-loops)

  (pretty/explain
   p/Program
   (-> "./test-programs/example.c"
       slurp
       p/parse-from-src
       r/resolve-program))

  (pretty/explain
   p/Program
   (-> "./test-programs/example.c"
       slurp
       p/parse-from-src
       r/resolve-program
       label-loops))

  ())
