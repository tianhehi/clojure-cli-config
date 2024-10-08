(ns user
  (:require [clojure.main]
            [potemkin]
            [sc.api :as sc]
            [clj-reload.core :as reload]
            [portal.api]
            [clojure.reflect :as rf]
            [clj-commons.pretty.repl]
            [clj-commons.format.table]))

(add-tap #'portal.api/submit)

(defn visualize-portal []
  (portal.api/open {:app false
                    ;; :port 8889
                    :editor :vs-code}))
;; import pretty lib
(potemkin/import-fn clj-commons.pretty.repl/install-pretty-exceptions)
(potemkin/import-fn clj-commons.format.table/print-table)

;; import reload tools
(potemkin/import-fn reload/reload)
(potemkin/import-fn reload/unload)

;; import sc tool function and macro
(potemkin/import-macro sc/brk)
(potemkin/import-macro sc/spy)
(potemkin/import-macro sc/brkqt)
(potemkin/import-macro sc/spyqt)
(potemkin/import-macro sc/defsc)
(potemkin/import-macro sc/letsc)
(potemkin/import-macro sc/undefsc)


(defn tap-> [x]
  (doto x
    (tap>)))

(defmacro t->
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is a lambda or not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  {:added "1.0"}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (and (seq? form) (not (#{'fn 'fn*} (first form))))
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defn heap []
  (let [u (.getHeapMemoryUsage (java.lang.management.ManagementFactory/getMemoryMXBean))
        used (/ (.getUsed u) 1e6)
        total (/ (.getMax u) 1e6)]
    (format "Used: %.0f/%.0f MB (%.0f%%)" used total (/ used total 0.01))))





;; debug fn
(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym))
                 symbols)
            symbols)))

(defn reader-with-exit [request-prompt request-exit]
  (let [form (clojure.main/repl-read request-prompt request-exit)]
    (if (= 'ee form) request-exit form)))

(defmacro break []
  `(clojure.main/repl
    :prompt #(print "debug=> ")
    :read reader-with-exit
    :eval (partial contextual-eval (local-context))))


(println "Loaded system-wide user.clj!")