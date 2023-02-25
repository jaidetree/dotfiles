(import-macros testing :config.macros.testing)
(local {: parse &as parsers} (require :config.parsers))
(local {: == : === : toggle-debug} (require :config.testing))
(local {:core c} (require :config.utils))
(local {: header-args-parser 
        : block-lang-parser
        : block-header-args-parser
        : block-parser 
        : header-arg-pair 
        : header-arg-pairs 
        : header-arg-lang} (require :config.plugins.org-tangle))

(local src-block-with-args
  "    
   #+begin_src clojure :tangle shadow-cljs.edn :results none
    :dependencies
    [[reagent \"1.2.0\"]
     [promesa \"10.1.850\"]]
   #+end_src
")

(local src-block-plain
  "    
   #+begin_src clojure
    :dependencies
    [[reagent \"1.2.0\"]
     [promesa \"10.1.850\"]]
   #+end_src
")

(testing.print
  "parse block header-arg pair"
  (parse
    header-arg-pair
    ":tangle shadow-cljs.conf\n")
  :== {:ok true
       :input {:index 25}
       :output ["tangle" "shadow-cljs.conf"]})


(testing.print
  "parse block header-arg pairs"
  (parse
    header-arg-pairs
    ":tangle shadow-cljs.conf :results none\n")
  :== {:ok true
       :input {:index 39}
       :output [{:results "none" :tangle "shadow-cljs.conf"}]})

(testing.print
  "parse lang from src block"
  (parse
    block-lang-parser
    src-block-with-args)
  :== {:ok true
       :input {:index 28}
       :output ["clojure"]})

(testing.print
  "parse lang from src block with no args"
  (parse
    block-lang-parser
    src-block-plain)
  :== {:ok true
       :input {:index 28}
       :output ["clojure"]})

(testing.print
  "parse block header-args that may have whitespace prefix"
  (parse
    block-header-args-parser
    "   :tangle shadow-cljs.edn :results none\n :clj-kw-not-header-arg true")
  :== {:ok true
       :input {:index 41}
       :output [{:tangle "shadow-cljs.edn"
                 :results "none"}]})

(testing.print
  "parse block without any header args"
  (parse
    block-parser
    src-block-plain)
  :== {:ok true
       :lang "clojure"
       :props {}
       :body "    :dependencies\n    [[reagent \"1.2.0\"]\n     [promesa \"10.1.850\"]]"})

(testing.print
  "parse block with header args"
  (parse
    block-parser
    src-block-with-args)
  :== {:ok true
       :lang "clojure"
       :props {:tangle "shadow-cljs.edn" 
               :results "none"}
       :body "    :dependencies\n    [[reagent \"1.2.0\"]\n     [promesa \"10.1.850\"]]"})
