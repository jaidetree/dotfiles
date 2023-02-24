(import-macros testing :config.macros.testing)
(local {: parse &as parsers} (require :config.parsers))
(local {: == : toggle-debug} (require :config.testing))
(local {:core c} (require :config.utils))
(local {: header-args-parser} (require :config.plugins.org-tangle))


(testing.print
  "Parses first letter h and moves cursor"
  (parse (parsers.char :h) :hello-world)
  :== {:output ["h"]
       :ok true
       :input {:index 2
               :source :hello-world}})

(testing.print
  "Parses second letter given index 2"
  ((parsers.char :e) {:source :hello-world
                      :index 2})
  :== {:output ["e"]
       :input {:index 3 :source "hello-world"}
       :ok true})

(testing.print
  "parsers.and supports sequential parses but only uses last result"
  (parse (parsers.and (parsers.char :h)
                      (parsers.char :e)
                      (parsers.char :l))
         "hello-world")
  :== {:output ["l"]
       :input {:index 4 :source "hello-world"}
       :ok true})

(testing.print
  "parsers.and fails if any invalid"
  (parse (parsers.and (parsers.char :h)
                      (parsers.char :f)
                      (parsers.char :l))
         "hello-world")
  :== {:actual "e"
       :expected "f"
       :input {:index 2 :source "hello-world"}
       :ok false
       :output "e"})

(testing.print
  "parsers.seq supports sequential parses but aggregates the last result"
  (parse (parsers.seq (parsers.char :h)
                      (parsers.char :e)
                      (parsers.char :l))
         "hello-world")
  :== {:output [:h :e :l]
       :input {:index 4 :source "hello-world"}
       :ok true})

(testing.print
  "parsers.seq fails if any parsers fail"
  (parse (parsers.seq (parsers.char :h)
                      (parsers.char :e)
                      (parsers.char :g))
         "hello-world")
  :== {:ok false
       :output :l
       :input {:index 3
               :source "hello-world"}})

(testing.print
  "pasers.or support alternative parsing"
  (parse (parsers.or (parsers.char :f)
                     (parsers.char :h))
         "hello-world")
  :== {:ok true
       :input {:index 2}
       :output ["h"]})

(testing.print
  "pasers.or picks first pass"
  (parse (parsers.or (parsers.char :h)
                     (parsers.char :f))
         "hello-world")
  :== {:ok true
       :input {:index 2}
       :output ["h"]})

(testing.print
   "parsers.or combines with parsers.always"
   (parse (parsers.or (parsers.char :y)
                      (parsers.always :x))
          "hello world")
   :== {:ok true
        :input {:index 1}
        :output ["x"]})

(testing.print
  "parsers.or complex parsing"
  (parse (parsers.or
           (parsers.concat (parsers.seq (parsers.char ":") (parsers.alpha)))
           (parsers.always "*"))
         "hello")
  :== {:ok true
       :input {:index 1}
       :output ["*"]})

(testing.print
   "parsers.maybe can parse when parser succeeds"
   (parse (parsers.seq
            (parsers.maybe
             (parsers.drop (parsers.lit "header-args")))
            (parsers.lit ":conf"))
          "header-args:conf :tangle tmux.conf")
   :== {:input {:index 17}
        :output [":conf"]})

(testing.print
   "parsers.maybe can fail safely"
   (parse (parsers.seq
            (parsers.maybe
             (parsers.drop (parsers.lit "header-args")))
            (parsers.lit ":conf"))
          ":conf :tangle tmux.conf")
   :== {:input {:index 6}
        :output [":conf"]})

(testing.print
  "parsers.many repeats parsing"
  (parse (parsers.many (parsers.char "a"))
         "aaab")
  :== {:ok true
       :input {:index 4}
       :output ["a" "a" "a"]})

(testing.print
 "parsers.many stops on failure"
 (parse (parsers.many (parsers.char "*"))
        "**a**")
 :== {:input {:index 3}
      :ok true
      :output ["*" "*"]})

(testing.print
  "parsers.many works with parsers.or"
  (parse
    (parsers.concat
     (parsers.many
       (parsers.or (parsers.char "a")
                   (parsers.char "."))))
    "aaa.aab")
  :== {:ok true
       :input {:index 7}
       :output ["aaa.aa"]})

(testing.print
  "parsers.contains parses based on table of input"
  (parse (parsers.contains [:h :o :e])
         "hello")
  :== {:input {:index 2}
       :ok true
       :output ["h"]})

(testing.print
  "parsers.contains fails when input is not within allowed-list"
  (parse (parsers.contains [:x :y :z])
         "hello")
  :== {:input {:index 1}
       :ok false
       :output ["h"]})

(testing.print
  "parsers.contains + many can parse whitespace"
  (parse
    (parsers.concat
      (parsers.many (parsers.contains ["\n" " " "\t"])))
    "   \t \n hello-world")
  :== {:input {:index 8}
       :ok true
       :output ["   \t \n "]})

(testing.print
  "parsers.whitespace strips all whitespace chars"
  (parse (parsers.whitespace)
         "   \t \n hello-world")
  :== {:input {:index 8}
       :output []
       :ok true})

(testing.print
  "parsers.whitespace advances the index correctly"
  ((parsers.whitespace)
   {:source "hello  world"
           :index 6})
  :== {:input {:index 8}
       :output []
       :ok true})

(testing.print
  "parsers.alpha reads a sequence of letters a-z"
  (parse
    (parsers.alpha)
    "hello*world")
  :== {:ok true
       :input {:index 6}
       :output ["hello"]})


(testing.print
  "parsers.alpha can fail"
  (parse
    (parsers.alpha)
    "6hello*world")
  :== {:ok false
       :input {:index 1}
       :output ["6"]})

(testing.print
   "parsers can be combined for complex parsing"
   (parse (parsers.seq
            (parsers.whitespace)
            (parsers.concat (parsers.many (parsers.char "*")))
            (parsers.whitespace)
            (parsers.alpha))
          "   **** hello")
   :== {:ok true
        :input {:index 14}
        :output ["****" "hello"]})

(testing.print
  "parsers.not consumes everything until it fails"
  (parse
    (parsers.concat
     (parsers.many
       (parsers.not (parsers.char "*"))))
    "hello*world")
  :== {:ok true
       :input {:index 6}
       :output ["hello"]})

(testing.print
  "parsers.lit consumes literal sequence of chars"
  (parse (parsers.lit "hello-world")
         "hello-world")
  :== {:ok true
       :input {:index 12}
       :output ["hello-world"]})

(testing.print
  "pasers.lit fails if everything is not there"
  (parse (parsers.lit "hello")
         "help")
  :== {:ok false
       :input {:index 1}})

(testing.print
  "parsers.take-until reads just about anything until parser succeeds"
  (parse 
    (parsers.take-until
      (parsers.char "b"))
    "aaaab")
  :== {:ok true
       :input {:index 5}
       :output ["aaaa"]})

(testing.print
   "parsers compose for single header-args pair"
   (parse
     header-args-parser
     "conf :tangle base.conf")
   :== {:lang "conf"
        :props {:tangle "base.conf"}})

(testing.print
   "parsers compose for multiple header-args pairs"
   (parse
     header-args-parser
     "conf :tangle base.conf :results none")
   :== {:lang "conf"
        :props {:tangle "base.conf"
                :results "none"}})

(testing.print
   "parsers compose for multiple header-args with no lang"
   (parse
    header-args-parser
    ":tangle base.conf :results none")
   :== {:lang "*"
        :props {:tangle "base.conf"
                :results "none"}})

(testing.print
  "parsers can compose to parse the body of src blocks"
  (parse
    (parsers.seq
      (parsers.whitespace)
      (parsers.drop (parsers.and
                      (parsers.lit "#+begin_src")
                      (parsers.take-until (parsers.char "\n"))
                      (parsers.char "\n")))
      (parsers.take-until (parsers.seq
                            (parsers.whitespace)
                            (parsers.lit "#+end_src"))))
    "   
   #+begin_src clojure
    :dependencies
    [[reagent \"1.2.0\"]
     [promesa \"10.1.850\"]]
  #+end_src
")
  :== {:ok true
       :output ["    :dependencies
    [[reagent \"1.2.0\"]
     [promesa \"10.1.850\"]]"]})

(testing.print
  "parsers.between consumes data between two parsers"
  (parse
    (parsers.between (parsers.char "[")
                     (parsers.char "]"))
    "[hello-world]")
  :== {:ok true
       :input {:index 14}
       :output ["hello-world"]})

(testing.print
  "parsers.between can parse the body of src blocks"
  (parse
    (parsers.between
      (parsers.and (parsers.whitespace)
                   (parsers.lit "#+begin_src")
                   (parsers.take-until (parsers.char "\n"))
                   (parsers.char "\n"))
      (parsers.and (parsers.whitespace)
                   (parsers.lit "#+end_src")))
    "   
   #+begin_src clojure
    :dependencies
    [[reagent \"1.2.0\"]
     [promesa \"10.1.850\"]]
  #+end_src
")
  :== {:ok true
       :output ["    :dependencies
    [[reagent \"1.2.0\"]
     [promesa \"10.1.850\"]]"]})
