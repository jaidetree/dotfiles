(when (not _G.fennel)
  (global fennel (require :fennel)))

(import-macros testing :config.macros.testing)

(local {:core c} (require :config.utils))

(local parsers {})

(local debug-state {:active false})

(fn toggle-debug
  [bool]
  (tset debug-state :active bool))

(fn debug
  [& args]
  (when debug-state.active
   (print (unpack args))))

(fn success
  [output input]
  {:ok true : output : input})

(fn fail
  [expected actual input]
  {:ok false :output actual : expected : actual : input})

(fn read
  [input]
  (string.sub input.source input.index input.index))

(fn read-n
  [input n]
  (string.sub input.source input.index (- (+ input.index n) 1)))

(fn advance
  [n input]
  {:source input.source :index (+ input.index n)})

(fn parsers.char
  [c]
  (fn [input]
    (let [first-char (read input)]
      (if (= first-char c)
          (success c (advance 1 input))
          (fail c first-char input)))))

(fn parsers.and
  [...]
  (let [parsers [...]]
    (fn [input]
      (var result-acc (success "" input))
      (var i 1)
      (while (and result-acc.ok (. parsers i))
        (let [parser (. parsers i)
              result (parser result-acc.input)]
          (debug "parsers.and result" (fennel.view result))
          (when result.ok
              (set i (+ i 1)))
          (set result-acc result)))
      result-acc)))

(fn parsers.seq
  [...]
  (let [parsers [...]]
    (fn [input]
      (var result-acc (success [] input))
      (var i 1)
      (while (and result-acc.ok (. parsers i))
        (let [parser (. parsers i)
              result (parser result-acc.input)]
          (debug "parsers.seq result" (fennel.view result))
          (if result.ok
              (do
                (set i (+ i 1))
                (set result-acc.input result.input)
                (when (> (length result.output) 0)
                  (table.insert result-acc.output result.output)))
              (set result-acc result))))
      result-acc)))

(fn parsers.or
  [...]
  (let [parsers [...]]
    (fn [input]
      (var result-acc (fail nil [] input))
      (var i 1)
      (while (and (not result-acc.ok) (. parsers i))
        (let [parser (. parsers i)
              result (parser result-acc.input)]
          (if result.ok
              (do
                (set result-acc result))
              (do
                (set i (+ i 1))
                (set result-acc.input result.input)))))
      result-acc)))

(fn valid-input?
  [input]
  (<= input.index (length input.source)))

(fn parsers.many
  [parser]
  (fn [input]
    (var result-acc (success "" input))
    (var done false)
    (while (and (not done) (valid-input? result-acc.input))
      (let [result (parser result-acc.input)]
        (if result.ok
            (do
              (tset result-acc :input result.input)
              (tset result-acc :output (.. result-acc.output result.output)))
            (do
              (set done true)))))
    result-acc))

(fn parsers.drop
  [parser]
  (fn [input]
    (let [result (parser input)]
      (if result.ok
          (let [index result.input.index]
            (success
              ""
              {:index index
               :source input.source}))
          result))))

(fn parsers.whitespace
  []
  (parsers.drop (parsers.many (parsers.or (parsers.char " ")
                                          (parsers.char "\t")
                                          (parsers.char "\r")
                                          (parsers.char "\n")))))

(fn parsers.contains
  [xs]
  (fn [input]
    (let [first-char (read input)]
      (accumulate
        [result (fail (fennel.view xs) first-char input)
         i x (ipairs xs)
         &until (or result.ok (not (valid-input? input)))]
        (if (= x first-char)
          (success x (advance 1 input))
          result)))))

(fn parsers.not
  [parser]
  (fn [input]
    (let [result (parser input)]
      (if result.ok
          (fail false true input)
          (success result.output
                   (advance (- result.input.index 1) result.input))))))

(fn parsers.lit
  [str]
  (fn [input]
    (let [chars (read-n input (length str))]
      (if (= chars str)
          (success str (advance (length str) input))
          (fail str chars input)))))

(fn parsers.between [start middle end]
  (parsers.and start middle end))

(fn parsers.alpha
  []
  (parsers.many
   (parsers.contains
     (vim.split :abcdefghijklmnoqrstuvwxyz
                ""
                {:plain true}))))

(comment
  (ipairs (string.gmatch "abcdefghijklmnoqrstuvwxyz" "."))
  (accumulate [xs []
               x (string.gmatch "abcdefghijklmnoqrstuvwxyz" ".")]
    (do
     (table.insert xs x)
     xs)))

(fn input [source]
  {: source :index 1})

(fn parse [parser source]
  (parser (input source)))

(set parsers.parse parse)

(fn assert-ok [result]
  (assert result.ok)
  result)

(fn ==
  [actual expected ctx]
  (accumulate [state {:ok true :msg ""}
               k expected-v (c.seq expected)
               &until (not state.ok)]
    (let [actual-v (and actual (. actual k))
          path-k (if (and ctx ctx.parent-k)
                   (.. ctx.parent-k "." k)
                   k)]
     (if
       ;; Early fail: Types don't match
       (not (= (type actual-v) (type expected-v)))
       {:ok false :msg (.. "expected " path-k " to be a " (type expected-v) " got " (type actual-v))}

       ;; Recurse if fail
       (and (= (type actual-v) :table))
       (== actual-v expected-v {:parent-k k})

       ;; Equivalent primitive values
       (= actual-v expected-v)
       state

       ;; Else
       {:ok false
        :msg (.. "expected " path-k " to be " (fennel.view expected-v) " got " (fennel.view actual-v))}))))

(testing.print
  "Parses first letter h and moves cursor"
  (parse (parsers.char :h) :hello-world)
  :== {:output "h"
       :ok true
       :input {:index 2
               :source :hello-world}})

(testing.print
  "Parses second letter given index 2"
  ((parsers.char :e) {:source :hello-world
                      :index 2})
  :== {:output "e"
       :input {:index 3 :source "hello-world"}
       :ok true})

(testing.print
  "parsers.and supports sequential parses but only uses last result"
  (parse (parsers.and (parsers.char :h)
                      (parsers.char :e)
                      (parsers.char :l))
         "hello-world")
  :== {:output "l"
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
  "pasers.or support alternative parsing"
  (parse (parsers.or (parsers.char :f)
                     (parsers.char :h))
         "hello-world")
  :== {:input {:index 2}
       :ok true
       :output "h"})

(testing.print
  "pasers.or picks first pass"
  (parse (parsers.or (parsers.char :h)
                     (parsers.char :f))
         "hello-world")
  :== {:input {:index 2}
       :ok true
       :output "h"})

(testing.print
  "parsers.many repeats parsing"
  (parse (parsers.many (parsers.char "*"))
         "****h")
  :== {:input {:index 5}
       :ok true
       :output "****"})

(testing.print
  "parsers.many stops on failure"
  (parse (parsers.many (parsers.char "*"))
         "**a**")
  :== {:input {:index 3}
       :ok true
       :output "**"})

(testing.print
  "parsers.contains parses based on table of input"
  (parse (parsers.contains [:h :o :e])
         "hello")
  :== {:input {:index 2}
       :ok true
       :output "h"})

(testing.print
  "parsers.contains fails when input is not within allowed-list"
  (parse (parsers.contains [:x :y :z])
         "hello")
  :== {:input {:index 1}
       :ok false
       :output "h"})

(testing.print
  "parsers.many can parse whitespace with parsers.or"
  (parse (parsers.many (parsers.contains ["\n" " " "\t"]))
         "   \t \n hello-world")
  :== {:input {:index 8}
       :ok true
       :output "   \t \n "})

(testing.print
  "parsers.whitespace strips all whitespace chars"
  (parse (parsers.whitespace)
         "   \t \n hello-world")
  :== {:input {:index 8}
       :output ""
       :ok true})

(testing.print
  "parsers.whitespace advances the index correctly"
  ((parsers.whitespace)
   {:source "hello  world"
           :index 6})
  :== {:input {:index 8}
       :output ""
       :ok true})

(testing.print
   "parsers can be combined for complex parsing"
   (parse (parsers.seq
            (parsers.whitespace)
            (parsers.many (parsers.char "*"))
            (parsers.whitespace)
            (parsers.alpha))
          "   **** hello")
   :== {:ok true
        :input {:index 14}
        :output ["****" "hello"]})

(comment ;; Testing
  (parse (parsers.lit :hello) :hello-world)
  (parse (parsers.char :h) :hello-world)
  (parse (parsers.not (parsers.char "]")) "target]rest")
  (parse (parsers.not (parsers.char "]")) "]rest")
  ;; This causes infinite loops. Don't run this.
  ;; (parse (parsers.many (parsers.not (parsers.char "]"))) "target]rest")
  ;;

  (parse (parsers.between
           (parsers.char "a")
           (parsers.char "b")
           (parsers.char "c"))
         "abc")
  (parse (parsers.alpha)
         "a")
  (parse (parsers.many (parsers.alpha))
         "abc")
  (parse (parsers.between (parsers.char "[")
                          (parsers.alpha)
                          (parsers.char "]")) "[markdown-url]")


  ;;
  nil)
