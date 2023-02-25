(local {:core c} (require :config.utils))
(local {: debug} (require :config.testing))

(local parsers {})

(fn success
  [output input]
  {:ok true : output : input})

(set parsers.success success)

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
          (success [c] (advance 1 input))
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
          (debug "parsers.seq" (fennel.view result))
          (if result.ok
            (do
              (set i (+ i 1))
              (set result-acc.input result.input)
              (if
                (> (length result.output) 1)
                (table.insert result-acc.output result.output)
                (> (length result.output) 0)
                (tset result-acc :output (c.concat result-acc.output result.output))))
            (set result-acc result))))
      result-acc)))


(fn parsers.or
  [& parsers]
  (fn [input]
    (accumulate [result-acc (fail {:ok true} {:ok false} input)
                 i parser (ipairs parsers)
                 &until result-acc.ok]
      (let [result (parser result-acc.input)]
        (debug "parsers.or" (fennel.view result))
        (if result.ok
          (success
            result.output
            result.input)
          result)))))

(fn parsers.maybe
  [parser]
  (fn [input]
    (debug "parsers.maybe" (fennel.view input))
    (let [result (parser input)]
      (if result.ok
        result
        (success [] input)))))

(fn valid-input?
  [input]
  (<= input.index (length input.source)))

(fn parsers.many
  [parser]
  (fn [input]
    (var result-acc (success [] input))
    (var done false)
    (var last-result {})
    (while (and (not done) (valid-input? result-acc.input))
      (let [result (parser result-acc.input)]
        (debug "parsers.many input" (fennel.view result))
        (if result.ok
            (do
              (tset result-acc :input result.input)
              (if
                (= (type (. result.output 1) :string)
                  (each [_ v (ipairs result.output)]
                   (table.insert result-acc.output v)))
                (= (type (. result.output 1) :table)
                   (table.insert result-acc.output result.output))))

            (do
              (set last-result result)
              (set done true)))))
    (debug "parsers.many output" (fennel.view result-acc))
    (if (= (length result-acc.output) 0)
      (fail last-result.expected
            last-result.actual
            last-result.input)
      result-acc)))

(fn parsers.drop
  [parser]
  (fn [input]
    (let [result (parser input)]
      (if result.ok
          (let [index result.input.index]
            (success
              []
              {:index result.input.index
               :source input.source}))
          result))))

(fn parsers.whitespace
  []
  (parsers.drop
    (parsers.many
      (parsers.or
        (parsers.char " ")
        (parsers.char "\t")
        (parsers.char "\r")
        (parsers.char "\n")))))

(fn parsers.contains
  [xs]
  (fn [input]
    (let [first-char (read input)]
      (accumulate
        [result (fail (fennel.view xs) [first-char] input)
         i x (ipairs xs)
         &until (or result.ok (not (valid-input? input)))]
        (if (= x first-char)
          (success [x] (advance 1 input))
          result)))))

(fn parsers.any-char
  [chars]
  (fn [input]
    (let [first-char (read input)]
      (if (string.find chars first-char 1 true)
        (success [first-char] (advance 1 input))
        (fail chars [first-char] input)))))

(fn parsers.not
  [parser]
  (fn [input]
    (let [result (parser input)]
      (debug "parsers.not" (fennel.view result))
      (if result.ok
          (fail false true input)
          (success [result.output]
                   (advance (+ 1 (- result.input.index input.index))
                            result.input))))))

(fn parsers.lit
  [str]
  (fn [input]
    (let [chars (read-n input (length str))]
      (if (= chars str)
          (success [str] (advance (length str) input))
          (fail str chars input)))))

(fn input [source]
  {: source :index 1})

(fn parse [parser source]
  (parser (input source)))

(set parsers.parse parse)

(fn xf
  [parser xf-fn]
  (fn [input]
    (let [result (parser input)]
      (if result.ok
        (xf-fn result)
        result))))

(fn parsers.concat
  [parser]
  (parsers.xf
    parser
    (fn
      [result]
      (success [(table.concat result.output "")] result.input))))

(set parsers.xf xf)

(fn parsers.map
  [map-fn parser]
  (parsers.xf
    parser
    (fn [result]
      (if result.ok
        (success
          (icollect
            [_ v (ipairs result.output)]
            (map-fn v))
          result.input)
        result))))

(fn parsers.first
  [parser]
  (xf parser
      (fn [result]
        (success (. result.output 1) result.input))))

(fn parsers.alpha
  []
  (parsers.concat
   (parsers.many
    (parsers.contains
      (vim.split :abcdefghijklmnopqrstuvwxyz
                 ""
                 {:plain true})))))

(fn parsers.always
  [output]
  (fn [input]
    (success [output] input)))

;; @TODO Maybe this should fail if it reaches the end of input without hitting
;; terminating parser?
(fn parsers.take-until
  [parser]
  (fn [input]
    (var done false)
    (var end-result (fail "take-until: at least one valid parse" "no valid parses" input))
    (var output "")
    (while (and (not done) (valid-input? end-result.input))
      (let [result (parser end-result.input)]
        (if
          ;; No output collected, end reached first try
          ;; Considered a failure
          (and result.ok (= output ""))
          (do
            (set done true))

          ;; Some output was collected, done parsing
          ;; Considered a success
          result.ok
          (do
            (set end-result (success [output] end-result.input)
                        (set done true)

                      ;; Else
                      (do
                        (set output (.. output (read end-result.input)))
                        (set end-result (success [output] (advance 1 end-result.input))))
                end-result)))))))

(fn parsers.between
  [start end]
  (fn [input]
   (let [parser (parsers.seq
                  (parsers.drop start)
                  (parsers.take-until end)
                  (parsers.drop end))
         result (parser input)]
     (debug "parsers.between" (fennel.view result))
     result)))

(fn parsers.log
  [label parser]
  (fn [input]
    (print label "input:" (fennel.view input))
    (let [result (parser input)]
      (print label "output:" (fennel.view result))
      result)))

parsers
