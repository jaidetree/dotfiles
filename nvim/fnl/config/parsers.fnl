(when (not fennel)
 (local fennel (require :fennel)))
(local parsers {})

(fn success
  [output input]
  {:ok true
   : output
   : input})

(fn fail
  [expected actual input]
  {:ok false
   :output actual
   : expected
   : actual
   : input})

(fn read
  [input]
  (string.sub input.source input.index input.index))

(fn advance
  [n input]
  {:source input.source
   :index (+ input.index n)})

(fn parsers.char
  [c]
  (fn [input]
    (let [first-char (read input)]
      (if (= first-char c)
        (success c (advance 1 input))
        (fail    c first-char input)))))

(fn parsers.and
  [...]
  (let [parsers [...]]
    (fn [input]
      (var result-acc (success [] input))
      (var i 1)
      (while (and result-acc.ok (. parsers i))
        (let [parser (. parsers i)
              result (parser result-acc.input)]
          (if result.ok
            (do 
              (set i (+ i 1))
              (set result-acc.input result.input)
              (table.insert result-acc.output result.output))
            (do
              (set result-acc result)))))
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
    (var result-acc (success [] input))
    (var done false)
    (while (and (not done) (valid-input? result-acc.input)) 
      (let [result (parser result-acc.input)]
        (if result.ok
          (do 
            (set result-acc.input result.input)
            (table.insert result-acc.output result.output))
          (do
            (set done true)))))
    result-acc))
  
(fn parsers.drop
  [parser]
  (fn [input]
    (let [result (parser input)]
      (if result.ok
        (do (success nil result.input))
        result))))
  

(set parsers.whitespace 
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
      (var result (fail (.. "[" (table.concat xs " ") "]")
                        first-char
                        input))

      (var i 1)
      (while (and (not result.ok) (valid-input? input))
        (let [x (. xs i)]
          (when (= x first-char)
            (set result (success x (advance 1 input))))
          (set i (+ i 1))))
      result)))
          

(fn parsers.any 
  [...]
  (parsers.many (parsers.or ...)))
        

(set parsers.alpha
 (parsers.contains))
   

(fn input
  [source]
  {: source
   :index 1})


(fn parse
  [parser source]
  (parser (input source)))

(set parsers.parse parse)

(fn assert-ok
  [result]
  (assert result.ok)
  result)

(print (fennel.view (assert-ok (parse (parsers.char :h) "hello-world"))))
(print (fennel.view (assert-ok ((parsers.char :e) {:source "hello-world" :index 2}))))
(print (fennel.view (assert-ok (parse (parsers.and
                                        (parsers.char :h)
                                        (parsers.char :e)
                                        (parsers.char :l)) "hello-world"))))
(print (fennel.view (assert-ok (parse (parsers.or
                                        (parsers.char :h)
                                        (parsers.char :e)) "hello-world"))))

(print (fennel.view (parse (parsers.many (parsers.char "*")) "****")))

(print (fennel.view (parse parsers.whitespace "   \t \n hello-world")))

(print (fennel.view (parse (parsers.contains
                             [:l :h :o :e])
                           "hello")))

(print (fennel.view (parse (parsers.any
                             (parsers.contains [:l :h :o :e])) 
                           "hello")))

(print (fennel.view (parse 
                     (parsers.and
                       ;; Update drop to somehow not output anything
                       parsers.whitespace
                       ;; Add a one-or-more or 1+ fn to only pass if at least one match is found
                       (parsers.many (parsers.char "*"))
                       parsers.whitespace
                       (parsers.any (parsers.contains (vim.split "abcdefghijklmnoqrstuvwxyz" "" {:plain true})))) 
                     "   **** hello")))

