(fn print-test
  [test-name body1 ...]
  (assert body1 "Body expression expected")
  `(print (.. ";; test: " ,test-name "\n"
              ";; code: " ,(view body1 (unpack [...]))
              "\n"
              ";; result:\n"
              ,(string.rep "-" 40) "\n"
              (fennel.view ,body1 (unpack ,[...]))
              "\n\n")
          "\n\n"))

{:print print-test}
