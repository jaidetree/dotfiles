(fn print-test
  [test-name body1 test-fn expected]
  (assert body1 "Body expression expected")
  `(let [output# ,body1
         expected# ,expected
         divider# (.. (string.rep "-" 40) "\n")
         result# (,(sym test-fn) output# expected#)]
    (print (.. ,test-name "\n"
               ";; code: " ,(view body1) "\n"
               ";; output:\n"
               divider#
               (fennel.view output#) "\n"
               divider#
               ";; test: " (if result#.ok "PASS" "FAIL") "\n"
               divider#
               (.. "(" ,test-fn " % " (fennel.view expected#) ")") "\n"
               divider#
               (if (not result#.ok)
                 (.. "  - " result#.msg "\n")
                 "")
               "\n\n")
          "\n\n")
    (when (not result#.ok)
      (error (.. "Test failed: " result#.msg)))))

(fn with-debug
  [body1 & body]
  (assert body1 "Body expression expected")
  `(do
     (toggle-debug true)
     ,body1
     (unpack ,[...])
     (toggle-debug false)))

{:print print-test
 : with-debug}
