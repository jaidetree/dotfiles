(local is (require :assert))
(local {: join
        : map} (require :lib.functional))

(local {: reset
        : make-advisable
        : add-advice
        : advisable-keys
        : print-advisable-keys} (require :advice))

(describe
 "Advice"
 (fn []
   (before
    (fn []
      (reset)))

   ;; (it "Should prepend to tables using tables.insert"
   ;;     (fn []
   ;;       (let [tbl []]
   ;;         (table.insert tbl 1 :a)
   ;;         (table.insert tbl 1 :b)
   ;;         (is.eq? (join " " tbl) "b a" "Prepending does not work"))))

   (it "Should call unadvised functions as-is"
       (fn []
         (let [test-func (make-advisable
                          :test-func-1
                          (fn [arg]
                            "Advisable test function"
                            (.. "Hello " arg)))]

           (is.eq? (test-func "cat") "Hello cat" "Unadvised test-func did not return \"Hello cat\""))))

   (it "Should call override functions instead"
       (fn []
         (let [test-func (make-advisable
                          :test-func-2
                          (fn [...]
                            "Advisable test function"
                            "Plain pizza"))]

           (add-advice test-func :override (fn [...] (.. "Overrided " (join " " [...]))))
           (is.eq? (test-func "anchovie" "pizza") "Overrided anchovie pizza" "Override test-func did not return \"Overrided anchovie pizza\""))))

   (it "Should call around functions with orig"
       (fn []
         (let [test-func (make-advisable
                          :test-func-3
                          (fn [...]
                            "Advisable test function"
                            ["old" (table.unpack [...])]))]

           (add-advice test-func :around (fn [orig ...] (join " " ["around" (table.unpack (orig (table.unpack [...])))])))
           (is.eq? (test-func "one" "two") "around old one two" "Around test-func did not return \"around one two old\""))))

   (it "Should call before functions"
       (fn []
         (let [state {:calls 0
                      :args ""}
               test-func (make-advisable
                          :test-func-4
                          (fn [...]
                            "Advisable test function"
                            (let [args [...]]
                              (tset state :args (.. state.args " " (join " " (map #(+ $1 2) [...])))))
                            (tset state :calls (+ state.calls 1))))]

           (add-advice test-func :before (fn [...]
                                           (let [args [...]]
                                             (tset state :args (join " " [...])))
                                           (tset state :calls (+ state.calls 1))))
           (test-func 1 2)
           (is.eq? state.calls 2 "Before test-func did not call both the original and before fn")
           (is.eq? state.args "1 2 3 4" "Before test-func did not call both the original and before with the same args"))))

   (it "Should call orig if before-while returns truthy"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-5
                          (fn [...]
                            "Advisable test function"
                            (.. "original " (join " " [...]))))]

           (add-advice test-func
                       :before-while
                       (fn [...]
                         (tset state :called true)
                         true))
           (is.eq? (test-func 1 2) "original 1 2" "Before-while test-func did not call original function")
           (is.eq? state.called true "Before-while test-func advice function was not called"))))

   (it "Should not call orig if before-while returns false"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-5b
                          (fn [...]
                            "Advisable test function"
                            (.. "original " (join " " [...]))))]

           (add-advice test-func
                       :before-while
                       (fn [...]
                         (tset state :called true)
                         false))
           (is.eq? (test-func 1 2) false "Before-while test-func did call original function")
           (is.eq? state.called true "Before-while test-func advice function was not called"))))


   (it "Should call orig if before-until returns falsey value"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-6
                          (fn [...]
                            "Advisable test function"
                            (.. "original " (join " " [...]))))]

           (add-advice test-func
                       :before-until
                       (fn [...]
                         (tset state :called true)
                         false))
           (is.eq? (test-func 1 2) "original 1 2" "Before-until test-func did not call original function")
           (is.eq? state.called true "Before-until test-func advice function was not called"))))


   (it "Should not call orig if before-until returns truthy value"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-6b
                          (fn [...]
                            "Advisable test function"
                            (.. "original " (join " " [...]))))]

           (add-advice test-func
                       :before-until
                       (fn [...]
                         (tset state :called true)
                         true))
           (is.eq? (test-func 1 2) true "Before-until test-func did call original function")
           (is.eq? state.called true "Before-until test-func advice function was not called"))))


   (it "Should call after functions"
       (fn []
         (let [state {:calls 0
                      :args ""}
               test-func (make-advisable
                          :test-func-7
                          (fn [...]
                            "Advisable test function"
                            (let [args [...]]
                              (tset state :args (join " " [...])))
                            (tset state :calls (+ state.calls 1))
                            true))]

           (add-advice test-func :after (fn [...]
                                           (let [args [...]]
                                             (tset state :args (.. state.args " " (join " " (map #(+ $1 2) [...])))))
                                           (tset state :calls (+ state.calls 1))))
           (test-func 1 2)
           (is.eq? state.calls 2 "After test-func did not call both the original and after fn")
           (is.eq? state.args "1 2 3 4" "After test-func did not call both the original and after with the same args"))))


   (it "Should call after-while if orig returns truthy"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-8
                          (fn [...]
                            "Advisable test function"
                            (.. "original " (join " " [...]))))]

           (add-advice test-func
                       :after-while
                       (fn [...]
                         (tset state :called true)
                         true))
           (is.eq? (test-func 1 2) true "After-while test-func did not call original function")
           (is.eq? state.called true "After-while test-func advice function was not called"))))

   (it "Should not call after-while if orig returns falsey"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-8b
                          (fn [...]
                            "Advisable test function"
                            false))]

           (add-advice test-func
                       :after-while
                       (fn [...]
                         (tset state :called true)
                         true))
           (is.eq? (test-func 1 2) false "After-while test-func did not call original function")
           (is.eq? state.called false "After-while test-func advice function was called"))))



   (it "Should call after-until if orig returns falsey value"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-9
                          (fn [...]
                            "Advisable test function"
                            false))]

           (add-advice test-func
                       :after-until
                       (fn [...]
                         (tset state :called true)
                         false))
           (is.eq? (test-func 1 2) false "After-until test-func did not call original function")
           (is.eq? state.called true "After-until test-func advice function was not called"))))

   (it "Should not call after-until if orig returns truthy value"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-9b
                          (fn [...]
                            "Advisable test function"
                            (.. "original " (join " " [...]))))]

           (add-advice test-func
                       :after-until
                       (fn [...]
                         (tset state :called true)
                         false))
           (is.eq? (test-func 1 2) "original 1 2" "After-until test-func did call advise function")
           (is.eq? state.called false "After-until test-func advice function was called"))))

   (it "Should filter args sent to orig function"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-10
                          (fn [...]
                            "Advisable test function"
                            (.. "original " (join " " [...]))))]

           (add-advice test-func
                       :filter-args
                       (fn [arg-1 arg-2]
                         (tset state :called true)
                         [ arg-2 ]))
           (is.eq? (test-func 1 2) "original 2" "Filter-args test-func did call orig function with filtered-args")
           (is.eq? state.called true "Filter-args test-func advice function was not called"))))

   (it "Should filter the return value from orig function"
       (fn []
         (let [state {:called false}
               test-func (make-advisable
                          :test-func-11
                          (fn [...]
                            "Advisable test function"
                            [ "original" (table.unpack [...])]))]

           (add-advice test-func
                       :filter-return
                       (fn [[arg-1 arg-2 arg-3]]
                         (tset state :called true)
                         (.. "filtered " arg-2 " " arg-3)))
           (is.eq? (test-func 1 2) "filtered 1 2" "Filter-return test-func did call advise with orig return")
           (is.eq? state.called true "Filter-return test-func advice function was not called"))))

   ))
