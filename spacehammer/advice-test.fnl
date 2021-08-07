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

   


   ))

