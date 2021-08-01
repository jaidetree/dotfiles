(var suites [])
(local state {:suite nil
              :ran 0
              :failed 0
              :passed 0})

;; 4-bit colors
;; https://i.stack.imgur.com/9UVnC.png

(local colors {:red   "31"
               :green "92"})

(fn describe
  [suite-name suite-f]
  (table.insert suites {:name suite-name
                        :suite suite-f
                        :tests []}))

(fn it
  [description test-f]
  (if state.suite
      (table.insert state.suite.tests {:desc description
                                       :test test-f})))

(fn collect-tests
  []
  (each [i suite-map (ipairs suites)]
    (tset state :suite suite-map)
    (suite-map.suite))
  suites)

(fn color
  [text color]
  (assert (. colors color) (.. "Color " color " could not be found"))
  (.. "\27[" (. colors color) "m" text "\27[0m"))


(fn green
  [text]
  (color text :green))

(fn red
  [text]
  (color text :red))

(fn try-test
  [f]
  (let [(ok err) (xpcall f (fn [err]
                             (do
                               (tset state :failed (+ state.failed 1))
                               (print (.. "    " (red "[ FAIL ]") "\n"))
                               (print (debug.traceback err) "\n"))))]
    (if ok
        (do
          (print  (.. "    " (green "[ OK ]") "\n"))
          (tset state :passed (+ state.passed 1)))
        )))

(fn init
  []
  (set suites [])
  (tset state :ran 0)
  (tset state :failed 0)
  (tset state :passed 0))

(fn run-all-tests
  []
  (print "")
  (let [start (os.clock)]
    (each [i suite-map (ipairs suites)]
      (print suite-map.name "\n")
      (each [_ test-map (ipairs suite-map.tests)]
        (print (.. "  " test-map.desc " ...  \t"))
        (try-test test-map.test)
        (tset state :ran (+ state.ran 1))))

    (let [end (os.clock)
          elapsed (- end start)]
      (print (.. "\n  Ran " state.ran " tests " (green state.passed) " passed " (red state.failed) " failed in " elapsed " seconds"))
      (when (> state.failed 0)
        (error "Tests failed")))))

{: init
 : suites
 : it
 : describe
 : collect-tests
 : run-all-tests}
