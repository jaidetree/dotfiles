(local fennel (require :fennel))
(local {: slice}  (require :lib.functional))

(local homedir (os.getenv "HOME"))
(local customdir (.. homedir "/.spacehammer"))
(tset fennel :path (.. customdir "/?.fnl;" fennel.path))
(tset fennel :path (.. customdir "/?/init.fnl;" fennel.path))

(global pprint (fn [x] (print (fennel.view x))))

(global {: after
         : before
         : describe
         : it} (require :testing))

(print _G.describe)

(local {: init
        : collect-tests
        : run-all-tests} (require :testing))

(fn load-tests
  [args]

  (init)
  (let [[dir & test-files] (slice 2 args)]
    (each [i test-file (ipairs test-files)]
      (let [test-file-path (hs.fs.pathToAbsolute (.. dir "/" test-file))]
        (print "Running tests for" test-file-path)
        (fennel.dofile test-file-path))
      ))


  (collect-tests)
  (run-all-tests)
  )


{: load-tests}