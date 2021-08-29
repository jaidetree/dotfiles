(local fennel (require :fennel))
(local {: slice}  (require :lib.functional))

(local homedir (os.getenv "HOME"))
(local customdir (.. homedir "/.spacehammer"))
(tset fennel :path (.. customdir "/?.fnl;" fennel.path))
(tset fennel :path (.. customdir "/?/init.fnl;" fennel.path))

;; Setup some globals for test files and debugging

(global pprint (fn [x] (print (fennel.view x))))


(fn run-file
  [args]

  "
  Run an arbitrary hs based fennel file
  "
  (let [[dir & test-files] (slice 2 args)]
    (each [i filename (ipairs test-files)]
      (let [filepath (hs.fs.pathToAbsolute (.. dir "/" filename))]
        (print "Running file" filepath)
        (fennel.dofile filepath))
      ))

  )


{: run-file}
