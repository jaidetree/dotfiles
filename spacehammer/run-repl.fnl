;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup a REPL server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global repl (require :repl))
(local coroutine (require :coroutine))
(global replserver (repl.start))
(repl.run replserver)

;; (local fennel (require :fennel))

;; (fennel.repl {})
