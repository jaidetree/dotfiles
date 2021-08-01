;; Copyright (c) 2017-2020 Ag Ibragimov & Contributors
;;
;;; Author: Jay Zawrotny <jayzawrotny@gmail.com>
;;
;;; URL: https://github.com/agzam/spacehammer
;;
;;; License: MIT
;;

"
Macros to create advisable functions or register advice for advisable functions
"

;; How do I generate a function here that can do the lookup without having to
;; generate a new function every call?
;;
;; In the doom discord, mjbach + flat mentioned going the other way. Update the
;; global function registry to support advice instead of doing the lookup on
;; every call

(fn defn
  [fn-name args docstr body1 ...]
  (assert (= (type docstr) :string) "A docstr required for advisable functions")
  (assert body1 "advisable function expected body")
  `(local ,fn-name
          (let [fn# (_G.advisable-fn
                     ,(tostring fn-name)
                     (fn ,fn-name ,args ,body1 ,...))]
            (fn ,fn-name
              [...]
              ,docstr
              (fn# (table.unpack [...]))))
    ))

;; (fn ,fn-name
;;       [...]
;;       ,docstr
;;       (_G.dispatch-advice
;;        ,(tostring fn-name)
;;        ""
;;        (fn ,fn-name ,args ,body1 ,...)
;;        [...]))


(fn defadvice
  [advice-fn-name advice-args advice-type fn-name docstr body1 ...]
  nil)

{:defn          defn
 :defadvice     defadvice}
