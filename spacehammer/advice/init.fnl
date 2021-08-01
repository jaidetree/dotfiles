;; Copyright (c) 2017-2020 Ag Ibragimov & Contributors
;;
;;; Author: Jay Zawrotny <jayzawrotny@gmail.com>
;;
;;; URL: https://github.com/agzam/spacehammer
;;
;;; License: MIT
;;

"
Advising API to register functions
"

(require-macros :lib.macros)
(local {:reduce reduce
        :seq    seq} (require :lib.functional))

(local advice {})

(fn add-advice
  [f advice-type advice-fn]
  (let [key f.key
        advice-entry (. advice key)]
    (when advice-entry
      (tset advice-entry.advice advice-type advice-fn))))

(fn remove-advice
  [fn-name advice-type]
  nil)

(fn register-advisable
  [key f]
  ;; @TODO Replace with if-let or similar macro but doesn't work in an isolated fennel file
  (let [advice-entry (. advice key)]
    (if advice-entry
        advice-entry
        (tset advice key
              {:original f
               :advice {}}))))

(fn apply-advice
  [entry args]
  (if
   entry.advice.override
   (entry.advice.override (table.unpack args))

   entry.advice.around
   (entry.advice.around entry.original (table.unpack args))

   (let [args (and entry.advice.filter-args
                   (entry.advice.filter-args (table.unpack args)))
         passed-before-while (or (not entry.advice.before-while)
                                 (and entry.advice.before-while (entry.advice.before-while (table.unpack args))))
         passed-before-until (or passed-before-while
                                 (and (not passed-before-while) entry.advice.before-until (not (entry.advice.before-until (table.unpack args)))))]

     (when (and passed-before-while passed-before-until entry.advice.before)
       (entry.advice.before (table.unpack args)))

     
     (when (and passed-before-while passed-before-until)
       (let [return (entry.original (table.unpack args))]

         (let [passed-after-while (or (and return (not entry.advice.after-while) return)
                                      (and entry.advice.after-while (entry.advice.after-while (table.unpack args))))
               passed-after-until (or passed-after-while
                                      (and (not passed-after-while) entry.advice.after-until (entry.advice.after-until (table.unpack args))))]

           (when (and passed-after-until
                      entry.advice.after)
             (entry.advice.after (table.unpack args)))
           (if passed-after-until ))))))))

(fn count
  [tbl]
  (->> tbl
       (reduce (fn [acc _x _key]
                 (+ acc 1))
               0)))

(fn dispatch-advice
  [key [_tbl & args]]
  (let [entry (. advice key)]
    (if (> (count entry.advice) 0)
        (do
          (apply-advice entry args))
        (do
          (entry.original (table.unpack args))))))

(fn afn
  [f]
  (let [key (tostring f)
        advice-reg (register-advisable key f)
        ret {:key key}]
    (setmetatable ret
                  {:__call (fn [...]
                             (dispatch-advice key [...]))
                   :__index (fn [tbl key]
                              (. tbl key))})
    ret
    ;(fn [...]
    ;  (dispatch-advice key [...]))
    ))

{: afn
 : add-advice
 : remove-advice}
