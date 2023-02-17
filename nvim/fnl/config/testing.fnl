(local {:core c} (require :config.utils))

(local debug-state {:active false})

(fn toggle-debug
  [bool]
  (tset debug-state :active bool))

(fn debug
  [& args]
  (when debug-state.active
   (print (unpack args))))

(fn ==
  [actual expected ctx]
  (accumulate [state {:ok true :msg ""}
               k expected-v (c.seq expected)
               &until (not state.ok)]
    (let [actual-v (and actual (. actual k))
          path-k (if (and ctx ctx.parent-k)
                   (.. ctx.parent-k "." k)
                   k)]
     (if
       ;; Early fail: Types don't match
       (not (= (type actual-v) (type expected-v)))
       {:ok false :msg (.. "expected " path-k " to be a " (type expected-v) " got " (type actual-v))}

       ;; Recurse if fail
       (and (= (type actual-v) :table))
       (== actual-v expected-v {:parent-k k})

       ;; Equivalent primitive values
       (= actual-v expected-v)
       state

       ;; Else
       {:ok false
        :msg (.. "expected " path-k " to be " (fennel.view expected-v) " got " (fennel.view actual-v))}))))

{: ==
 : debug
 : toggle-debug}
