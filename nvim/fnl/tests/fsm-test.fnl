(import-macros testing :config.macros.testing)
(local fsm (require :config.fsm))
(local p (require :config.predicates))
(local {: == : === : toggle-debug} (require :config.testing))

(testing.assert! (. {} :a))

(testing.print
  "Defmachine defines a machine spec"
  (fsm.defmachine 
    {:name :test-fsm-1
     :state {:green  {:green-duration p.number?}
             :red    {:red-duration p.number?}
             :yellow {:yellow-duration p.number?}}
     :actions {:change (p.map {:label p.str?})}})
  :== {:name :test-fsm-1
       :transitions {}})
