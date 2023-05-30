(import-macros {: assert!} :config.macros.testing)
(local p (require :config.predicates))
(local m {})

(set m.registry {})

(fn m.def
  [unique-key pred-or-spec]
  (assert!
    (p.str? unique-key)
    (.. "spec.def error: TypeError: unique-key should be a string. Received "
        (fennel.view unique-key)))
  (assert!
    (p.fn? pred-or-spec)
    (.. "spec.def error: TypeError: pred-or-spec should be a function. Received "
        (fennel.view pred-or-spec)))
  (let [spec (. m.registry unique-key)]
    (assert!
      (not spec)
      (.. "spec.def error: UniqueConstraint: A spec with the key " unique-key " already exists:\n"
          (fennel.view spec))))
  (tset m.registry :unique-key pred-or-spec))

(fn m.and
  [& predicates]
  (fn [value]
    (accumulate
      [res true
       _ p? (ipairs predicates)
       &until (not res)]
      (p? value))))

(fn m.or
  [& key-predicate-pairs]
  (assert!
    (= (% (length key-predicate-pairs) 2) 0)
    (.. "spec.or error: SyntaxError: Arguments must contain an even number of forms. Received "
        (fennel.view key-predicate-pairs)))
  (fn [value]
    ()))


(fn m.keys
  [])


(fn m.verify
  [])

(fn m.conform
  [])


;; Exports
m
