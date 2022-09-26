(local M {})

(fn M.OpenTrouble
  []
  (let [trouble (require :trouble)]
    (trouble.open)))

(comment
  (M.OpenTrouble))

M
