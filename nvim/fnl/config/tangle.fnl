(local {:core c
        :string s} (require :config.utils))

(local state {:line      0
              :in-tangle false
              :level     0
              :files     {}})

;; Inspired by https://github.com/OrgTangle/org-babel-tangle.py/blob/master/org-babel-tangle


(fn tangle
  []
  "
  Org tangle-like function. Extremely hacky.
  "

  (let [lines (vim.api.nvim_buf_get_lines 0 0 -1 true)]
    (c.reduce 
      (fn [state line num]
        (if true {} {}))
      {}
      lines)))

(vim.api.nvim_create_user_command :OrgTangle tangle {})


