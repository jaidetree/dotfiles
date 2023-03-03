(local org (require :orgmode))
(local org-mappings (require :orgmode.org.mappings))
(local config (require :orgmode.config))
(local ts-utils (require :nvim-treesitter.ts_utils))

(org.setup_ts_grammar)
(org.setup
  {:org_agenda_files ["~/org/*" "~/org/**/*"]
   :org_default_notes_file "~/org/notes.org"
   :org_indent_mode :noindent})

(fn link?
  [_node]
  (not= (org-mappings._get_link_under_cursor) nil))


(fn todo-headline?
  [_node]
  (let [keywords (. (config:get_todo_keywords) :ALL)
        line (vim.fn.getline :.)]
    (accumulate
      [found false
       _i kw (ipairs keywords)
       &until found]
      (line:find (.. "* " kw) 1 true))))

(comment
  (let [line "** TODO Some Task"]
    (line:find (.. "* " "TODO") 1 true)))

(local actions
  {link?          "org_mappings.open_at_point"
   :timestamp     "org_mappings.change_date"
   todo-headline? "org_mappings.todo_next_state"
   :listitem      "org_mappings.toggle_checkbox"
   :list          "org_mappings.toggle_checkbox"})

(local default-action "org_mappings.cycle")

(fn node->action
  [node]
  (let [state {:action default-action}]
    (let [node-type (node:type)]
      (each [identifier action (pairs actions)
             &until (not= state.action default-action)]
        (print "node->action" (fennel.view {:type node-type : identifier}))
        (if
          ;; Identifier is a function that behaves similarly to a predicate
          (= (type identifier) :function)
          (when (identifier node)
            (set state.action action))

          ;; Identifier matches node type string
          (= node-type identifier)
          (set state.action action)))

      (when (= state.action default-action)
       (let [parent (node:parent)
             row (node:range)]
         (when (= row (parent:range))
           (let [parent-action (node->action parent)]
             (set state.action parent-action))))))

    state.action))


(fn toggle-org-item
  []
  (let [node (ts-utils.get_node_at_cursor)
        action-str (node->action node)]
    (when action-str
      (org.action action-str))))

(vim.api.nvim_create_autocmd
  "FileType"
  {:pattern "org"
   :callback (fn []
               (vim.api.nvim_buf_set_keymap
                 0 :n :<cr> ""
                 {:callback toggle-org-item
                  :noremap true})
               (vim.api.nvim_buf_set_keymap
                 0 :n :<leader>oxt "<cmd>Tangle<cr>"
                 {:desc "tangle"
                  :noremap true}))})

