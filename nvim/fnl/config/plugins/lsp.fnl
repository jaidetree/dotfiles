(local lspcfg (require :lspconfig))
(local twcfg (require :lspconfig.server_configurations.tailwindcss))
(local cmp-nvim-lsp (require :cmp_nvim_lsp))
(local capabilities
       (cmp-nvim-lsp.update_capabilities (vim.lsp.protocol.make_client_capabilities)))

(local wk (require :which-key))

(fn on-attach [client bufnr]
  (let [bufopts {:noremap true :silent true :buffer bufnr}]
    (fn map [lhs rhs ...]
      (let [args [...]
            opts (or (. args 1) {})]
        (vim.keymap.set :n (.. :<leader>c lhs) rhs
                        (vim.tbl_extend :force bufopts opts))))

    (map :d vim.lsp.buf.definition {:desc "Goto definition"})
    (map :D vim.lsp.buf.references {:desc "Goto references"})
    (map :i vim.lsp.buf.implementation {:desc "Goto implementation"})
    (map :h vim.lsp.buf.signature_help {:desc "Signature help"})
    (map :t vim.lsp.buf.type_definition {:desc "Goto type definition"})
    (map :r vim.lsp.buf.rename {:desc :Rename})
    (map :K vim.lsp.buf.hover {:desc :Documentation})
    ;; (map :K hover.hover {:desc :Documentation})
    ;; TODO: Replace with telescope UI command
    (map :a vim.lsp.buf.code_action {:desc "Code Actions"})
    (map :f vim.lsp.buf.format {:desc :Format})
    (map :wa vim.lsp.buf.add_workspace_folder {:desc "Create folder"})
    (map :wr vim.lsp.buf.remove_workspace_folder {:desc "Remove folder"})
    (map :wl #(print (vim.inspect (vim.lsp.buf.list_workspace_folders))))
    nil))

(local flags {:debounce_text_changes 150})
(local opts {:noremap true :silent true})

(wk.register {:<leader>l {:name :+lsp}})

(vim.keymap.set :n :<leader>le :<cmd>TroubleToggle<cr>
                (vim.tbl_extend :force opts {:desc "Open diganostics"}))

(vim.keymap.set :n :<leader>lq "<cmd>TroubleToggle loclist<cr>"
                (vim.tbl_extend :force opts {:desc "Location list"}))

(vim.keymap.set :n :<leader>lc
                (fn []
                  (print (vim.inspect (icollect [_ v (ipairs (vim.lsp.buf_get_clients))]
                                        [v.config.name v.config.root_dir]))))
                {:desc "Print clients"})

(comment (icollect [_ v (ipairs (vim.lsp.buf_get_clients))]
           {:name v.config.name :root v.config.root_dir})
  nil)

;; Other Bindings

(vim.keymap.set :n "[d" vim.diagnostic.goto_prev
                (vim.tbl_extend :force opts {:desc "Prev diagnostic issue"}))

(vim.keymap.set :n "]d" vim.diagnostic.goto_next
                (vim.tbl_extend :force opts {:desc "Next diagnostic issue"}))

(vim.keymap.set :n :<leader>cf vim.lsp.buf.format
                (vim.tbl_extend :force opts {:desc :Format}))

;; (vim.keymap.set :n :<leader>ce vim.diagnostic.open_float
;;                 (vim.tbl_extend :force opts {:desc "Diagnostics"}))

(vim.keymap.set :n :<leader>ce "<cmd>TroubleToggle document_diagnostics<cr>"
                (vim.tbl_extend :force opts {:desc :Diagnostics}))

(lspcfg.tsserver.setup {:on_attach on-attach : capabilities : flags})

(lspcfg.clojure_lsp.setup {:on_attach on-attach : capabilities : flags})

(lspcfg.tailwindcss.setup {:on_attach on-attach
                           : capabilities
                           : flags
                           :filetypes (vim.list_extend [:clojure]
                                                       twcfg.default_config.filetypes)})

(comment (vim.inspect twcfg))
