(local lspcfg (require :lspconfig))
(local twcfg (require :lspconfig.server_configurations.tailwindcss))
(local cmp-nvim-lsp (require :cmp_nvim_lsp))
(local capabilities
       (cmp-nvim-lsp.update_capabilities (vim.lsp.protocol.make_client_capabilities)))

(local wk (require :which-key))
(local async (require :plenary.async))
(local util (require :vim.lsp.util))
(local notify (require :notify))

(comment (notify :test))

(fn parse-markdown-links []
  nil)

(fn hover-handler [_ result ctx config]
  "Duplicates original implementation"
  (let [config (or config {})
        client (vim.lsp.get_client_by_id ctx.client_id)]
    (set config.focus_id ctx.method)
    (and result result.contents (print (vim.inspect result.contents)))
    (if (not (and result result.contents))
        (do
          ;; Soft-log this. For example hover when clojure and tailwindcss
          ;; clients are active
          (print (.. "lsp.hover[" (?. client :config :name)
                     "]: No information available")))
        (let [markdown-lines (-> result.contents
                                 (util.convert_input_to_markdown_lines)
                                 (util.trim_empty_lines))]
          (if (vim.tbl_isempty markdown-lines)
              (notify "No information available")
              (util.open_floating_preview (parse-markdown-links markdown-lines)
                                          :markdown config))))
    nil))

(comment ;; Trying lspsaga instead -- may switch back to hover.nvim
  (tset vim.lsp.handlers :textDocument/hover
        (vim.lsp.with hover-handler
                      {:border :single
                       :width 80
                       :opts {:offset_y 2}
                       :stylize_markdown true})))

(fn on-attach [client bufnr]
  (let [bufopts {:noremap true :silent true :buffer bufnr}]
    nil))

(local flags {:debounce_text_changes 150})
(local opts {:noremap true :silent true})

(fn map [lhs rhs ...]
  (let [args [...]
        custom-opts (or (. args 1) {})]
    (vim.keymap.set :n (.. :<leader>c lhs) rhs
                    (vim.tbl_extend :force opts custom-opts))))

(map :h "<cmd>Lspsaga lsp_finder<cr>" {:desc "Goto definition"})
(map :d "<cmd>Lspsaga peek_definition<cr>" {:desc "Goto definition"})
(map :D vim.lsp.buf.references {:desc "Goto references"})
(map :i vim.lsp.buf.implementation {:desc "Goto implementation"})
(map :h vim.lsp.buf.signature_help {:desc "Signature help"})
(map :t vim.lsp.buf.type_definition {:desc "Goto type definition"})
(map :r "<cmd>Lspsaga rename<cr>" {:desc :Rename})
(map :K "<cmd>Lspsaga hover_doc<cr>" {:desc :Documentation})
;; (map :K hover.hover {:desc :Documentation})
;; TODO: Replace with telescope UI command
(map :a "<cmd>Lspsaga code_action<cr>" {:desc "Code Actions"})
(map :f vim.lsp.buf.format {:desc :Format})
(map :wa vim.lsp.buf.add_workspace_folder {:desc "Create folder"})
(map :wr vim.lsp.buf.remove_workspace_folder {:desc "Remove folder"})
(map :wl #(print (vim.inspect (vim.lsp.buf.list_workspace_folders))))

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

(comment (vim.inspect twcfg)
  (lsp.buf_is_attached 0)
  nil)
