(local null-ls (require :null-ls))
(local augroup (vim.api.nvim_create_augroup :LspFormatting {}))

(fn null-ls-client? [client]
  (= client.name :null-ls))

(fn format-buf 
  [bufnr]
  (when (not vim.b.noformat)
    (vim.lsp.buf.format {: bufnr :filter null-ls-client?})))

(fn on-attach 
  [client bufnr]
  (when (client.supports_method :textDocument/formatting)
    (vim.api.nvim_clear_autocmds {:group augroup :buffer bufnr})
    (vim.api.nvim_create_autocmd :BufWritePre
                                 {:group augroup
                                  :buffer bufnr
                                  :callback #(format-buf bufnr)})))

(null-ls.setup
  {:debug true
   :on_attach on-attach
   :sources [;; TODO - Not loving the formatting decisions
             ;;        this tool makes atm
             ; null-ls.builtins.formatting.fnlfmt
             null-ls.builtins.formatting.zprint
             null-ls.builtins.diagnostics.checkmake
             null-ls.builtins.diagnostics.codespell
             ;; (null-ls.builtins.diagnostics.cspell.with
             ;;   {:args (fn [params]
             ;;            ["--language-id" params.ft
             ;;             "--show-suggestions"
             ;;             "stdin"])})
            
             null-ls.builtins.completion.spell
             null-ls.builtins.completion.luasnip
             null-ls.builtins.code_actions.gitsigns]})
                          
(fn toggle-formatting 
  []
  (let [disabled vim.b.noformat]
    (if disabled
        (do
          (set vim.b.noformat false)
          (vim.notify "Formatting on save enabled" :info {:title :Formatting}))
        (do
          (set vim.b.noformat true)
          (vim.notify "Formatting on save disabled" :warn {:title :Formatting})))))

(vim.api.nvim_create_user_command :ToggleFormatting toggle-formatting
                                  {:desc "Toggle auto null-ls formatting"})

(comment vim.b.noformat
  vim.log.levels
  (let [clients (vim.lsp.buf_get_clients)]
    (icollect [_ client (ipairs clients)]
      [client.name client.server_capabilities])))

(global dbg_client_info
        (fn []
          (fennel.view (vim.lsp.buf_get_clients))))
