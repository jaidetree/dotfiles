(local cmp (require :cmp))
(local cmp-git (require :cmp_git))
(local lspkind (require :lspkind))

(set vim.opt.completeopt [:menu :menuone :noselect])

(comment (vim.opt.completeopt:get))

(fn expand-snippet [args]
  (let [luasnip (require :luasnip)]
    (luasnip.lsp_expand args.body)))

(local lspkind-format (lspkind.cmp_format {:mode :symbol_text :preset :codicons} :maxwidth 50))

(fn format-completion
  [entry vim-item]
  (let [kind (lspkind-format entry vim-item)
        [kind-label menu] (vim.split kind.kind "%s" {:trimempty true})]
    (set kind.kind (.. " " kind-label " "))
    (set kind.menu (.. "    (" menu ")"))
    kind))
  

(cmp.setup {:snippet {:expand expand-snippet}
            :mapping {:<C-Space> (cmp.mapping.complete)
                      :<CR> (cmp.mapping.confirm {:select false})
                      :<Tab> (cmp.mapping.select_next_item)
                      :<S-Tab> (cmp.mapping.select_prev_item)
                      :<C-b> (cmp.mapping.scroll_docs -4)
                      :<C-f> (cmp.mapping.scroll_docs 4)
                      :<C-g> (cmp.mapping.abort)}
            :formatting {:fields ["kind" "abbr" "menu"]
                         :format format-completion}
            :sources (cmp.config.sources [{:name :nvim_lsp}
                                          {:name :luasnip}
                                          {:name :cmp_git}
                                          {:name :buffer}
                                          {:name :nvim_lsp_signature_help}])})

(cmp.setup.filetype :gitcommit
                    {:sources (cmp.config.sources [{:name :cmp_git}]
                                                  {:name :buffer})})

(cmp.setup.cmdline "/" {:mapping (cmp.mapping.preset.cmdline)
                        :sources [{:name :buffer}]})

(cmp.setup.cmdline ":"
                   {:mapping (cmp.mapping.preset.cmdline)
                    :sources (cmp.config.sources [{:name :path}]
                                                 {:name :cmdline})})

