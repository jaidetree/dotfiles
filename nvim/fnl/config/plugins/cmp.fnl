(local cmp (require :cmp))
(local cmp-git (require :cmp_git))

(set vim.opt.completeopt [:menu :menuone :noselect])

(comment
  (vim.opt.completeopt:get))

(fn expand-snippet
  [args]
  (let [luasnip (require :luasnip)]
    (luasnip.lsp_expand args.body)))

(cmp.setup
  {:snippet {:expand expand-snippet}
   :mapping {:<C-Space> (cmp.mapping.complete)
             :<CR>      (cmp.mapping.confirm {:select true}) 
             :<C-b>     (cmp.mapping.scroll_docs -4)
             :<C-f>     (cmp.mapping.scroll_docs 4)
             :<C-g>     (cmp.mapping.abort)}
   :sources (cmp.config.sources
              [{:name :nvim_lsp}
               {:name :luasnip}
               {:name :cmp_git}]
              {:name :buffer})})

(cmp.setup.filetype 
  :gitcommit
  {:sources (cmp.config.sources 
              [{:name :cmp_git}]
              {:name :buffer})})

(cmp.setup.cmdline
  "/"
  {:mapping (cmp.mapping.preset.cmdline)
   :sources [{:name :buffer}]})

(cmp.setup.cmdline
  ":"
  {:mapping (cmp.mapping.preset.cmdline)
   :sources (cmp.config.sources
              [{:name :path}]
              {:name :cmdline})})
               
  
                    
