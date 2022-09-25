;; DO NOT EDIT THIS FILE
;; Generated from ../neovim.norg
(local configdir (vim.fn.stdpath "config"))
(let [rtp (vim.api.nvim_get_option "runtimepath")
      custompaths [(.. configdir "/fnl")
                   (.. configdir "/lua")]
      customrtp (table.concat custompaths ",")]
  (vim.api.nvim_set_option "runtimepath" (.. customrtp "," rtp)))
(global fennel (require :config.fennel))
(let [fnldir (.. configdir "/fnl")]
  (each [_ dir (ipairs ["/?.fnl" "/?/init.fnl"])]
    (tset fennel :path (.. fnldir dir ";" fennel.path))))
(table.insert package.loaders 1 fennel.searcher)
(local cfg (require :config.core))
cfg