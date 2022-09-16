(local configdir (vim.fn.stdpath "config"))

;; Update runtime paths to leverage the ~/.config/nvim/{fnl,lua} dirs
;; This lets me organize my config by filetype, which we can then use
;; to load the fennel library

(let [rtp (vim.api.nvim_get_option "runtimepath")
      custompaths [(.. configdir "/fnl")
                   (.. configdir "/lua")]
      customrtp (table.concat custompaths ",")]
  (vim.api.nvim_set_option "runtimepath" (.. customrtp "," rtp)))

;; Load the fennel library

(global fennel (require :config.fennel))

;; Update fennel.path to load *.fnl files and /*/init.fnl files like lua does
;; so I can use lines like (local {: some-fn} (require :config.somelib))

(let [fnldir (.. configdir "/fnl")]
  (each [_ dir (ipairs ["/?.fnl" "/?/init.fnl"])]
    (tset fennel :path (.. fnldir dir ";" fennel.path))))

;; Add the searcher which uses the fennel.path to compile the fennel lisp into
;; lua. There is a performance penalty with this approach but it also means 
;; being able to eval forms at runtime to update the config.

(table.insert package.loaders 1 fennel.searcher)

;; Load the core configuration file from ./fnl/config/core.fnl
(local cfg (require :config.core))

;; Export the cfg object. Should prevent it from being garbace collected
cfg
