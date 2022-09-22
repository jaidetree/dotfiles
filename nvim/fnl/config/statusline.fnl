;; Exploring the minimum requirements to render a custom statusline
  
(local M {})

(fn render []
  "%f %=%h%m%r [%l,%c] %p%% | %L loc ")

(fn setup
  []
  (set vim.o.statusline "%{%v:lua.require('config.statusline').render()%}"))

(fn reload
  []
  (tset package.loaded :config.statusline {: render
                                           : setup})
  (require :config.statusline)
  (setup))

(setup)

(set M.render render)
(set M.setup setup)

(reload)

M
