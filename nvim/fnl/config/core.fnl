(local packer (require :config.packer))
(local {:core c :string s} (require :config.utils))

(require :config.tangle)

(fn kbd [key-str]
  "Helper to escape termcodes like <Space> to normal values"
  (vim.api.nvim_replace_termcodes key-str true false true))

(set vim.g.mapleader (kbd :<Space>))
(set vim.g.maplocalleader (kbd :<Space>s))

(set vim.opt.autoread true)
(set vim.opt.showmode true)
(set vim.opt.autoindent true)
(set vim.opt.smarttab true)
(set vim.opt.expandtab true)
(set vim.opt.tabstop 2)
(set vim.opt.shiftwidth 2)
(set vim.opt.showmatch true)
(set vim.opt.relativenumber true)
(set vim.opt.number true)
(set vim.opt.swapfile false)
(set vim.opt.clipboard :unnamedplus)
(set vim.opt.splitright true)
(set vim.opt.splitbelow true)

;; Replacing this with project nvim
;; (set vim.opt.autochdir true)

;; Packages
(fn pkg [name ...]
  (let [args [...]]
    (if (= (length args) 0)
        name
        (let [[opts] args]
          (accumulate [pkg-tbl {1 name} k v (pairs (or opts {}))]
            (doto pkg-tbl
              (tset k v)))))))

(fn fnl->packer [f]
  (fn [use]
    (f #(use (pkg $...)))))

(packer.startup (fnl->packer (fn [use]
                               (use :wbthomason/packer.nvim)
                               (use :catppuccin/nvim
                                    {:as :catppuccin
                                     :config #(let [catppuccin (require :catppuccin)]
                                                (set vim.g.catppuccin_flavour
                                                     :mocha)
                                                (catppuccin.setup)
                                                ;; TODO: Replace with vim.colorscheme when upgrading to 0.8
                                                (vim.cmd "colorscheme catppuccin"))})
                               (use :jenterkin/vim-autosource
                                    {:config (fn []
                                               (set vim.g.autosource_conf_names
                                                    [:.exrc.fnl
                                                     :.exrc.lua
                                                     :.exrc.vim
                                                     :.exrc]))})
                               (use :ahmedkhalf/project.nvim
                                    {:config #(let [project-nvim (require :project_nvim)]
                                                (project-nvim.setup {}))})
                               (use :gbprod/yanky.nvim
                                    {:config #(let [yanky (require :yanky)]
                                                (yanky.setup))})
                               (use :nvim-telescope/telescope.nvim
                                    {:requires [:nvim-lua/plenary.nvim
                                                :nvim-telescope/telescope-file-browser.nvim
                                                :kyazdani42/nvim-web-devicons]
                                     :after [:yanky.nvim]
                                     :config #(require :config.plugins.telescope)})
                               (use :shoumodip/nvim-literate)
                               (use :alexghergh/nvim-tmux-navigation
                                    {:config #(let [tmux (require :nvim-tmux-navigation)]
                                                (tmux.setup {:disable_when_zoomed true})
                                                (vim.keymap.set :n :<C-h>
                                                                tmux.NvimTmuxNavigateLeft)
                                                (vim.keymap.set :n :<C-j>
                                                                tmux.NvimTmuxNavigateDown)
                                                (vim.keymap.set :n :<C-k>
                                                                tmux.NvimTmuxNavigateUp)
                                                (vim.keymap.set :n :<C-l>
                                                                tmux.NvimTmuxNavigateRight)
                                                (vim.keymap.set :n "<C-\\\\>"
                                                                tmux.NvimTmuxNavigateLastActive)
                                                (vim.keymap.set :n :<C-Space>
                                                                tmux.NvimTmuxNavigateNext))})
                               (use :nvim-treesitter/nvim-treesitter
                                    {:requires [:p00f/nvim-ts-rainbow]
                                     :run ":TSUpdate"
                                     :config #(let [ts-cfg (require :nvim-treesitter.configs)
                                                    parsers (require :nvim-treesitter.parsers)
                                                    parser-cfg (parsers.get_parser_configs)]
                                                (ts-cfg.setup {:sync_install true
                                                               :auto_install true
                                                               :highlight {:enable true}
                                                               :rainbow {:enable true}})
                                                (set parser-cfg.markdown.filetype_to_parsername
                                                     :octo))})
                               (use :nvim-orgmode/orgmode
                                    {:after [:nvim-treesitter]
                                     :config #(let [orgmode (require :orgmode)
                                                    tscfg (require :nvim-treesitter.configs)]
                                                (orgmode.setup_ts_grammar)
                                                (tscfg.setup {:highlight {:enable true
                                                                          :additional_vim_regex_highlighting [:org]}
                                                              :ensure_installed [:org]})
                                                (orgmode.setup))})
                               (use :gpanders/nvim-parinfer)
                               (use :nvim-colortils/colortils.nvim
                                    {:cmd :Colortils
                                     :config #(let [colortils (require :colortils)]
                                                (colortils.setup {}))})
                               (use :folke/which-key.nvim
                                    {:config #(let [which-key (require :which-key)]
                                                (which-key.setup {}))})
                               (use :AndrewRadev/bufferize.vim)
                               ;; Language specific
                               (use :jaawerth/fennel.vim)
                               (use "~/projects/conjure")
                               (use :guns/vim-sexp
                                    {:config #(set vim.g.sexp_filetypes "")})
                               (use :nvim-neorg/neorg
                                    {:requires [:nvim-neorg/neorg-telescope
                                                :max397574/neorg-contexts
                                                :nvim-lua/plenary.nvim]
                                     :after [:nvim-treesitter]
                                     :config #(let [neorg (require :neorg)
                                                    tscfg (require :nvim-treesitter.configs)]
                                                (tscfg.setup {:ensure_installed [:norg]
                                                              :highlight {:enable true}})
                                                (neorg.setup {:load {:core.defaults {}
                                                                     :core.norg.dirman {:config {:workspaces {:work "~/neorg/work"
                                                                                                              :personal "~/neorg/personal"}}}
                                                                     :core.norg.concealer {}
                                                                     :core.integrations.telescope {}
                                                                     :external.context {}}}))})
                               (use :numToStr/Comment.nvim
                                    {:config #(let [cmnt (require :Comment)]
                                                (cmnt.setup {:padding true
                                                             :sticky true
                                                             :mappings {:basic true
                                                                        :extra true
                                                                        :extended true}}))})
                               (use :williamboman/mason.nvim
                                    {:config #(let [mason (require :mason)]
                                                (mason.setup))})
                               (use :williamboman/mason-lspconfig.nvim
                                    {:after [:mason.nvim]
                                     :config #(let [masonlsp (require :mason-lspconfig)]
                                                (masonlsp.setup {:automatic_installation true}))})
                               (use :neovim/nvim-lspconfig
                                    {:after [:mason.nvim
                                             :mason-lspconfig.nvim
                                             :nvim-cmp]
                                     :config #(require :config.plugins.lsp)})
                               (use :TimUntersberger/neogit
                                    {:requires [:nvim-lua/plenary.nvim]
                                     :config #(let [neogit (require :neogit)]
                                                (neogit.setup))})
                               (use :pwntester/octo.nvim
                                    {:requires [:nvim-lua/plenary.nvim
                                                :nvim-telescope/telescope.nvim
                                                :kyazdani42/nvim-web-devicons]
                                     :after [:telescope.nvim]
                                     :config #(let [octo (require :octo)]
                                                (octo.setup))})
                               (use :hrsh7th/nvim-cmp
                                    {:requires [:hrsh7th/cmp-nvim-lsp
                                                :hrsh7th/cmp-buffer
                                                :hrsh7th/cmp-path
                                                :hrsh7th/cmp-cmdline
                                                :hrsh7th/cmp-git
                                                (pkg :L3MON4D3/LuaSnip
                                                     {:tag :v1.*})
                                                :nvim-lua/plenary.nvim]
                                     :config #(require :config.plugins.cmp)})
                               (use :jose-elias-alvarez/null-ls.nvim
                                    {:requires [:lewis6991/gitsigns.nvim]
                                     :config #(let [null-ls (require :null-ls)
                                                    augroup (vim.api.nvim_create_augroup :LspFormatting
                                                                                         {})]
                                                (null-ls.setup {:on_attach (fn [client
                                                                                bufnr]
                                                                             (when (client.supports_method :textDocument/formatting)
                                                                               (vim.api.nvim_clear_autocmds {:group augroup
                                                                                                             :buffer bufnr})
                                                                               (vim.api.nvim_create_autocmd :BufWritePre
                                                                                                            {:group augroup
                                                                                                             :buffer bufnr
                                                                                                             :callback #(vim.lsp.buf.format)})))
                                                                :sources [null-ls.builtins.formatting.fnlfmt
                                                                          null-ls.builtins.formatting.zprint
                                                                          null-ls.builtins.diagnostics.checkmake
                                                                          null-ls.builtins.diagnostics.codespell
                                                                          null-ls.builtins.completion.spell
                                                                          null-ls.builtins.completion.luasnip
                                                                          null-ls.builtins.code_actions.gitsigns]}))})
                               (use :folke/trouble.nvim
                                    {:require [:kyazdani42/nvim-web-devicons]
                                     :config #(let [trouble (require :trouble)]
                                                (trouble.setup))})
                               ;; TODO: Install trouble to show diagnostics
                               ;; Automatically set up your configuration after cloning packer.nvim
                               ;; Put this at the end after all plugins
                               (when packer.bootstrap
                                 (packer.sync)))))

;; Advanced setup

(local wk (require :which-key))

;; Code & LSP

(wk.register {:<leader>c {:name :+code}})

(wk.register {:<leader>cw {:name :+workspace}})

(comment ;; Merges create new tables
  (let [tbl-a {:a 1}]
    {:merged (vim.tbl_extend :force tbl-a {:b 1}) :orig tbl-a}))

;; Utils

(fn lisp-filetype? [filetype]
  (let [filetypes (. vim.g "conjure#filetypes")]
    (vim.tbl_contains filetypes filetype)))

(comment (lisp-filetype? :fennel)
  (lisp-filetype? :rust))

;; Custom commands

(fn reload-config []
  (each [module-name exports (pairs package.loaded)]
    (when (s.starts-with? module-name :config)
      (tset package.loaded module-name nil)))
  (require :config.core)
  (print "Reloaded config"))

(vim.api.nvim_create_user_command :ReloadConfig reload-config {})

(fn fnlfile [{:fargs [filepath]}]
  (fennel.dofile filepath))

(vim.api.nvim_create_user_command :FnlFile fnlfile {:nargs 1 :complete :file})

;; Keybindings

(vim.keymap.set :n :<Leader><Leader> "<cmd>Telescope commands<cr>" {:desc :M-x})

(vim.keymap.set :n "<Leader>:" ":" {:desc :Cmd})

(vim.keymap.set :n :<Esc><Esc> :<cmd>nohl<cr> {:silent true})

;; Replace with some kind of fuzzy finder thing
(vim.keymap.set [:n :v :i] :<C-g> :<Esc>)

;; File > Editor

(wk.register {:<leader>fe {:name :+editor}})

(vim.keymap.set :n :<Leader>fee "<cmd>FnlFile %<cr>" {:desc "Eval fennel file"})

;; File

(wk.register {:<leader>f {:name :+file}})

(vim.keymap.set :n :<Leader>ff "<cmd>Telescope file_browser hidden=true<cr>"
                {:desc "Find file"})

(vim.keymap.set :n :<Leader>fp
                "<cmd>Telescope find_files cwd=~/.config/nvim<cr>"
                {:desc "Edit neovim config"})

(vim.keymap.set [:n :v] :<Leader>fs :<cmd>w<cr> {:desc "Save file"})

;; Buffer

(wk.register {:<leader>b {:name :+buffer}})

(vim.keymap.set :n :<Leader>bb "<cmd>Telescope buffers<cr>"
                {:desc "Switch buffer"})

;; Thanks to https://vim.fandom.com/wiki/Deleting_a_buffer_without_closing_the_window 
(vim.keymap.set :n :<Leader>bd "<cmd>:bnext<cr><cmd>:bdelete #<cr>"
                {:desc "Delete buffer"})

(vim.keymap.set :n :<Leader>bk "<cmd>:bdelete<cr>"
                {:desc "Kill buffer & window"})

(vim.keymap.set :n :<Leader>bp "<cmd>:bnext<cr>" {:desc "Previous buffer"})

(vim.keymap.set :n :<Leader>bn "<cmd>:bprevious<cr>" {:desc "Next buffer"})

;; Project

(wk.register {:<leader>p {:name :+project}})

(vim.keymap.set :n :<Leader>pf "<cmd>Telescope git_files<cr>"
                {:desc "Find project file"})

(vim.keymap.set :n :<Leader>pp "<cmd>Telescope projects<cr>"
                {:desc "Switch project"})

;; Org

(wk.register {:<leader>o {:name :+org}})

;; Git

(wk.register {:<leader>g {:name :+git}})

(vim.keymap.set :n :<leader>gg :<cmd>Neogit<cr>)

;; Help

(wk.register {:<leader>h {:name :+help}})

(vim.keymap.set :n :<Leader>hh ":help " {:desc :Help})

(vim.keymap.set :n :<Leader>hr :<cmd>ReloadConfig<cr> {:desc "Reload nvim cfg"})

(vim.keymap.set :n :<Leader>hR :<cmd>ReloadConfig<cr><cmd>PackerSync<cr>
                {:desc "Reload cfg + sync"})

(vim.keymap.set :n :<Leader>hm :<cmd>messages<cr> {:desc :messages})

(vim.keymap.set :n :<Leader>hn "<cmd>vert Bufferize nmap<cr>"
                {:desc "normal bindings"})

(vim.keymap.set :n :<Leader>hi "<cmd>vert Bufferize imap<cr>"
                {:desc "insert bindings"})

(vim.keymap.set :n :<Leader>hv "<cmd>vert Bufferize vmap<cr>"
                {:desc "visual bindings"})

;; Yank

(wk.register {:<leader>y {:name :+yank}})

(vim.keymap.set [:n :v] :<Leader>yk "<cmd>Telescope yank_history<cr>")

;; Window

(wk.register {:<leader>h {:name :+window}})

(vim.keymap.set :n :<Leader>w- :<cmd>split<cr>
                {:silent true :desc "Split Horizontal"})

(vim.keymap.set :n :<Leader>w<Bar> :<cmd>vsplit!<cr>
                {:silent true :desc "Split Vertical"})

(vim.keymap.set :n :<Leader>wd :<cmd>q<cr> {:silent true :desc "Quit Window"})

(vim.keymap.set :n :<Leader>wx :<cmd>bdelete<cr><cmd>q<cr>
                {:desc "Kill window"})

;; Quit

(wk.register {:<leader>q {:name :+quit}})

(vim.keymap.set :n :<Leader>qq ":quitall<Esc>" {:silent true})

;; Normal bindings

;; Move by visual lines
(vim.keymap.set [:n :v] :j :gj)
(vim.keymap.set [:n :v] :k :gk)

;; OS X Bindings

(vim.keymap.set [:n :v] :<D-s> :<cmd>w<cr>)
(vim.keymap.set [:n :v] :<D-p> "<cmd>Telescope commands<cr>")
(vim.keymap.set [:n :v] :<D-t> :<cmd>tabnew<cr>)
(vim.keymap.set [:n :v] "<D-;>" :gcc {:remap true})

;; Clipboard

(vim.keymap.set [:n :x] :p "<Plug>(YankyPutAfter)")
(vim.keymap.set [:n :x] :P "<Plug>(YankyPutBefore)")
(vim.keymap.set [:n :x] :gp "<Plug>(YankyGPutAfter)")
(vim.keymap.set [:n :x] :gP "<Plug>(YankyGPutBefore)")

(vim.keymap.set :n :<C-n> "<Plug>(YankyCycleForward)")
(vim.keymap.set :n :<C-p> "<Plug>(YankyCycleBackward)")

(vim.keymap.set :i :<D-v> "<Esc><Plug>(YankyPutAfter)")
(vim.keymap.set :v :<D-c> "\"+y")

;; Insert bindings

;; Escape shortcut in insert mode

(vim.keymap.set :i :jj :<Esc> {:noremap true})

;; Conjure Evaluation

(wk.register {:<leader>s {:name :+mode}})

(wk.register {:<leader>se {:name :+eval}})

(wk.register {:<leader>sl {:name :+log
                           :e "Log buf"
                           :g "Toggle log"
                           :l "Jump latest"
                           :q "Close log"
                           :r "Reset soft"
                           :R "Reset hard"
                           :s "Log split horizontal"
                           :t "Log tab"
                           :v "Log Split vertical"}})

(vim.keymap.set [:n :i] :<C-c><C-c> :<LocalLeader>er
                {:desc "Eval top-level form" :remap true})

(vim.keymap.set [:n :i] :<C-c><C-e> :<LocalLeader>ee
                {:desc "Eval form at point" :remap true})

(vim.keymap.set [:n :i] :<C-c><C-f>
                :<C-c><C-C><cmd>ConjureLogCloseVisible<cr><LocalLeader>lv
                {:desc "Eval top-level pprint" :remap true})

(vim.keymap.set [:n :i] :<C-c><C-m> :<LocalLeader>em
                {:desc "Eval marked" :remap true})

(vim.keymap.set [:n :i] :<C-c><C-l> :<LocalLeader>eb
                {:desc "Eval buf" :remap true})

(vim.keymap.set [:n :i] :<C-c><C-k> :<LocalLeader>ef
                {:desc "Eval file" :remap true})

(comment (vim.opt.path:prepend :$HOME/.asdf/shims)
  (vim.opt.path:get)
  (print (vim.inspect (vim.opt.filetype:get)))
  (print (vim.inspect (let [ts (require :conjure.tree-sitter)]
                        (ts.node->str (ts.get-root)))))
  (. vim.g "conjure#mapping#log_split")
  vim.g.conjure#filetypes
  (vim.opt.filetype:get)
  (vim.cmd :!tailwindcss-language-server)
  (vim.opt.path:get)
  (vim.opt.mouse:get)
  vim.g.neovide
  vim.g.neovide_input_use_logo
  vim.g.neovide_cursor_trail_length
  vim.g.neovide_cursor_animation_length
  nil)
