(local packer (require :config.packer))
(local {:core c :string s} (require :config.utils))

;; Just exploring writing a custom statusline from scratch
;; (require :config.statusline)

(fn kbd [key-str]
  "Helper to escape termcodes like <Space> to normal values"
  (vim.api.nvim_replace_termcodes key-str true false true))

(set vim.g.mapleader (kbd :<Space>))
(set vim.g.maplocalleader (kbd :<Space>m))

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
(set vim.opt.guifont ["Operator Mono SSm Lig:h14"])
(set vim.opt.textwidth 80)
(set vim.opt.foldmethod :manual)
(set vim.opt.updatetime 250)
(set vim.opt.lazyredraw false)

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

(packer.startup
  (fnl->packer
    (fn [use]
      (use :wbthomason/packer.nvim)
      (use :catppuccin/nvim
           {:as :catppuccin
            :config #(let [catppuccin (require :catppuccin)
                           statusline (require :config.statusline)]
                       (set vim.g.catppuccin_flavour
                            :mocha)
                       (catppuccin.setup)
                       ;; TODO: Replace with vim.colorscheme when upgrading to 0.8
                       (vim.cmd "colorscheme catppuccin")
                       (statusline.setup))})

      (use :jenterkin/vim-autosource
           {:config (fn []
                      (set vim.g.autosource_conf_names
                           [:.exrc.fnl
                            :.exrc.lua
                            :.exrc.vim
                            :.exrc]))})
      (use :ahmedkhalf/project.nvim
           {:config #(let [project-nvim (require :project_nvim)]
                       ;; Ignoring null-ls as it seems attached to main project
                       (project-nvim.setup {:ignore_lsp [:null-ls]}))})
      (use :gbprod/yanky.nvim
           {:config #(let [yanky (require :yanky)]
                       (yanky.setup))})
      (use :nvim-telescope/telescope.nvim
           {:requires [:nvim-lua/plenary.nvim
                       :nvim-telescope/telescope-file-browser.nvim
                       :nvim-telescope/telescope-ui-select.nvim
                       :kyazdani42/nvim-web-devicons]
            :after [:yanky.nvim]
            :config #(require :config.plugins.telescope)})
      (use :shoumodip/nvim-literate)
      (use :alexghergh/nvim-tmux-navigation
           {:config #(let [tmux (require :nvim-tmux-navigation)]
                       (tmux.setup {:disable_when_zoomed true}))})
      (use :nvim-treesitter/nvim-treesitter
           {:requires [:p00f/nvim-ts-rainbow
                       :nvim-treesitter/nvim-treesitter-context]
            :run ":TSUpdate"
            :config #(let [ts-cfg (require :nvim-treesitter.configs)
                           ts-ctx (require :treesitter-context)
                           parsers (require :nvim-treesitter.parsers)
                           parser-cfg (parsers.get_parser_configs)]
                       (ts-cfg.setup {:sync_install true
                                      :auto_install true
                                      :highlight {:enable true}
                                      :rainbow {:enable true}
                                      :indent {:enable true}})
                       (ts-ctx.setup)
                       (set parser-cfg.markdown.filetype_to_parsername
                            :octo))})

      (use :nvim-orgmode/orgmode
           {:after [:nvim-treesitter]
            :config #(require :config.plugins.org)})

      (use :gpanders/nvim-parinfer)
      (use :uga-rosa/ccc.nvim
           {:config #(let [ccc (require :ccc)]
                       (ccc.setup {:bar_char "█"
                                   :default_color "#00ffcc"
                                   :toggle_alpha true
                                   :inputs [ccc.input.hsl
                                            ccc.input.rgb
                                            ccc.input.cmyk]
                                   :outputs [ccc.output.hex
                                             ccc.output.css_rgb
                                             ccc.output.css_hsl]}))})
      (use :folke/which-key.nvim
           {:config #(let [which-key (require :which-key)]
                       (which-key.setup {}))})
      (use :AndrewRadev/bufferize.vim)
      ;; Language specific
      (use :jaawerth/fennel.vim)
      (use :Olical/conjure
           {:config #(do
                       (tset vim.g "conjure#mapping#log_split" "l-")
                       (tset vim.g "conjure#mapping#log_vsplit" "l/"))})
      (use :guns/vim-sexp
           {:config #(set vim.g.sexp_filetypes "")})
      (use :numToStr/Comment.nvim
           {:config #(let [cmnt (require :Comment)]
                       (cmnt.setup {:padding true
                                    :sticky true
                                    :mappings {:basic true
                                               :extra true
                                               :extended true}}))})
      (use :williamboman/mason.nvim
           {:after [:nvim-lspconfig]
            :config #(let [mason (require :mason)]
                       (mason.setup))})
      (use :williamboman/mason-lspconfig.nvim
           {:after [:mason.nvim
                    :nvim-lspconfig]
            :config #(let [masonlsp (require :mason-lspconfig)]
                       (masonlsp.setup {:automatic_installation true}))})
      (use :neovim/nvim-lspconfig
           {:after [:nvim-cmp
                    :nvim-notify
                    :lspsaga.nvim
                    :null-ls.nvim]
            :config #(require :config.plugins.lsp)})
      (use :TimUntersberger/neogit
           {:requires [:nvim-lua/plenary.nvim]
            :config #(let [neogit (require :neogit)]
                       (neogit.setup {:use_magit_keybindings true}))})
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
                       :hrsh7th/cmp-nvim-lsp-signature-help
                       :onsails/lspkind.nvim
                       (pkg :L3MON4D3/LuaSnip
                            {:tag :v1.*})
                       :nvim-lua/plenary.nvim]
            :config #(require :config.plugins.cmp)})
      (use :lewis6991/gitsigns.nvim
           {:config #(let [gitsigns (require :gitsigns)]
                       (gitsigns.setup))})
      (use :jose-elias-alvarez/null-ls.nvim
           {:requires [:lewis6991/gitsigns.nvim]
            :config #(require :config.plugins.null-ls)})
      (use :folke/trouble.nvim
           {:require [:kyazdani42/nvim-web-devicons]
            :config #(let [trouble (require :trouble)]
                       (trouble.setup))})
      (use :rcarriga/nvim-notify
           {:config #(set vim.notify (require :notify))})
      (use :glepnir/lspsaga.nvim
           {:branch :main
            :config #(let [lspsaga (require :lspsaga)]
                       (lspsaga.setup
                         {:finder_action_keys {:open "<cr>"
                                               :vsplit "/"
                                               :split "-"}}))})
      (use :AckslD/nvim-FeMaco.lua
           {:config #(let [femaco (require :femaco)]
                       (femaco.setup))})
      (use :sakhnik/nvim-gdb {:cmd :!./install.sh})
      (use :tpope/vim-repeat)
      (use :kylechui/nvim-surround
           {:config #(let [surround (require :nvim-surround)]
                       (surround.setup))})
      (use "lcheylus/overlength.nvim"
            {:config #(let [overlength (require :overlength)]
                        (overlength.setup
                          {:textwidth_mode 1
                           :default_overlength 80
                           :grace_length 1
                           :highlight_to_eol true
                           :bg "#0f0b0b"
                           :disable_ft ["" "qf" "help" "man" "packer" "NvimTree" "Telescope" "WhichKey"]}))})
      (use "stevearc/dressing.nvim"
           {:config #(let [dressing (require :dressing)]
                       (dressing.setup {:select {:enabled false}}))})

      (use "andrewferrier/wrapping.nvim"
           {:config #(let [wrapping (require :wrapping)]
                       (wrapping.setup
                         {:auto_set_mode_filetype_allowlist [:asciidoc
                                                             :gitcommit
                                                             :mail
                                                             :markdown
                                                             :norg
                                                             :org
                                                             :text
                                                             :tex]}))})
      (use "seblj/nvim-tabline"
           {:requires ["kyazdani42/nvim-web-devicons"]
            :config #(let [tabline (require :tabline)]
                       (tabline.setup
                         {}))})

     (use "soywod/himalaya"
          {:after [:telescope.nvim]})

     (comment
      (use "~/.local/share/nvim/site/pack/packer/opt/himalaya/vim"
           {:after [:himalaya]
            :config #(do
                      (set vim.g.himalaya_mailbox_picker :telescope)
                      (set vim.g.himalaya_telescope_preview_enabled 1))}))

     (use "GnikDroy/projections.nvim"
          {:after [:telescope.nvim]
           :config #(require :config.plugins.projections)})

     (use "debugloop/telescope-undo.nvim"
          {:after [:telescope.nvim]
           :config #(let [telescope (require :telescope)]
                      (telescope.load_extension "undo"))})

     (use "m4xshen/autoclose.nvim")

     (use "jonsmithers/vim-html-template-literals"
          {:config #(do
                      (set vim.g.htl_all_templates 1))})

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

(fn lisp-filetype?
  [filetype]
  (let [filetypes (or (. vim.g "conjure#filetypes") {})]
    (vim.tbl_contains filetypes filetype)))

(comment (lisp-filetype? :fennel)
  (lisp-filetype? :rust))

;; Custom commands

(fn reload-statusline
  []
  (tset package.loaded :config.statusline nil)
  (let [sl (require :config.statusline)]
    (sl.setup)))

(vim.api.nvim_create_user_command :ReloadStatusLine reload-statusline {})

(fn reload-config []
  (let [modules []]
   (each [module-name exports (pairs package.loaded)]
     (when (and (s.starts-with? module-name :config)
                (not= module-name :config.fennel))
       (table.insert modules module-name)
       (tset package.loaded module-name nil)))
   (require :config.core)
   (each [_i module-name (ipairs modules)]
     (when (not= module-name :config.core)
       (require module-name)))
   (print "Reloaded config")))

(comment
  (require "init")
  (reload-config)
  package.loaded)

(vim.api.nvim_create_user_command :ReloadConfig reload-config {})
;; (vim.api.nvim_create_user_command :ReloadConfig (fn [] nil) {})


(fn fnlfile
  [{:fargs [filepath]}]
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
(vim.keymap.set :n :<Leader>bd "<cmd>:bprev<cr><cmd>:bdelete #<cr>"
                {:desc "Delete buffer"})

(vim.keymap.set :n :<Leader>bk "<cmd>:bdelete<cr>"
                {:desc "Kill buffer & window"})

(vim.keymap.set :n :<Leader>bp "<cmd>:bprevious<cr>" {:desc "Previous buffer"})

(vim.keymap.set :n :<Leader>bn "<cmd>:bnext<cr>" {:desc "Next buffer"})

;; Project

(wk.register {:<leader>p {:name :+project}})

(vim.keymap.set :n :<Leader>pf "<cmd>Telescope find_files<cr>"
                {:desc "Find project file"})

(vim.keymap.set :n :<Leader>pP "<cmd>Telescope projects<cr>"
                {:desc "Switch project"})

(vim.keymap.set "n" :<Leader>pp "<cmd>Telescope projections<cr>")

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

(vim.keymap.set :n :<Leader>hs "<cmd>ReloadStatusLine<cr>"
                {:desc "Reload statusline"
                 :noremap true})

;; Help > Plugins

(wk.register {:<leader>hp :+plugins})

(vim.keymap.set :n :<Leader>hpf :<cmd>ReloadFeline<cr> {:desc "Reload Feline"})

;; Insert operations

(wk.register {:i {:name :+insert}})

(vim.keymap.set :n :<leader>ic :<cmd>CccPick<cr>
                {:desc "Pick color" :remap false :silent true})

;; Lisp

(wk.register {:<leader>k {:name :+lisp}})
(wk.register {:<leader>k= {:name :+indent}})

(comment
  (vim.cmd "(sexp_indent)")
  (vim.cmd "<Plug>(sexp_raise_element)"))

(fn sexp
  [cmd]
  (.. "<cmd>ParinferOff<cr>" cmd "<cmd>ParinferOn<cr>"))
   ;(vim.cmd "ParinferOn")))

(comment
  (vim.cmd "execute normal <Plug>(sexp_raise_element)")
  (do
    (vim.cmd "ParinferOff")
    (vim.cmd "execute normal <Plug>(sexp_raise_element)<cr>")
    (vim.cmd "ParinferOn")))


(vim.keymap.set :n :<leader>k== (sexp "<Plug>(sexp_indent)<cr>") {:desc :indent})
(vim.keymap.set :n :<leader>k=- (sexp "<Plug>(sexp_indent_top)<cr>")
                {:desc "indent top"})
(vim.keymap.set :n :<leader>kw (sexp "<Plug>(sexp_round_tail_wrap_element)<cr>")
               {:desc "wrap ("})
(vim.keymap.set :n "<leader>k[" (sexp "<Plug>(sexp_square_tail_wrap_element)<cr>")
              {:desc "wrap ["})
(vim.keymap.set :n "<leader>k{" (sexp "<Plug>(sexp_curly_tail_wrap_element)<cr>")
                {:desc "wrap {"})
(vim.keymap.set :n :<leader>kr (sexp "<Plug>(sexp_raise_element)<cr>") {:desc :raise})
(vim.keymap.set :n :<leader>kc (sexp "<Plug>(sexp_convolute)<cr>") {:desc :convolute})
(vim.keymap.set :n :<leader>ks (sexp "<Plug>(sexp_capture_next_element)")
                {:desc :slurp})
(vim.keymap.set :n :<leader>kS (sexp "<Plug>(sexp_capture_prev_element)")
                {:desc "slurp backward"})
(vim.keymap.set :n :<leader>kb (sexp "<Plug>(sexp_emit_tail_element)")
                {:desc "barf"})
(vim.keymap.set :n :<leader>kB (sexp "<Plug>(sexp_emit_head_element)")
                {:desc "barf backward"})
(vim.keymap.set :n :<leader>kk (sexp "<Plug>(sexp_move_to_prev_element_head)")
                {:desc "prev element"})
(vim.keymap.set :n :<leader>kK (sexp "<Plug>(sexp_move_to_prev_top_element)")
                {:desc "prev top element"})
(vim.keymap.set :n :<leader>kj (sexp "<Plug>(sexp_move_to_next_element_head)")
                {:desc "next element"})
(vim.keymap.set :n :<leader>kJ (sexp "<Plug>(sexp_move_to_next_top_element)")
                {:desc "next top element"})
(vim.keymap.set :n :<leader>kh (sexp "<Plug>(sexp_flow_to_prev_leaf_head)")
                {:desc "back element"})
(vim.keymap.set :n :<leader>kl (sexp "<Plug>(sexp_flow_to_next_leaf_head)")
                {:desc "next element"})
(vim.keymap.set :n :<leader>kt (sexp "<Plug>(sexp_swap_element_backward)")
                {:desc "transition"})
(vim.keymap.set :n :<leader>kT (sexp "<Plug>(sexp_swap_element_forward)")
                {:desc "transition forward"})
(vim.keymap.set :n "<leader>kW" (sexp "<Plug>(sexp_splice_list)")
                {:desc "splice"})
(comment
 nil
 [ 1 2 3]

 nil)


;; Quit

(wk.register {:<leader>q {:name :+quit}})

(vim.keymap.set :n :<Leader>qq :<cmd>quitall<cr> {:silent true})

;; Toggle

(wk.register {:<leader>t {:name :+toggle}})
(vim.keymap.set :n :<Leader>tf :<cmd>ToggleFormatting<cr>
                {:silent true :remap false :desc :Auto-formatting})
(vim.keymap.set :n :<Leader>tr
                (fn []
                  (if vim.bo.readonly
                     (set vim.bo.readonly false)
                     (set vim.bo.readonly true)))
                {:silent true :remap false :desc :Readonly})
(vim.keymap.set :n :<Leader>tw "<cmd>ToggleWrapMode<cr>"
                {:remap false :desc "Toggle wrapping"})

;; Window

(wk.register {:<leader>h {:name :+window}})

(vim.keymap.set :n :<Leader>w- :<cmd>split<cr>
                {:silent true :desc "Split Horizontal"})
(vim.keymap.set :n :<Leader>w/ :<cmd>vsplit!<cr>
                {:silent true :desc "Split Vertical" :noremap true})
(vim.keymap.set :n :<Leader>wd :<cmd>q<cr> {:silent true :desc "Quit Window"})
(vim.keymap.set :n :<Leader>wx :<cmd>bdelete<cr><cmd>q<cr>
                {:desc "Kill window"})
(vim.keymap.set :n :<Leader>w= "<cmd>wincmd =<cr>"
                {:desc "Equalize"})

;; Undo

(vim.keymap.set :n :<leader>u "<cmd>Telescope undo<cr>" {:desc "Undo"})

;; Tmux Navigation

(local tmux (require :nvim-tmux-navigation))

(vim.keymap.set :n :<Leader>wh tmux.NvimTmuxNavigateLeft {:desc "Window left"})
(vim.keymap.set :n :<Leader>wj tmux.NvimTmuxNavigateDown {:desc "Window down"})
(vim.keymap.set :n :<Leader>wk tmux.NvimTmuxNavigateUp {:desc "Window up"})
(vim.keymap.set :n :<Leader>wl tmux.NvimTmuxNavigateRight {:desc "Window right"})
(vim.keymap.set :n :<Leader>w<Space> tmux.NvimTmuxNavigateNext {:desc "Window next"})

(vim.keymap.set :n :<C-h>
               tmux.NvimTmuxNavigateLeft)
(vim.keymap.set :n :<C-j>
              tmux.NvimTmuxNavigateDown)
(vim.keymap.set :n :<C-k>
              tmux.NvimTmuxNavigateUp)
(vim.keymap.set :n :<C-l>
              tmux.NvimTmuxNavigateRight)
(vim.keymap.set :n :<C-Bslash>
              tmux.NvimTmuxNavigateLastActive)
(vim.keymap.set :n :<C-Space>
              tmux.NvimTmuxNavigateNext)

;; Yank

(wk.register {:<leader>y {:name :+yank}})

(vim.keymap.set [:n :v] :<Leader>yk "<cmd>Telescope yank_history<cr>")

;; Normal bindings

;; Move by visual lines
(vim.keymap.set [:n :v] :j :gj)
(vim.keymap.set [:n :v] :k :gk)

;; OS X Bindings

(vim.keymap.set [:n :v] :<D-s> :<cmd>w<cr>)
(vim.keymap.set [:n :v] :<D-p> "<cmd>Telescope find_files<cr>")
(vim.keymap.set [:n :v] :<D-S-P> "<cmd>Telescope commands<cr>")
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
(vim.keymap.set :c :<D-v> :<C-r>+ {:remap false})


;; Insert bindings

;; Escape shortcut in insert mode

(vim.keymap.set :i :jj :<Esc> {:noremap true})

;; Conjure Evaluation

(wk.register {:<leader>m {:name :+mode}})

(wk.register {:<leader>me {:name :+eval}})

(wk.register {:<leader>ml {:name :+log
                           :e "Log buf"
                           :g "Toggle log"
                           :l "Jump latest"
                           :q "Close log"
                           :r "Reset soft"
                           :R "Reset hard"
                           :s "Log split horizontal"
                           :t "Log tab"
                           :v "Log Split vertical"}})

(fn conjure-bindings
  [{: buf}]
  (vim.keymap.set [:n :i] :<C-c><C-c> :<LocalLeader>er
                  {:desc "Eval top-level form" :remap true :buffer buf})

  (vim.keymap.set [:n :i] :<C-c><C-e> :<LocalLeader>ee
                  {:desc "Eval form at point" :remap true :buffer buf})

  (vim.keymap.set [:n :i] :<C-c><C-f>
                  :<C-c><C-C><cmd>ConjureLogCloseVisible<cr><LocalLeader>lv
                  {:desc "Eval top-level pprint" :remap true :buffer buf})

  (vim.keymap.set [:n :i] :<C-c><C-m> :<LocalLeader>em
                  {:desc "Eval marked" :remap true :buffer buf})

  (vim.keymap.set [:n :i] :<C-c><C-l> :<LocalLeader>eb
                  {:desc "Eval buf" :remap true :buffer buf})

  (vim.keymap.set [:n :i] :<C-c><C-k> :<LocalLeader>ef
                  {:desc "Eval file" :remap true :buffer buf})

  (vim.keymap.set [:n :i] "<C-c>;" :<LocalLeader>ecr
                  {:desc "Eval comment below" :remap true :buffer buf}))

;; Git Commit Bindings

(fn neogit-bindings
  [{: buf}]
  (vim.api.nvim_buf_set_keymap buf :n :<C-c><C-c> "<cmd>wq<cr>"
                  {:desc "Commit" :noremap true})
  (vim.api.nvim_buf_set_keymap buf :n :<C-c><C-k> (.. "<cmd>bdelete! " buf "<cr>")
                  {:desc "Cancel" :noremap true}))


(vim.api.nvim_create_augroup :JConditionalBindings {:clear true})
(vim.api.nvim_create_autocmd
  [:BufEnter :BufWinEnter :BufNew]
  {:group  :JConditionalBindings
   :callback
   (fn [args]
     (if
       (lisp-filetype? vim.bo.filetype) (conjure-bindings args))
     nil)})

(vim.api.nvim_create_augroup :JNeogitBindings {:clear true})
(vim.api.nvim_create_autocmd
  :Filetype
  {:group :JNeogitBindings
   :pattern :NeogitCommitMessage
   :callback neogit-bindings})

(vim.api.nvim_create_augroup :JHelpBindings {:clear true})
(vim.api.nvim_create_autocmd
  :Filetype
  {:group :JHelpBindings
   :pattern :help
   :callback
   (fn [args]
     (vim.keymap.set :n :q "<cmd>q<cr>"
                     {:desc "Quit" :nowait true
                      :remap true :buffer args.buf}))})

(vim.api.nvim_create_augroup :JHimalaya {:clear true})
(vim.api.nvim_create_autocmd
  :Filetype
  {:group :JHimalaya
   :pattern :himalaya
   :callback
   (fn [args]
     nil)})

;; Document Editing

(vim.keymap.set [:n :i] "<C-c>'" :<cmd>FeMaco<cr>)

;; Command line
;; - Gotta have those common emacs bindings

(vim.keymap.set :c :<C-a> :<Home> {:remap false})
(vim.keymap.set :c :<C-e> :<End> {:remap false})
(vim.keymap.set :c :<C-b> :<S-Left> {:remap false})
(vim.keymap.set :c :<C-f> :<S-Right> {:remap false})

;; Custom plugins
;; - These are worth separating from normal plugin setup as these can be
;;   reloaded quickly with `<SPC>hr` vs going through the packer sync
;;   process
(require :config.plugins.org-tangle)

(comment ;; eval these
  (vim.opt.path:prepend :$HOME/.asdf/shims)
  (vim.opt.path:get)
  (let [telescope (require :telescope)]
    (print (vim.inspect telescope)))
  (print (vim.inspect (vim.opt.filetype:get)))
  (let [ts (require :conjure.tree-sitter)]
    (ts.node->str (ts.get-root)))
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
  (or (not (= (vim.api.nvim_buf_get_option 0 :buftype) ""))
      (= (vim.api.nvim_buf_get_name 0) ""))
  (let [sources (require :null-ls.sources)
        ft (vim.api.nvim_buf_get_option 0 :filetype)]
    (vim.inspect (icollect [_ source (ipairs (sources.get_all))]
                   (if (sources.is_available source ft)
                       source))))
  (let [saga (require :lspsaga)]
    (saga.init_lsp_saga))
  (let [client (require :null-ls.client)]
    (print (vim.inspect client))
    (client.supports_method :textDocument/formatting))
  nil)
