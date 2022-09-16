(local packer (require :config.packer)) 
(local {:core   c
        :string s} (require :config.utils))
(require :config.tangle)

(fn kbd
  [key-str]
  "Helper to escape termcodes like <Space> to normal values"
  (vim.api.nvim_replace_termcodes key-str true false true))

(set vim.g.mapleader (kbd "<Space>"))
(set vim.g.maplocalleader (kbd "<Space>s"))
(set vim.g.autoread true)
(set vim.g.showmatch true)
(set vim.g.showmode true)
(set vim.g.autoindent true)
(set vim.g.smarttab true)
(set vim.g.expandtab true)
(set vim.g.tabstop 2)
(set vim.g.shiftwidth 2)

(set vim.opt.relativenumber true)
(set vim.opt.number true)
(set vim.opt.swapfile false)
(set vim.opt.clipboard :unnamedplus)
(set vim.opt.splitright true)
(set vim.opt.splitbelow true)

;; Replacing this with project nvim
;; (set vim.opt.autochdir true)

;; Packages
(fn pkg 
  [name ...]
  (let [args [...]]
    (if (= (length args) 0)
      name
      (let [[opts] args
            pkg-tbl {1 name}]
         (each [k v (pairs (or opts {}))]
           (tset pkg-tbl k v))
         pkg-tbl))))

(fn fnl->packer
  [f]
  (fn [use]
    (f #(use (pkg $...)))))

       
(packer.startup
  (fnl->packer
   (fn [use]
     (use "wbthomason/packer.nvim")

     (use "catppuccin/nvim"
          {:as "catppuccin"
           :config #(let [catppuccin (require :catppuccin)]
                      (set vim.g.catppuccin_flavour "mocha")
                      (catppuccin.setup)
                      ;; TODO: Replace with vim.colorscheme when upgrading to 0.8
                      (vim.cmd "colorscheme catppuccin"))})

     (use "jenterkin/vim-autosource"
          {:config (fn []
                    (set vim.g.autosource_conf_names
                         [".exrc.fnl" ".exrc.lua" ".exrc.vim" ".exrc"]))})

     (use "ahmedkhalf/project.nvim"
          {:config #(let [project-nvim (require :project_nvim)]
                     (project-nvim.setup))})

     (use "gbprod/yanky.nvim"
          {:config #(let [yanky (require :yanky)]
                      (yanky.setup))})
                      
     (use "nvim-telescope/telescope.nvim"
          {:requires ["nvim-lua/plenary.nvim"
                      "nvim-telescope/telescope-file-browser.nvim"]
           :after ["yanky.nvim"]           
           :config #(let [_ (require :yanky)
                          ts (require :telescope)
                          actions (require :telescope.actions)
                          sa (require :telescope.actions.state)
                          fb-actions (. ts :extensions :file_browser :actions)]

                      (fn magic-tab
                        [prmpt-bufnr]
                        (let [fb-actions (. ts :extensions :file_browser :actions)
                              prompt (sa.get_current_line)
                              base  (+ actions.toggle_selection actions.move_selection_worse)]
                          (if 
                           (or (= prompt "..")
                               (= prompt "../"))
                           (fb-actions.goto_parent_dir prmpt-bufnr)

                           (or (= prompt "~")
                               (= prompt "~/"))
                           (fb-actions.goto_home_dir prmpt-bufnr)

                           (base prmpt-bufnr))))

                      (ts.setup
                       {:defaults {:theme :ivy
                                   :mappings {:i {:<C-g> actions.close
                                                  :<Esc> actions.close}
                                              :n {:<C-g> actions.close}}}
                        :pickers {:find_files {:theme :ivy}
                                  :git_files {:theme :ivy}}
                        :extensions {:file_browser {:theme :ivy
                                                    :grouped true
                                                    :select_buffer true
                                                    :hijack_netrw false
                                                    :cwd_to_path true
                                                    :path "%:p:h"
                                                    :mappings {:i {"^" fb-actions.goto_parent_dir
                                                                   "~" fb-actions.goto_home_dir
                                                                   "<Tab>" magic-tab}}}}})
                                                                             
                      
                      (ts.load_extension "file_browser")
                      (ts.load_extension "projects")
                      (ts.load_extension "yank_history"))})


     (use "shoumodip/nvim-literate")

     (use "alexghergh/nvim-tmux-navigation"
          {:config #(let [tmux (require :nvim-tmux-navigation)]
                         (tmux.setup {:disable_when_zoomed true})

                         (vim.keymap.set :n :<C-h> tmux.NvimTmuxNavigateLeft)
                         (vim.keymap.set :n :<C-j> tmux.NvimTmuxNavigateDown)
                         (vim.keymap.set :n :<C-k> tmux.NvimTmuxNavigateUp)
                         (vim.keymap.set :n :<C-l> tmux.NvimTmuxNavigateRight)
                         (vim.keymap.set :n :<C-\\> tmux.NvimTmuxNavigateLastActive)
                         (vim.keymap.set :n :<C-Space> tmux.NvimTmuxNavigateNext))})


     (use "nvim-treesitter/nvim-treesitter"
          {:requires ["p00f/nvim-ts-rainbow"]
           :run ":TSUpdate"
           :config #(let [ts-cfg (require :nvim-treesitter.configs)]
                      (ts-cfg.setup {:sync_install true
                                     :auto_install true
                                     :highlight {:enable true}
                                     :rainbow {:enable true}}))})

     (use "nvim-orgmode/orgmode"
          {:after ["nvim-treesitter"]
           :config #(let [orgmode (require :orgmode)
                          tscfg (require :nvim-treesitter.configs)]
                      (orgmode.setup_ts_grammar)
                      (tscfg.setup {:highlight {:enable true
                                                :additional_vim_regex_highlighting [:org]}
                                    :ensure_installed [:org]})
                      (orgmode.setup))})

     (use "gpanders/nvim-parinfer")

     (use "nvim-colortils/colortils.nvim"
          {:cmd "Colortils"
           :config #(let [colortils (require :colortils)]
                      (colortils.setup {}))})
   
     (use "folke/which-key.nvim"
          {:config #(let [which-key (require :which-key)]
                     (which-key.setup {}))})

     (use "AndrewRadev/bufferize.vim")

     ;; Language specific

     (use "jaawerth/fennel.vim")

     (use "Olical/conjure")

     (use "guns/vim-sexp")

     (use "nvim-neorg/neorg"
          {:requires ["nvim-neorg/neorg-telescope"
                      "max397574/neorg-contexts"
                      "nvim-lua/plenary.nvim"]
           :after ["nvim-treesitter"]
           :config #(let [neorg (require :neorg)
                          tscfg (require :nvim-treesitter.configs)]
                      (tscfg.setup {:ensure_installed [:norg]
                                    :highlight {:enable true}})
                      (neorg.setup
                        {:load {:core.defaults {}
                                :core.norg.dirman {:config {:workspaces {:work "~/neorg/work"
                                                                         :personal "~/neorg/personal"}}}
                                :core.norg.concealer {}
                                :core.integrations.telescope {}
                                :external.context {}}}))})


     ;; Automatically set up your configuration after cloning packer.nvim
     ;; Put this at the end after all plugins

    (when packer.bootstrap
       (packer.sync)))))


;; Custom commands

(fn reload-config
  []
  (each [module-name exports (pairs package.loaded)]
    (when (s.starts-with? module-name "config")
      (tset package.loaded module-name nil)))
      
  (require :config.core)
  (print "Reloaded config"))

(vim.api.nvim_create_user_command :ReloadConfig reload-config {})

(fn fnlfile
  [{:fargs [filepath]}]
  (fennel.dofile filepath))

(vim.api.nvim_create_user_command :FnlFile fnlfile 
                                  {:nargs 1
                                   :complete "file"})
                                                    

;; Keybindings

(local wk (require :which-key))

(wk.register
  {:<leader>s {:name "+mode"}})

(vim.keymap.set :n :<Leader><Leader> "<cmd>Telescope commands<cr>"
                {:desc "M-x"}) 
(vim.keymap.set :n :<Leader>: ":"
                {:desc "Cmd"}) 
(vim.keymap.set :n :<Esc><Esc> ":nohl<Esc>" {:silent true}) ;; Replace with some kind of fuzzy finder thing
(vim.keymap.set [:n :v :i] :<C-g> :<Esc>)

;; Lisp

(wk.register
  {:<leader>l {:name "+editor"}})


(vim.keymap.set :n :<Leader>le ":FnlFile %<Esc>"
                {:desc "Eval fennel file"})

;; File

(wk.register
  {:<leader>f {:name "+file"}})

(vim.keymap.set :n :<Leader>ff "<cmd>Telescope file_browser<cr>"
                {:desc "Find file"})
(vim.keymap.set :n :<Leader>fp "<cmd>Telescope file_browser path=~/.config/nvim/fnl/config<cr>"
                {:desc "Edit neovim config"})

(vim.keymap.set [:n :v] :<Leader>fs "<cmd>w<cr>"
                {:desc "Save file"})

;; Buffer

(wk.register 
  {:<leader>b {:name "+buffer"}})

(vim.keymap.set :n :<Leader>bb "<cmd>Telescope buffers<cr>"
                {:desc "Switch buffer"})
(vim.keymap.set :n :<Leader>bd "<cmd>:bdelete<cr>"
                {:desc "Delete buffer"})

;; Project

(wk.register
  {:<leader>p {:name "+project"}})

(vim.keymap.set :n :<Leader>pf "<cmd>Telescope git_files<cr>"
                {:desc "Find project file"})

(vim.keymap.set :n :<Leader>pp "<cmd>Telescope projects<cr>"
                {:desc "Switch project"})


;; Org

(wk.register
  {:<leader>o {:name "+org"}})

;; Help

(wk.register
  {:<leader>h {:name "+help"}})

(vim.keymap.set :n :<Leader>hh ":help "
                {:desc "Help"})
(vim.keymap.set :n :<Leader>hr "<cmd>ReloadConfig<cr>"
                {:desc "Reload nvim cfg"})
(vim.keymap.set :n :<Leader>hR "<cmd>ReloadConfig<cr><cmd>PackerSync<cr>"
                {:desc "Reload cfg + sync"})
(vim.keymap.set :n :<Leader>hm "<cmd>messages<cr>"
                {:desc "messages"})
(vim.keymap.set :n :<Leader>hn "<cmd>vert Bufferize nmap<cr>"
                {:desc "normal bindings"})
(vim.keymap.set :n :<Leader>hi "<cmd>vert Bufferize imap<cr>"
                {:desc "insert bindings"})
(vim.keymap.set :n :<Leader>hv "<cmd>vert Bufferize vmap<cr>"
                {:desc "visual bindings"})

;; Yank

(wk.register
  {:<leader>y {:name "+yank"}})

(vim.keymap.set [:n :v] :<Leader>yk "<cmd>Telescope yank_history<cr>")

;; Window

(wk.register
  {:<leader>h {:name "+window"}})

(vim.keymap.set :n :<Leader>w- "<cmd>split<cr>" 
                {:silent true
                 :desc "Split Horizontal"})
(vim.keymap.set :n :<Leader>w<Bar> "<cmd>vsplit!<cr>" 
                {:silent true
                 :desc "Split Vertical"})
(vim.keymap.set :n :<Leader>wd "<cmd>q<cr>"
                {:silent true
                 :desc "Quit Window"})
(vim.keymap.set :n :<Leader>wx "<cmd>bdelete<cr><cmd>q<cr>"
                {:desc "Kill window"})

;; Quit

(wk.register
  {:<leader>q {:name "+quit"}})

(vim.keymap.set :n :<Leader>qq ":quitall<Esc>" {:silent true})

;; Normal bindings

;; Move by visual lines
(vim.keymap.set [:n :v] :j :gj)
(vim.keymap.set [:n :v] :k :gk)


;; OS X Bindings

(vim.keymap.set [:n :v] :<D-s> :<cmd>w<cr>)
(vim.keymap.set [:n :v] :<D-p> "<cmd>Telescope commands<cr>")

;; Clipboard

(vim.keymap.set ["n" "x"] "p" "<Plug>(YankyPutAfter)")
(vim.keymap.set ["n" "x"] "P" "<Plug>(YankyPutBefore)")
(vim.keymap.set ["n" "x"] "gp" "<Plug>(YankyGPutAfter)")
(vim.keymap.set ["n" "x"] "gP" "<Plug>(YankyGPutBefore)")

(vim.keymap.set :n :<C-n> "<Plug>(YankyCycleForward)")
(vim.keymap.set :n :<C-p> "<Plug>(YankyCycleBackward)")

(vim.keymap.set :i :<D-v> "<Esc><Plug>(YankyPutBefore)")
(vim.keymap.set :v :<D-c> "\"+y")

;; Insert bindings

;; Escape shortcut in insert mode

(vim.keymap.set :i :jj "<Esc>")

;; Conjure Evaluation

(vim.keymap.set [:n :i] :<C-c><C-e> "<cmd>ConjureEvalCurrentForm<cr><cmd>silent! call repeat#set(\" see\", 1)<cr>")
(vim.keymap.set [:n :i] :<C-c><C-f> "<cmd>ConjureEvalRootForm<cr><cmd>ConjureLogCloseVisible<cr><cmd>ConjureLogToggle<cr>")
(vim.keymap.set [:n :i] :<C-c><C-m> "<cmd>ConjureEvalMarkedForm<cr>")
(vim.keymap.set [:n :i] :<C-c><C-c> "<cmd>ConjureEvalRootForm<cr><cmd>silent! call repeat#set(\" ser\", 1)<cr>")
(vim.keymap.set [:n :i] :<C-c><C-l> "<cmd>ConjureEvalBuf<cr><cmd>silent! call repeat#set(\" seb\", 1)<cr>")
(vim.keymap.set [:n :i] :<C-c><C-k> "<cmd>ConjureEvalFile<cr><cmd>silent! call repeat#set(\" sef\", 1)<cr>")

(comment

  (print (vim.inspect (vim.opt.filetype:get)))

  (vim.api.nvim_get_option "nocp")

  (. vim.g "conjure#mapping#log_split")

  nil
  
  nil)
  
  
