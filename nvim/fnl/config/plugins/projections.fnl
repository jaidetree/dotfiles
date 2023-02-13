(local projections (require :projections))
(local telescope (require :telescope))

(projections.setup
  {:workspaces [["~/projects"   [".git"]]
                ["~/dotfiles"   []]]})

(local Session (require :projections.session))

;; Switch to project if vim was started in project dir
(let [switcher (require :projections.switcher)]
  (vim.api.nvim_create_autocmd
    [:VimEnter]
    {:callback #(when (= (vim.fn.argc) 0)
                  (switcher.switch (vim.loop.cwd)))}))


;; Auto-save session on exit
(vim.api.nvim_create_autocmd
  [:VimLeavePre]
  {:callback #(Session.store (vim.loop.cwd))})


;; Register a command to restore the last project session
(vim.api.nvim_create_user_command
 :RestoreProjectSession
 (fn []
  (Session.restore (vim.loop.cwd)))
 {})

(vim.opt.sessionoptions:append "localoptions")

(telescope.load_extension :projections)


(comment
  (vim.fn.stdpath "data")
  (vim.fn.stdpath "cache")

  ;; (.. (Path.new (vim.fn.stdpath "data")) "projections_workspaces.json")

  (let [Workspace (require :projections.workspace)
        utils (require :projections.utils)
        {: config} (require :projections.config)
        projects {}]
    (Workspace.get_workspaces_from_config))



  projections)
