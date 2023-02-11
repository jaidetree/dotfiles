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

  (.. (Path.new (vim.fn.stdpath "data")) "projections_workspaces.json")

  (let [Workspace (require :projections.workspace)
        utils (require :projections.utils)
        {: config} (require :projections.config)
        projects []]
    (Workspace.get_workspaces_from_config))


  (let [Workspace (require :projections.workspace)
        utils (require :projections.utils)
        {: config} (require :projections.config)
        projects []]
    (tset Workspace 
          :get_workspaces_from_config
          (fn []
            (utils._unique_workspaces
             (accumulate [dirs []
                          _ ws (ipairs config.workspaces)]
               (do
                 (if
                  (= (type ws) "table")
                  (let [(path patterns) (unpack ws)]
                   (table.insert dirs (Workspace.new (Path.new path) patterns)))

                  (= (type ws) "string")
                  (table.insert dirs (Workspace.new (Path.new ws) config.patterns)))
                 dirs))))))
    
   


  (let [workspace (require :projections.workspace)
        utils (require :projections.utils)
        {: config} (require :projections.config)
        projects []]
    (utils._unique_workspaces
      (accumulate [dirs []
                   _ ws (ipairs config.workspaces)]
        (do
           (if
               (= (type ws) "table")
               (let [(path patterns) (unpack ws)]
                 (table.insert dirs (workspace.new (Path.new path) patterns)))

               (= (type ws) "string")
               (table.insert dirs (workspace.new (Path.new ws) config.patterns)))
           dirs))))
        
      


  projections)
