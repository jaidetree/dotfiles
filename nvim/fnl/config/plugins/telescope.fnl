(local ts (require :telescope))
(local actions (require :telescope.actions))
(local sa (require :telescope.actions.state))
(local fba (. ts :extensions :file_browser :actions))

(fn magic-tab [prmpt-bufnr]
  (let [fba (. ts :extensions :file_browser :actions)
        prompt (sa.get_current_line)
        base (+ actions.toggle_selection actions.move_selection_worse)]
    (if (or (= prompt "..") (= prompt "../")) (fba.goto_parent_dir prmpt-bufnr)
        (or (= prompt "~") (= prompt "~/")) (fba.goto_home_dir prmpt-bufnr)
        (base prmpt-bufnr))))

(ts.setup {:defaults {:theme :ivy
                      :mappings {:i {:<C-g> actions.close :<Esc> actions.close}
                                 :n {:<C-g> actions.close}}}
           :pickers {:find_files {:theme :ivy} :git_files {:theme :ivy}}
           :extensions {:file_browser {:theme :ivy
                                       :grouped true
                                       :select_buffer true
                                       :hijack_netrw false
                                       :cwd_to_path true
                                       :path "%:p:h"
                                       :mappings {:i {:^ fba.goto_parent_dir
                                                      "~" fba.goto_home_dir
                                                      :<Tab> magic-tab}}}}})

(ts.load_extension :file_browser)
(ts.load_extension :projects)
(ts.load_extension :yank_history)
(ts.load_extension :notify)
