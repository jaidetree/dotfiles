(local wezterm (require :wezterm))

(local config (wezterm.config_builder))

(local configdir wezterm.config_dir)

(wezterm.add_to_config_reload_watch_list (.. configdir "/fnl/j/wezterm/config.fnl"))

(tset config :color_scheme :Dracula)

(tset config :default_prog ["/usr/local/bin/fish" "-l"])
(tset config :font (wezterm.font "OperatorMono Nerd Font" {:weight :Bold}))

(wezterm.log_info (.. "ssh_domains " (fennel.view config.ssh_domains)))



config
