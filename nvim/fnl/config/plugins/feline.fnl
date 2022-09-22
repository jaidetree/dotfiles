(local feline (require :feline))
(local defaults (require :feline.defaults))

;; References
;; - https://github.com/EdenEast/nyx/blob/8a9819e/config/.config/nvim/lua/eden/modules/ui/feline/init.lua
;; - https://github.com/EdenEast/nyx/blob/8a9819e4ea11193434b2366b9f1d65ed3a4661f3/config/.config/nvim/lua/eden/modules/ui/feline/util.lua
;; - https://github.com/feline-nvim/feline.nvim/blob/master/USAGE.md#default-providers
;; - https://github.com/feline-nvim/feline.nvim/blob/master/USAGE.md#default-providers
;; - https://user-images.githubusercontent.com/2746374/137549252-333f074e-47a0-464f-ac8a-7ce0ee43433c.png

(local icons defaults.statusline.separators.default_value)

(comment ;; icons
  {:block "█"
   :circle "●"
   :left ""
   :left_filled ""
   :left_rounded ""
   :left_rounded_thin ""
   :right ""
   :right_filled ""
   :right_rounded ""
   :right_rounded_thin ""
   :slant_left ""
   :slant_left_2 ""
   :slant_left_2_thin ""
   :slant_left_thin ""
   :slant_right ""
   :slant_right_2 ""
   :slant_right_2_thin ""
   :slant_right_thin ""
   :vertical_bar "┃"
   :vertical_bar_thin "│"})

(local mode-labels {:n :NORMAL
                    :no :NORMAL
                    :i :INSERT
                    :v :VISUAL
                    :V :V-LINE
                    " " :SELECT
                    :c :COMMAND
                    :cv :COMMAND
                    :ce :COMMAND
                    :R :REPLACE
                    :Rv :REPLACE
                    :s :SELECT
                    :S :SELECT
                    :t :TERMINAL})

(local mode-colors {:NORMAL "#8EE8DA"
                    :INSERT "#008a73"
                    :VISUAL "#000000"
                    :V-LINE "#000000"
                    " " "#000000"
                    :COMMAND "#000000"
                    :REPLACE "#000000"
                    :SELECT "#000000"
                    :TERMINAL "#000000"})

(fn formatting-clients []
  (let [sources (require :null-ls.sources)
        ft vim.bo.filetype
        methods (sources.get_supported ft)]
    (accumulate [formatters {} method names (pairs methods)]
      (doto formatters
        (when (= method :formatting)
          (vim.list_extend formatters names))))))

(fn with-formatters [f]
  (fn []
    (let [formatters (formatting-clients)]
      (f formatters))))

(fn with-format-state [f]
  (fn []
    (let [formatters (formatting-clients)
          disabled vim.b.noformat]
      (f (and (> (length formatters) 0) (not disabled))))))

(comment ;; dbg
  (format-state print)
  (formatting-clients)
  nil)

;; (fn count-diagnostics
;;   [severity]
;;   (let [count (vim.tbl_count (vim.diagnostic.get 0 (and severity {:severity severity})))]
;;     count))
;;
;; (fn diagnostic-errors
;;   []
;;   (values (count-diagnostics vim.diagnostic.severity.ERROR) "  "))
;;
;; (fn diagnostic-warnings
;;   []
;;   (values (count-diagnostics vim.diagnostic.severity.WARN) "  "))
;;
;; (fn diagnostic-info
;;   []
;;   (values (count-diagnostics vim.diagnostic.severity.INFO) "  "))
;;
;; (fn diagnostic-hints
;;   []
;;   (values (count-diagnostics vim.diagnostic.severity.HINT) "  "))

(comment (diagnostic-errors))

;; Status Items

(local c {:vim-mode (let [get-label #(. mode-labels (vim.fn.mode))
                          get-color #(. mode-colors
                                        (. mode-labels (vim.fn.mode)))]
                      {:name :vim-mode
                       :provider #(string.format " %s " (get-label))
                       :hl (fn []
                             {:bg (get-color) :fg "#19192A"})
                       :right_sep {:str icons.slant_right
                                   :hl (fn []
                                         {:fg (get-color)
                                          :bg (if (not= vim.b.gitsigns_status_dict
                                                        nil)
                                                  "#333351" "#19192a")})}})
          :gitbranch {:name :gitbranch
                      :provider #(string.format "%s  "
                                                (feline.providers.git_branch))
                      :icon "   "
                      :hl {:bg "#333351"}
                      :right_sep {:str icons.slant_right
                                  :hl {:fg "#333351" :bg "#19192A"}}
                      :enabled #(not= vim.b.gitsigns_status_dict nil)}
          :file-type {:name :file-type
                      :provider #(let [ft vim.bo.filetype]
                                   (string.format " %s "
                                                  (if (= "" ft) :unknown ft)))
                      :hl {:bg "#C36892" :fg "#ffffff"}
                      :left_sep (with-formatters (fn [formatters]
                                                   (let [has-formatters (> (length formatters)
                                                                           0)]
                                                     {:str (if has-formatters
                                                               icons.left_filled
                                                               icons.slant_left)
                                                      :hl {:fg "#C36892"
                                                           :bg (if (> (length formatters)
                                                                      0)
                                                                   "#3a3b5d"
                                                                   "#19192a")}})))}
          :file-name {:name :file-name
                      :provider #(string.format " %s "
                                                (feline.providers.file_info {:icon ""}
                                                                            {:type :relative}))
                      :hl {:bg "#19192A"}
                      :fg "#ffffff"}
          :default {:name :default
                    :provider ""
                    :hl :StatusLine
                    :right_sep {:str icons.slant_right :hl {:fg :white}}}
          :position {:name :position
                     :provider :position
                     :hl {:bg "#3a3b5d"}
                     :left_sep {:str icons.block :hl {:fg "#3a3b5d"}}}
          :line-percentage {:name :line-percentage
                            :provider :line_percentage
                            :hl {:bg "#3a3b5d"}
                            :left_sep {:str icons.block :hl {:fg "#3a3b5d"}}}
          :scroll-bar {:name :scroll-bar
                       :provider :scroll_bar
                       :hl {:bg "#3a3b5d"}
                       :left_sep {:str icons.block :hl {:fg "#3a3b5d"}}}
          :file-encoding {:name :file-encoding
                          :provider #(string.format " %s "
                                                    (feline.providers.file_encoding))
                          :hl {:bg "#aa4473" :fg "#ffffff"}
                          :left_sep {:str icons.left_filled
                                     :hl {:fg "#aa4473" :bg "#c36892"}}
                          :right_sep {:str icons.slant_right_2
                                      :hl {:fg "#aa4473" :bg "#3a3b5d"}}}
          :lsp-client-names {:name :lsp-client-name
                             :provider #(let [clients (vim.lsp.buf_get_clients)
                                              names (icollect [_ client (ipairs clients)]
                                                      client.config.name)]
                                          (string.format " %s %s " ""
                                                         (table.concat names
                                                                       " ")))
                             :hl {:bg "#3a3b5d"}
                             :left_sep {:str (.. " " icons.left_rounded)
                                        :hl {:fg "#3A3B5D" :bg "#19192a"}}
                             :enabled #(> (length (vim.lsp.buf_get_clients)) 0)}
          :lsp-formatters {:name :lsp-formatters
                           :provider #(let [formatters (formatting-clients)]
                                        (if (> (length formatters) 0)
                                            (string.format " %s "
                                                           (table.concat formatters " "))
                                            " nofmt "))
                           :hl {:bg "#3a3b5d"}
                           :enabled (with-formatters #(> (length $1) 0))}
          :lsp-format-status {:name :lsp-format-status
                              :provider (with-format-state (fn [enabled]
                                                             (if enabled
                                                                 "  " "  ")))
                              :hl (with-format-state #(if $1
                                                          {:bg "#a3e5a6"
                                                           :fg "#19192a"}
                                                          {:bg "#e10014"
                                                           :fg "#ffffff"}))
                              :left_sep (with-format-state (fn [enabled]
                                                             {:str (.. " "
                                                                       icons.slant_left)
                                                              :hl (if enabled
                                                                      {:fg "#a3e5a6"
                                                                       :bg "#19192a"}
                                                                      {:fg "#e10014"
                                                                       :bg "#19192a"})}))
                              :right_sep (with-format-state (fn [enabled]
                                                              {:str icons.slant_right_2
                                                               :hl (if enabled
                                                                       {:fg "#a3e5a6"
                                                                        :bg "#3a3b5d"}
                                                                       {:fg "#e10014"
                                                                        :bg "#3a3b5d"})}))
                              :enabled (with-formatters #(> (length $1) 0))}
          :lsp-errors {:name :lsp-errors
                       :provider :diagnostic_errors
                       :hl {:bg "#19192a" :fg "#e10014"}}
          :lsp-warnings {:name :lsp-warnings
                         :provider :diagnostic_warnings
                         :icon " "
                         :hl {:bg "#19192a" :fg "#e49e00"}}
          :lsp-info {:name :lsp-info
                     :provider :diagnostic_info
                     :hl {:bg "#19192a" :fg "#8ac0fe"}}
          :lsp-hints {:name :lsp-hints
                      :provider :diagnostic_hints
                      :hl {:bg "#19192a" :fg "#d5affb"}}})

(local components {:active [[c.vim-mode c.gitbranch c.file-name c.default]
                            [c.lsp-hints
                             c.lsp-info
                             c.lsp-warnings
                             c.lsp-errors
                             c.lsp-format-status
                             c.lsp-formatters
                             c.file-type
                             c.file-encoding
                             c.position
                             c.line-percentage
                             c.scroll-bar]]
                   :inactive [[c.file-name]
                              [c.lsp-format-status
                               c.lsp-formatters
                               c.file-type]]})

(feline.setup {: components})

(fn reload []
  (each [module-name v (pairs package.loaded)]
    (when (string.find module-name :^feline)
      (tset package.loaded module-name nil)))
  (let [feline (require :feline)]
    (feline.setup {: components})))

(comment ;; Dev
  (feline.reset_highlights)
  (reload)
  (file-name)
  (set package.loaded.feline nil)
  (fennel.view (vim.opt.guifont:get)) ; "[\"OperatorMono Nerd Font\"]"
  (vim.fn.mode)
  (fennel.view components)
  (. components :active)
  nil)
