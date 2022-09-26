;; Exploring the minimum requirements to render a custom statusline
(local M {})

;; Borrowed from https://github.com/feline-nvim/feline.nvim/blob/496975425a28ef1f974e90e9664fe3409738f071/lua/feline/providers/vi_mode.lua#L5
(local mode-alias 
  {:n :NORMAL
   :no :OP
   :nov :OP
   :noV :OP
   "no\022" :OP
   :niI :NORMAL
   :niR :NORMAL
   :niV :NORMAL
   :v :VISUAL
   :vs :VISUAL
   :V :LINES
   :Vs :LINES
   "\022" :BLOCK
   "\022s" :BLOCK
   :s :SELECT
   :S :SELECT
   "\019" :BLOCK
   :i :INSERT
   :c :COMMAND
   :ix :INSERT
   :R :REPLACE
   :Rc :REPLACE
   :Rv :V-REPLACE
   :Rx :REPLACE
   :c :COMMAND
   :cv :COMMAND
   :ce :COMMAND
   :r :ENTER
   :rm :MORE
   :r? :CONFIRM
   :! :SHELL
   :t :TERM
   :nt :TERM
   :null :NONE})

(local mode-colors 
  {:NORMAL  {:bg "#8EE8DA" :fg "#19192a"}
   :INSERT  {:bg "#008a73" :fg "#ffffff"}
   :VISUAL  {:bg "#000000"}
   :OP      {:bg "#000000"}
   :BLOCK   {:bg "#000000"}
   :LINES   {:bg "#000000"}
   :V-LINE  {:bg "#000000"}
   " "      {:bg "#000000"}
   :COMMAND {:bg "#000000"}
   :REPLACE {:bg "#000000"}
   :SELECT  {:bg "#000000"}})

(local icons 
 {:vertical_bar       "┃"
  :vertical_bar_thin  "│"
  :left               ""
  :right              ""
  :block              "█"
  :left_filled        ""
  :right_filled       ""
  :slant_left         ""
  :slant_left_thin    ""
  :slant_right        ""
  :slant_right_thin   ""
  :slant_left_2       ""
  :slant_left_2_thin  ""
  :slant_right_2      ""
  :slant_right_2_thin ""
  :left_rounded       ""
  :left_rounded_thin  ""
  :right_rounded      ""
  :right_rounded_thin ""
  :circle             "●"
  :error              ""
  :warn               ""
  :info               ""
  :hint               ""})


;; Borrowed from 
;; https://github.com/feline-nvim/feline.nvim/blob/496975425a28ef1f974e90e9664fe3409738f071/lua/feline/providers/cursor.lua#L5

;; (local scroll-icons ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"])

(local scroll-icons ["▏" "▎" "▍" "▌" "▋" "▊" "▉" "█"])


(fn get-formatters
  []
  (let [sources (require :null-ls.sources)
        ft vim.bo.filetype
        sources (sources.get_available ft)]
    (accumulate [formatters {} _ source (ipairs sources)]
      (do
        (when (and source.methods.NULL_LS_FORMATTING (not= source.name "codespell"))
          (table.insert formatters source.name))
        formatters))))

(fn count-diagnostics
  [severity]
  (let [count (vim.tbl_count (vim.diagnostic.get 0 (and severity {:severity severity})))]
    count))

(fn diagnostic-errors
  []
  (count-diagnostics vim.diagnostic.severity.ERROR))

(fn diagnostic-warnings
  []
  (count-diagnostics vim.diagnostic.severity.WARN))

(fn diagnostic-info
  []
  (count-diagnostics vim.diagnostic.severity.INFO))

(fn diagnostic-hints
  []
  (count-diagnostics vim.diagnostic.severity.HINT))

(fn active? []
  (= (vim.api.nvim_get_current_win) (tonumber vim.g.actual_curwin)))

(fn get-or-create-hl
  [group hl-name attrs]
  ;; (when (not (pick-values 1 (pcall vim.api.nvim_get_hl_by_name hl-name false)))
  ;;   (let [ns (vim.api.nvim_create_namespace group)]
  ;;     (vim.api.nvim_set_hl ns hl-name attrs)))
  (let [ns (vim.api.nvim_create_namespace group)]
    (vim.api.nvim_set_hl 0 hl-name attrs))
  hl-name)

(fn str 
  [head ...]
  (let [args [...]]
    (accumulate [result (or head "")
                 _ s (ipairs args)]
      (.. result s))))

(fn hl 
  [{: bg : fg} content ...]
  (let [args [...]
        name (str :StatusX (or bg "bg") :X (or fg "fg"))
        hl-name (get-or-create-hl :JStatusLine (string.gsub name "#" "")
                                  {: fg : bg})]
    (str "%#" hl-name "#" content (unpack args))))

(fn rect
  [{: fg : bg} content ...]
  (let [args [...]]
    (..
     (hl 
       {: fg : bg} 
       content (unpack args)))))

(fn slant-left 
  [{: fg : bg : prev-bg} content ...]
  (let [args [...]]
    (..
     (hl {:fg bg :bg prev-bg} icons.slant_left)
     (hl {: fg : bg} content (unpack args)))))

(fn slant-right 
  [{: fg : bg : next-bg} content ...]
  (let [args [...]]
    (..
     (hl 
       {: fg : bg} 
       content (unpack args))
     (hl {:fg bg :bg next-bg} icons.slant_right))))

(fn filled-left
  [{: fg : bg : prev-bg} content ...]
  (let [args [...]]
    (..
      (hl {:fg bg :bg prev-bg} icons.left_filled)
      (hl {: fg : bg} content (unpack args)))))

(fn filled-right
  [{: fg : bg : next-bg} content ...]
  (let [args [...]]
    (..
       (hl {: fg : bg} content (unpack args))
       (hl {:fg bg :bg next-bg} icons.right_filled))))

(fn bubble
  [{: fg : bg : next-bg : prev-bg} content ...]
  (let [args [...]]
    (..
      " "
      (hl {:fg bg :bg prev-bg} icons.left_rounded)
      (hl {: fg : bg} content (unpack args))
      (hl {:fg bg :bg next-bg} icons.right_rounded)
      " ")))

(fn pos?
  [x]
  (if (= x nil) 
    false
    (> x 0)))

(fn vi-mode 
  [{: mode : branch : readonly}]
  (let [color (. mode-colors mode)]
    (slant-right {:bg color.bg :fg color.fg 
                  :next-bg (if branch   "#5b5ba5" 
                               readonly "#303050"
                               "#19192a")} 
                 " " mode " ")))

(fn git-branch
  [{: branch : readonly}]
  (when branch
    (let [changed (. vim.b.gitsigns_status_dict "changed")
          added   (. vim.b.gitsigns_status_dict "added")
          removed (. vim.b.gitsigns_status_dict "removed")
          updated (or (pos? changed) (pos? added) (pos? removed))
          bg "#404080"
          entries []
          insert #(table.insert entries $1)]
      (when updated
        (when (pos? changed) (insert (hl {:fg "#ffb78e" :bg bg} "±" changed)))
        (when (pos? added) (insert (hl {:fg "#a5e8a7" :bg bg} "+" added)))
        (when (pos? removed) (insert (hl {:fg "#e85b00" :bg bg} "−" removed))))
       
      (str
        (filled-right
          {:bg "#5b5ba5" :fg "#ffffff" 
           :next-bg (if 
                      updated bg
                      readonly "#303050" 
                      "#19192a")}
          "  " branch " ")
        (if updated
          (slant-right
            {:bg bg :fg "#ffffff"
             :next-bg (if readonly "#303050")}
            (.. " " (table.concat entries " ") " "))
          " ")))))
 
(fn readonly
  [{: readonly}]
  (when readonly
    (..
      (hl {:fg "#f6deb0" :bg "#303050"} "  ")
      (slant-right 
        {:bg "#303050" :fg "#ffffff" :next-bg "#19192a"}
        "RO "))))

(fn file
  [_state]
  (let [modified vim.bo.modified
        dev-icons (require :nvim-web-devicons) 
        (icon-str  icon-color) (dev-icons.get_icon_color (vim.fn.expand "%:t"))]
    (..
      (hl {:fg icon-color} " " icon-str)
      (hl {:bg "#19192a" :fg (if modified "#ffb78e" "fg")} 
          " %<%f"
          (if modified (.. " " icons.circle) ""))
      " ")))

(fn lsp-diagnostics
  []
  (let [errors (diagnostic-errors)
        warnings (diagnostic-warnings)
        infos    (diagnostic-info)
        hints (diagnostic-hints)
        bg "#303050"
        entries []
        insert #(table.insert entries $1)]
    (when (or (pos? errors) (pos? warnings) (pos? infos) (pos? hints))
      (when (pos? hints)
         (insert 
           (hl 
             {:bg bg :fg "#8ee8da"}
             (string.format "%s %s" icons.hint hints))))
      (when (pos? infos)
         (insert
           (hl
             {:bg bg :fg "#81b1ea"}
             (string.format "%s %s" icons.info infos))))
      (when (pos? warnings)
        (insert
          (hl
            {:bg bg :fg "#fde4b5"}
            (string.format "%s %s" icons.warn warnings))))
      (when (pos? errors)
        (insert
          (hl
            {:bg bg :fg "#e10014"}
            (string.format "%s %s" icons.error errors)))
       (bubble
         {:bg "#303050" :fg "#ffffff"}
         (table.concat entries " "))))))
        

(fn lsp-formatters
  [{:formatters {: list : active : enabled}}]
  (when active
    (let [status-color (if enabled "#a1eaac" "#e10014")]
      (str
        (if enabled
          (slant-left {:fg "#19192a" :bg status-color} "  ")
          (slant-left {:fg "#ffffff" :bg status-color} "  "))
        (slant-left 
                  {:prev-bg status-color :fg "#ffffff" :bg "#303050"}
                  "   "
                  (table.concat list " ")
                  " ")))))

(fn file-type
  [{: file-type : formatters}]
  (let [file-type (.. " " (if (= file-type "") "scratch" file-type) " ")]
    (if formatters.active
     (filled-left 
       {:prev-bg "#303050" :fg "#ffffff" :bg "#c36892"}
       file-type)
     (slant-left 
       {:prev-bg "#19192a" :fg "#ffffff" :bg "#c36892"}
       file-type))))

(fn file-loc
  [_state]
  (filled-left
    {:prev-bg "#c36892" :fg "#ffffff" :bg "#aa4473"}
    " %L lines "
    (hl {:fg "#aa4473" :bg "#19192a"} icons.slant_right_2)))

(fn cursor-pos
  [{: cursor}]
  (let [[line col] cursor]
    (hl
      {:fg "#8ac0fe"}
      " %03l:%02v")))

(fn scrollbar
  [{: cursor}]
  (let [[current _] cursor
        total   (vim.api.nvim_buf_line_count 0)
        icon-index (math.floor (* (/ current total) 8))]
    (..
      (if 
       (= current 1)
       " TOP "
       (= current total)
       " BOT "
       " %p%% ")
       
      (hl
        {:fg "#a4e7a7"}
        (string.rep "▰" icon-index))
      (hl
        {:fg "#444470"}
        (string.rep "▰" (- 8 icon-index))
        " "))))
      
(fn section 
  [state element-fns]
  (accumulate [section-str "" _ element-fn (ipairs element-fns)]
    (let [element (element-fn state)]
      (if (not= element nil)
        (.. section-str element)
        section-str))))

(fn active-statusline 
  []
  (str
    (let [state {:mode (. mode-alias (. (vim.api.nvim_get_mode) :mode))
                 :branch vim.b.gitsigns_head
                 :readonly vim.bo.readonly}]
     (section 
       state
       [vi-mode
        git-branch
        readonly
        file]))
    "%="
    (let [formatters (get-formatters)
          state {:cursor     (vim.api.nvim_win_get_cursor 0)
                 :formatters {:list formatters
                              :active (> (length formatters) 0)
                              :enabled (not vim.b.noformat)}
                 :file-type  vim.bo.filetype}]
      (section 
        state
        [lsp-diagnostics
         lsp-formatters
         file-type
         file-loc
         cursor-pos
         scrollbar])))) 

(fn inactive-statusline 
  []
  (str 
    (section {} [(fn [] "%<%f")]) 
    "%="
    (section {} [(fn [] "%h ")
                 (fn [] (let [ft vim.bo.filetype]
                          (when (not= ft "help")
                            ft)))])))

(fn M.render 
  []
  (if (active?)
      (active-statusline)
      (inactive-statusline)))

(fn M.setup 
  []
  (set vim.o.statusline "%{%v:lua.require'config.statusline'.render()%}"))

(comment ;; dbg
  highlights
  (vim.api.nvim_set_hl (vim.api.nvim_create_namespace :JStatusLine) :Statusxff0000xffffff {:bg "#ff0000" :fg "#ffffff"})
  (vim.api.nvim_get_hl_by_name :Statusxff0000xffffff true)
  (pcall vim.api.nvim_get_hl_by_name :Statusxff0000xffffff true)
  (vim.keymap.del :n "iÞ")
  (hl {:fg "#ffffff" :bg "#000000"} :test " hello " :world)
  (active-statusline)
  (vi-mode)
  (print (unpack [:1 :2 :3]))
  (tonumber vim.g.actual_curwin)
  (tonumber :5)
  (M.render)
  (active?))

M
