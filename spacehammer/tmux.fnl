(require-macros :lib.macros)
(local {:compose compose
        :logf logf
        :map map
        :merge merge
        :split split} (require :lib.functional))
(local atom (require :lib.atom))

(local session (atom.new nil))
(local prev-window (atom.new nil))

(fn selected-text
  []
  (when-let [el (hs.uielement.focusedElement)]
    (: el :selectedText)))

(fn cmd
  [cmd-str f args]
  (let [task (hs.task.new cmd-str
                          f
                          (or args []))]
    (task:start)))

(fn parse-sessions
  [exit-code stdout stderr]
  (->> stdout
       (split "\n")
       (map #(. (split ":" $1) 1))))

(fn session-selector
  [f]
  (fn [choice]
    (when-let [session-name choice.text]
      (atom.reset! session session-name)
      (f session-name))))

(fn choose-session
  [sessions f]
  (let [choices (->> sessions
                     (map (fn [session i]
                            {:text session
                             :uuid i})))
        chooser (hs.chooser.new (session-selector f))]
    (chooser:choices choices)
    (chooser:show)))

(fn tmux-sessions
  [f]
  (cmd "/usr/local/bin/tmux"
       #(-> (parse-sessions $1 $2 $3)
            (choose-session f))
       ["list-sessions"]))

(fn select-tmux-session
  [continue]
  (fn [msg]
    (if msg.session
        (continue msg)
        (tmux-sessions #(continue (merge msg {:session $}))))))

(fn load-tmux-buffer
  [f msg]
  (let [{:path path
         :session session} msg]
    (cmd "/usr/local/bin/tmux"
         #(f)
         ["load-buffer" path ";"
          "paste-buffer" "-dpr" "-t" session ";"])))

(fn save-tmp-file
  [continue]
  (fn [msg]
    (let [{:text text} msg
          path (os.tmpname)
          file (io.open path "w+")]
      (print "Writing text to " path)
      (file:write text)
      (file:close)
      (continue (merge msg {:path path
                            :file file})))))

(fn send-file-to-tmux
  [continue]
  (fn [msg]
    (load-tmux-buffer #(continue msg) msg)))

(fn delete-tmp-file
  [continue]
  (fn [msg]
    (let [{:path path} msg]
      (os.remove path)
      (continue msg))))

(fn activate-terminal
  [continue]
  (fn [msg]
    (let [screens (length (hs.screen.allScreens))
          front-window (hs.window.frontmostWindow)
          term-window (-> "iTerm"
                          (hs.application.find)
                          (: :focusedWindow))]
        (if (and (= screens 1) term-window)
            (do
              (atom.reset! prev-window front-window)
              (term-window:focus))
            (> screens 1)
            (do
              (alert "Send selected text to tmux"))))
    msg))

(local pipeline ((compose
                  select-tmux-session
                  save-tmp-file
                  send-file-to-tmux
                  delete-tmp-file
                  activate-terminal)
                 print))

(fn send-to-tmux
  [custom-msg]
  (let [window (atom.deref prev-window)
        msg (merge
             {:text (selected-text)
              :session (atom.deref session)}
             custom-msg)]
    (if (and (not msg.text) window)
        (do
          (window:focus)
          (atom.reset! prev-window nil))
        (when msg.text
          (pipeline msg)))))

{:send-to-tmux send-to-tmux
 :send-to-tmux-new-session #(send-to-tmux {:session false})}
