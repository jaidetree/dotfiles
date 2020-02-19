(require-macros :lib.macros)
(local {:compose compose
        :logf logf
        :map map
        :merge merge
        :split split} (require :lib.functional))
(local atom (require :lib.atom))

(local session (atom.new nil))

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

(fn select-session
  [choice]
  (atom.reset! session choice.text))

(fn choose-session
  [sessions]
  (let [choices (->> sessions
                     (map (fn [session i]
                            {:text session
                             :uuid i})))
        chooser (hs.chooser.new #(when $ (select-session $)))]
    (print (hs.inspect choices))
    (chooser:choices choices)
    (chooser:show)))

(fn tmux-sessions
  []
  (cmd "/usr/local/bin/tmux"
       #(-> (parse-sessions $1 $2 $3)
            (choose-session))
       ["list-sessions"]))

(tmux-sessions)

(fn load-tmux-buffer
  [f msg]
  (let [{:path path
         :session session} msg]
    (cmd "/usr/local/bin/tmux"
     #(do (print $1 $2 $3)
          (f))
     ["load-buffer" path ";"
      "paste-buffer" "-t" session])))

(fn save-tmp-file
  [continue]
  (fn [msg]
    (let [{:text text} msg
          path (os.tmpname)
          file (io.open path "w")]
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
    (let [{:file file} msg]
      (continue msg))))

(local pipeline ((compose
                  save-tmp-file
                  send-file-to-tmux
                  delete-tmp-file)
                 print))

(fn send-to-tmux
  []
  (let [msg {:text (selected-text)
             :session (atom.deref session)}]
    (print "Sending text to terminal\n" msg.text)
    (when msg.text
      (pipeline msg))))

(fn log-selected-text
  []
  (let [text (selected-text)]
    (print "Selected text:\n" text)))

{:log-selected-text log-selected-text
 :send-to-tmux send-to-tmux}
