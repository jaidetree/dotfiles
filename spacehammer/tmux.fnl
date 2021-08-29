(require-macros :lib.macros)
(local {: compose
        : filter
        : identity
        : logf
        : map
        : merge
        : reduce
        : split} (require :lib.functional))
(local atom (require :lib.atom))

(local session (atom.new nil))
(local prev-window (atom.new nil))

(fn sleep
  [s]
  (let [start (os.clock)]
    (while (<= (- (os.clock) start) s))))

(fn tmux-bin
  []
  "/usr/local/bin/tmux")

(fn focused-selected-text
  []
  "
  Strategy to get selected text:

  Get the selected text from the current OS X ui element
  Does not work in browser or Electron apps
  "
  (when-let [el (hs.uielement.focusedElement)]
            (: el :selectedText)))

(fn clipboard-selected-text
  []
  "
  Strategy to get selected text:

  Invoke the copy-to-clipboard keystroke, get the contents, compare
  the count, and if the count changed return the clipboard text.

  Before the selected text is returned it restores the previous value
  to the clipboard.
  "
  ;; Capture current clipboard contents
  (let [prev (hs.pasteboard.getContents)
        prev-count (hs.pasteboard.changeCount)]
    ;; invoke the copy key combo
    (hs.eventtap.keyStroke [:cmd] :c)
    ;; Get the new pasteboard contents
    (let [next (hs.pasteboard.getContents)
          next-count (hs.pasteboard.changeCount)]
      ;; If it changed, we have selected text!
      (when (not (= prev-count next-count))
        ;; Restore the clipboard contents
        (hs.pasteboard.setContents prev)
        ;; Return the captured text
        next))))

(fn selected-text
  []
  "
  Iterate through multiple strategies for retrieving the selected text
  "
  (reduce
   (fn [prev next-strat]
     (if prev
         prev
         (next-strat)))
   nil
   [
    focused-selected-text
    clipboard-selected-text]))

(fn cmd
  [cmd-str f args]
  (let [task (hs.task.new cmd-str
                          f
                          (or args []))]
    (task:start)))

(fn tmux-cmd
  [f args]
  (cmd (tmux-bin) f args))

(fn parse-session
  [session-str i]
  "
  Receives a line of text separated by :|: to separate out the
  components expects something like the following:

  session_name:|:window_name:|:session_name:window_index.pane_index:|:pane_title

  Returns a table like the following:

  {:session string
   :window  string
   :target  string
   :title   string}
  "
  (let [parts (split ":|:" session-str)
        [session window target title] parts]
    {:id i
     : session
     : window
     : target
     : title}))

(fn parse-sessions
  [exit-code stdout stderr]
  (->> stdout
       (split "\n")
       (filter #(not (= $1 "")))
       (map parse-session)
       ))

(fn session-selector
  [f]
  (fn [choice]
    (when choice
      (when-let [session-name choice.uuid]
                (atom.reset! session session-name)
                (f session-name)))))

(fn choose-session
  [sessions f]
  (let [choices (->> sessions
                     (map (fn [session i]
                            {:text (string.format "%s > %s (-t %s)\n%s" session.session session.window session.target session.title)
                             :uuid session.target})))
        chooser (hs.chooser.new (session-selector f))]
    (chooser:choices choices)
    (chooser:show)))

(fn tmux-sessions
  [f]
  (tmux-cmd
   #(-> (parse-sessions $1 $2 $3)
        (choose-session f))
   ["list-panes" "-a" "-F" "#{session_name}:|:#W:|:#{session_name}:#I.#P:|:#{pane_title}"]))

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
    (tmux-cmd
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

(fn commit-command
  [continue]
  (fn [msg]
    (tmux-cmd
     #(continue msg)
     ["send-keys" "-t" msg.session "Enter"])))

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
          term-window (-?> "iTerm"
                          (hs.application.find)
                          (: :focusedWindow))]
        (if (and (= screens 1) term-window)
            (do
              (atom.reset! prev-window front-window)
              (term-window:focus))
            (> screens 1)
            (do
              (alert "Selected text sent to tmux"))))
    msg))

(fn confirmation-alert
  [continue]
  (fn [msg]
    (alert "Selected text sent to tmux")
    (continue msg)))

(local pipeline ((compose
                  select-tmux-session
                  save-tmp-file
                  send-file-to-tmux
                  commit-command
                  delete-tmp-file
                  confirmation-alert
                  ;; activate-terminal
                  )
                 pprint))

(fn send-to-tmux
  [custom-msg]
  (let [window (atom.deref prev-window)
        selected (selected-text)
        msg (merge
             {:text selected
              :session (atom.deref session)}
             custom-msg)]
    (if (and (not msg.text) window)
        (do
          (window:focus)
          (atom.reset! prev-window nil))
        (when msg.text
          (pipeline msg)))))

(local app-fns
       {"Emacs"
        (fn [send app]
          ;; (: (hs.eventtap.event.newKeyEvent [] :v true) :post)
          (hs.eventtap.keyStroke [:alt] :h app)
          ;; (hs.timer.usleep 100)
          ;; (hs.eventtap.keyStroke [] "a" app)
          ;; (hs.timer.usleep 100)
          ;; (hs.eventtap.keyStroke [] "p" app)
          (let [text (selected-text)]
            (when (not (= text ""))
              (send {:text (string.gsub text "\n" "")})
              (hs.eventtap.keyStroke [] :escape)
              (hs.eventtap.keyStroke [] :j)))
          )})

(fn prepare-selection
  [send]
  (when-let [app (: (hs.application.frontmostApplication) :name)]
            (when-let [app-fn (. app-fns app)]
                      (app-fn send app))))


(fn send-to-tmux-repl
  []
  (prepare-selection send-to-tmux))

{: send-to-tmux
 : send-to-tmux-repl
 :send-to-tmux-new-session #(send-to-tmux {:session false})}
