(require-macros :lib.macros)
(local windows (require :windows))
(local {:concat concat
        :filter filter
        :logf logf
        :map map
        :range range} (require :lib.functional))
(local vim (require :vim))

(local {:activate-modal activate-modal} (require :lib.modal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn allowed-app?
  [window]
  (if (: window :isStandard)
      true
      false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn activator
  [app-name]
  "
  A higher order function to activate a target app. It's useful for quickly
  binding a modal menu action or hotkey action to launch or focus on an app.
  Takes a string application name
  Returns a function to activate that app.

  Example:
  (local launch-emacs (activator \"Emacs\"))
  (launch-emacs)
  "
  (fn activate []
    (windows.activate-app app-name)))

(fn toggle-console
  []
  "
  A simple action function to toggle the hammer spoon console.
  Change the keybinding in the common keys section of this config file.
  "
  (if-let [console (hs.console.hswindow)]
          (hs.closeConsole)
          (hs.openConsole)))

(fn jump []
  (let [wns (->> (hs.window.allWindows)
                 (filter allowed-app?))]
    (hs.hints.windowHints wns nil true)))

(fn center-mouse
  []
  (let [screen (hs.screen.primaryScreen)
        frame (: screen :frame)
        w (/ frame._w 2)
        h (/ frame._h 2)]
    (hs.mouse.setAbsolutePosition {:x w :y h})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local music-app
       "Spotify")

(local return
       {:key :space
        :title "Back"
        :action :previous})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local window-jumps
       [{:mods [:cmd]
         :key "hjkl"
         :title "Jump"}
        {:mods [:cmd]
         :key :h
         :action "windows:jump-window-left"
         :repeatable true}
        {:mods [:cmd]
         :key :j
         :action "windows:jump-window-above"
         :repeatable true}
        {:mods [:cmd]
         :key :k
         :action "windows:jump-window-below"
         :repeatable true}
        {:mods [:cmd]
         :key :l
         :action "windows:jump-window-right"
         :repeatable true}])

(local window-layouts
       [{:key :f
         :title "Full-screen"
         :action "layouts:full-size"
         :repeatable true}
        {:key :h
         :title "Left Half"
         :action "layouts:left-half"
         :repeatable true}
        {:key :l
         :title "Right Half"
         :action "layouts:right-half"
         :repeatable true}
        {:mods [:shift]
         :key :h
         :title "Left Big"
         :action "layouts:left-big"
         :repeatable true}
        {:mods [:shift]
         :key :l
         :title "Right Small"
         :action "layouts:right-small"
         :repeatable true}])

(local window-move-screens
       [{:key "n, p"
         :title "Move next\\previous screen"}
        {:mods [:shift]
         :key "n, p"
         :title "Move up\\down screens"}
        {:key :n
         :action "windows:move-south"
         :repeatable true}
        {:key :p
         :action "windows:move-north"
         :repeatable true}
        {:mods [:shift]
         :key :n
         :action "windows:move-west"
         :repeatable true}
        {:mods [:shift]
         :key :p
         :action "windows:move-east"
         :repeatable true}])

(local window-items
       (concat
        [return
         {:key :w
          :title "Last Window"
          :action "windows:jump-to-last-window"}]
        window-layouts
        window-jumps
        window-move-screens
        [{:key :m
          :title "Maximize"
          :action "windows:maximize-window-frame"}
         {:key :c
          :title "Center"
          :action "windows:center-window-frame"}
         {:key :g
          :title "Grid"
          :action "windows:show-grid"}
         {:key :u
          :title "Undo"
          :action "windows:undo-action"}
         {:key :s
          :title "Move to Screen"
          :enter (fn [] (print "Entered move to screen"))
          :exit (fn [] (print "Exited move to screen"))
          :items [return
                  {:key :s
                   :title "Whatever"
                   :action (fn [] nil)}]}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apps Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local app-items
       [return
        {:key :e
         :title "Emacs"
         :action (activator "Emacs")}
        {:key :g
         :title "Chrome"
         :action (activator "Google Chrome")}
        {:key :f
         :title "Firefox"
         :action (activator "Firefox")}
        {:key :i
         :title "iTerm"
         :action (activator "iTerm2")}
        {:key :s
         :title "Slack"
         :action "slack:quick-switcher"}
        {:key :b
         :title "Brave"
         :action (activator "Brave")}
        {:key :m
         :title music-app
         :action (activator music-app)}])

(local media-items
       [return
        {:key :s
         :title "Play or Pause"
         :action hs.spotify.playpause}
        {:key :h
         :title "Prev Track"
         :action hs.spotify.previous}
        {:key :l
         :title "Next Track"
         :action hs.spotify.next}
        {:key :j
         :title "Volume Down"
         :action "multimedia:volume-down"
         :repeatable true}
        {:key :k
         :title "Volume Up"
         :action "multimedia:volume-up"
         :repeatable true}
        {:key :a
         :title (.. "Launch " music-app)
         :action (activator music-app)}])

(local zoom-items
       [return
        {:key :m
         :title "Start meeting"
         :action "zoom:start-meeting"}
        {:key :a
         :title "Mute or Unmute Audio"
         :action "zoom:mute-or-unmute-audio"}
        {:key :v
         :title "Start or Stop Video"
         :action "zoom:start-or-stop-video"}
        {:key :s
         :title "Start or Stop Sharing"
         :action "zoom:start-or-stop-sharing"}
        {:key :f
         :title "Pause or Resume Sharing"
         :action "zoom:pause-or-resume-sharing"}
        {:key :i
         :title "Invite..."
         :action "zoom:invite"}
        {:key :l
         :title "End Meeting"
         :action "zoom:end-meeting"}])

(local shadow-items
       [return
        {:key :s
         :title "Toggle shadow shutdown blocker"
         :action "shadow:keep-awake"}])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Menu & Global Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local menu-items
       [{:key :space
         :title "Alfred"
         :action (activator "Alfred 4")}
        {:key :w
         :title "Window"
         :enter "windows:enter-window-menu"
         :exit "windows:exit-window-menu"
         :items window-items}
        {:key :a
         :title "Apps"
         :items app-items}
        {:key :j
         :title "Jump"
         :action jump}
        {:key :m
         :title "Media"
         ;; :enter (fn [menu]
         ;;          (print "Entering menu: " (hs.inspect menu)))
         ;; :exit (fn [menu]
         ;;         (print "Exiting menu: " (hs.inspect menu)))
         :items media-items}
        {:key :s
         :title "Shadow"
         :items shadow-items}
        {:key :z
         :title "Zoom"
         :items zoom-items}])

(local common-keys
       [{:mods [:cmd]
         :key :space
         :action "lib.modal:activate-modal"}
        {:mods [:cmd :ctrl]
         :key :space}
        {:mods [:hyper]
         :key :a
         :action "zoom:mute-or-unmute-audio"}
        {:mods [:hyper]
         :key :1
         :action "layouts:full-size"}
        {:mods [:hyper]
         :key :2
         :action "layouts:left-half"}
        {:mods [:hyper]
         :key :3
         :action "layouts:right-half"}
        {:mods [:hyper]
         :key :4
         :action "layouts:left-big"}
        {:mods [:hyper]
         :key :5
         :action "layouts:right-small"}
        {:mods [:hyper]
         :key :v
         :action "vim:enable"}
        {:mods [:hyper]
         :key :f
         :action (fn []
                   (hs.eventtap.keyStroke nil :f19 200000 (hs.application.find "OBS")))}
        {:mods [:hyper]
         :key :e
         :action (fn []
                   (hs.eventtap.keyStroke nil :f16 200000 (hs.application.find "OBS")))}
        {:mods [:hyper]
         :key :r
         :action hs.reload}
        {:mods [:hyper]
         :key :p
         :action (fn []
                   (hs.eventtap.keyStroke [:cmd :ctrl :shift :alt] :c))}
        {:mods [:ctrl]
         :key "["
         :action (fn []
                   (hs.eventtap.keyStroke nil :escape))}
        {:mods [:cmd]
         :key :i
         :action (fn []
                   (let [el (hs.uielement.focusedElement)
                         role (when (and el el.role) (: el :role))
                         title (when (and el el.title) (: el :title))]
                     (print (hs.inspect {:element el
                                         :role role
                                         :title title}))))}
        {:mods [:ctrl :shift]
         :key "`"
         :action toggle-console}
        {:mods [:hyper]
         :key :m
         :action center-mouse}
        {:mods [:hyper]
         :key :t
         :action "tmux:send-to-tmux"}
        {:mods [:hyper]
         :key :c
         :action "tmux:send-to-tmux-repl"}
        {:mods [:hyper]
         :key :s
         :action "tmux:send-to-tmux-new-session"}])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App Specific Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local browser-keys
       [{:mods [:cmd :shift]
         :key :l
         :action "chrome:open-location"}
        {:mods [:cmd]
         :key :k
         :action "chrome:prev-tab"
         :repeat true}
        {:mods [:cmd]
         :key :j
         :action "chrome:next-tab"
         :repeat true}])

(local brave-config
       {:key "Brave Browser"
        :keys browser-keys})

(local chrome-config
       {:key "Google Chrome"
        :keys browser-keys})

(local emacs-config
       {:key "Emacs"
        ;; :activate (fn []
        ;;             (print "Activating Emacs"))
        ;; :deactivate (fn []
        ;;               (print "Deactivating Emacs"))
        :activate "vim:disable"
        :deactivate "vim:enable"
        :items []
        :keys [{:key :y
                :mods [:cmd]
                :action (fn []
                          (alert "Hi Emacs"))}]})

(local grammarly-config
       {:key "Grammarly"
        :launch (fn [])
        :items (concat
                menu-items
                [{:mods [:ctrl]
                  :key :c
                  :title "Return to Emacs"
                  :action "grammarly:back-to-emacs"}])
        :keys ""})

(local hammerspoon-config
       {:key "Hammerspoon"
        ;; :enter (fn []
        ;;          (print "Entering Hammerspoon :D"))
        ;; :exit (fn []
        ;;         (print "Exiting Hammerspoon T_T"))
        ;; :activate (fn []
        ;;             (print "Activating Hammerspoon"))
        ;; :deactivate (fn []
        ;;             (print "Deactivating Hammerspoon"))
        :items (concat
                menu-items
                [{:key :r
                  :title "Reload Console"
                  :action hs.reload}
                 {:key :c
                  :title "Clear Console"
                  :action hs.console.clearConsole}])
        :keys [{:mods [:cmd]
                :key :y
                :action (fn []
                          (alert "Hi Hammerspoon"))}]})

(local slack-config
       {:key "Slack"
        :keys [{:mods [:cmd]
                :key  :g
                :action "slack:scroll-to-bottom"}
               {:mods [:ctrl]
                :key :r
                :action "slack:add-reaction"}
               {:mods [:ctrl]
                :key :h
                :action "slack:prev-element"}
               {:mods [:ctrl]
                :key :l
                :action "slack:next-element"}
               {:mods [:ctrl]
                :key :t
                :action "slack:thread"}
               {:mods [:ctrl]
                :key :p
                :action "slack:prev-day"}
               {:mods [:ctrl]
                :key :n
                :action "slack:next-day"}
               {:mods [:ctrl]
                :key :i
                :action "slack:next-history"
                :repeat true}
               {:mods [:alt]
                :key :k
                :action "slack:scroll-up"
                :repeat true}
               {:mods [:alt]
                :key :j
                :action "slack:scroll-down"
                :repeat true}
               {:mods [:ctrl]
                :key :o
                :action "slack:prev-history"
                :repeat true}
               {:mods [:ctrl]
                :key :j
                :action "slack:down"
                :repeat true}
               {:mods [:ctrl]
                :key :k
                :action "slack:up"
                :repeat true}]})

(local apps
       [brave-config
        chrome-config
        emacs-config
        grammarly-config
        hammerspoon-config
        slack-config])

(fn shadow-running?
  []
  (hs.application.get "com.blade.shadow-macos"))

(local config
       {:title "Main Menu"
        :items menu-items
        :keys common-keys
        :apps apps
        :grid {:margins [5 5]
               :size "8x2"}
        ;; :hyper {:key :F18}
        :hyper {:mods []
                :key :F18}
        :idle {:tasks [{:delay 1800
                        :idle (fn [state]
                                (let [speaker (hs.audiodevice.defaultOutputDevice)
                                      pre-muted (: speaker :muted)]
                                  (when (not pre-muted)
                                    (: speaker :setMuted true))
                                  (if pre-muted
                                      {:pre-muted true}
                                      {:pre-muted false})))
                        :active (fn [state]
                                  (let [speaker (hs.audiodevice.defaultOutputDevice)
                                        pre-muted state.pre-muted]
                                    (when (not pre-muted)
                                      (: speaker :setMuted false))))}
                       {:delay 1800
                        :idle (fn [state]
                                (let [is-playing (hs.spotify.isPlaying)]
                                  (when is-playing
                                    (hs.spotify.pause))
                                  (if is-playing
                                      {:was-playing true}
                                      {:was-playing false})))
                        :active (fn [state]
                                  (when state.was-playing
                                    (hs.spotify.play)))}]
               :unless shadow-running?}
        :vim {:enabled false}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup a nREPL server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global repl (require :repl))
(local coroutine (require :coroutine))
(global replserver (repl.start))
(repl.run replserver)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

config
