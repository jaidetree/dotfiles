(require-macros :lib.macros)
(require-macros :lib.advice.macros)

(hs.console.clearConsole)

(hs.loadSpoon "SpoonInstall")


(local hyper (require :lib.hyper))
(local zoom (require :zoom))
(local tmux (require :tmux))
(local idle (require :idle))
(local config (require :config))
(local url-handlers (require :url-handlers))
(global guides (require :guides))


(require :console)

(var timer nil)

(fn set-timer
  []
  (set timer (hs.timer.doAfter 3 zoom.mute-audio)))

(fn clear-timer
  []
  (when timer
    (: timer :stop)))

(fn reset-timer
  []
  (clear-timer)
  (set-timer))

;; Setup push to talk

(hyper.bind-spec
  {:key :s
   :press (fn []
            (reset-timer)
            (zoom.unmute-audio))
   :release (fn []
              (zoom.mute-audio)
              (reset-timer))
   :repeat reset-timer})


;; (global idle-state (idle.init config))

;; (global repl (require :repl))

;; (local coroutine (require :coroutine))
;; (global replserver (repl.start))
;; (repl.run replserver)

;; (print (hs.inspect replserver))

(local state {:last-song nil})
(local menuitem (hs.menubar.new))

(fn loop
  [get-song-fn callback-fn]
  (let [song (get-song-fn)]
  (when (not (= state.last-song song))
    (when song
      (callback-fn song))
    (tset state :last-song song)))
	(hs.timer.doAfter 1 #(loop get-song-fn callback-fn)))

(fn get-track
  []
  (when-let [player (if (hs.spotify.isPlaying) hs.spotify
                        (hs.itunes.isPlaying) hs.itunes
                        nil)]
            (let [track (player.getCurrentTrack)
                  album (player.getCurrentAlbum)
                  artist (player.getCurrentArtist)]
              (.. track " - " album " - " artist))))

(loop get-track #(menuitem:setTitle $))
