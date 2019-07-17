(local hyper (require :lib.hyper))
(local zoom (require :zoom))

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
