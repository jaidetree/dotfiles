(require-macros :lib.macros)
(local log (hs.logger.new "zoom.fnl" "debug"))

(fn get-zoom
  []
  (hs.appfinder.appFromName "zoom.us"))

(fn mute-audio
  [zoom]
  (let [zoom (or zoom (get-zoom))
        item (: zoom :findMenuItem ["Meeting" "Mute Audio"])]
    (when item
      (: zoom :selectMenuItem ["Meeting" "Mute Audio"]))))

(fn unmute-audio
  [zoom]
  (let [zoom (or zoom (get-zoom))
        item (: zoom :findMenuItem ["Meeting" "Unmute Audio"])]
    (when item
      (: zoom :selectMenuItem ["Meeting" "Unmute Audio"]))))

(fn mute-or-unmute-audio
  []
  (log.i "Mute or Unmute Zoom")
  (when-let [zoom (get-zoom)]
            (if (: zoom :findMenuItem ["Meeting" "Mute Audio"])
                (do (mute-audio zoom)
                    (log.i "Mute Audio"))
                (do (unmute-audio zoom)
                    (log.i "Unmute Audio")))))

(fn start-or-stop-video
  []
  (when-let [zoom (get-zoom)]
            (if (: zoom :findMenuItem ["Meeting" "Start Video"])
                (: zoom :selectMenuItem ["Meeting" "Start Video"])
                (: zoom :selectMenuItem ["Meeting" "Stop Video"]))))

(fn start-or-stop-sharing
  []
  (when-let [zoom (get-zoom)]
            (if (: zoom :findMenuItem ["Meeting" "Start Share"])
                (: zoom :selectMenuItem ["Meeting" "Start Share"])
                (: zoom :selectMenuItem ["Meeting" "Stop Share"]))))

(fn pause-or-resume-sharing
  []
  (when-let [zoom (get-zoom)]
            (if (: zoom :findMenuItem ["Meeting" "Pause Share"])
                (: zoom :selectMenuItem ["Meeting" "Pause Share"])
                (: zoom :selectMenuItem ["Meeting" "Resume Share"]))))

(fn invite
  []
  (when-let [zoom (get-zoom)]
            (if (: zoom :findMenuItem ["Meeting" "Invite"])
                (: zoom :selectMenuItem ["Meeting" "Invite"]))))


(fn end-meeting
  []
  (when-let [zoom (get-zoom)]
            (when (: zoom :findMenuItem ["Meeting" "Exit Minimal View"])
              (: zoom :selectMenuItem ["Meeting" "Exit Minimal View"]))
            (when (: zoom :findMenuItem ["Meeting" "Stop Share"])
              (: zoom :selectMenuItem ["Meeting" "Stop Share"]))
            (let [win  (: zoom :findWindow "Zoom Meeting ID: .*")]
              (: win :close))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:mute-audio mute-audio
 :unmute-audio unmute-audio
 :mute-or-unmute-audio mute-or-unmute-audio
 :start-or-stop-video start-or-stop-video
 :start-or-stop-sharing start-or-stop-sharing
 :pause-or-resume-sharing pause-or-resume-sharing
 :invite invite
 :end-meeting end-meeting}
