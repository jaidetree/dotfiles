
(require-macros :lib.macros)
(require-macros :lib.advice.macros)
(local statemachine (require :lib.statemachine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relevant Docs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples
;; https://github.com/asmagill/hammerspoon/wiki/hs.canvas.examples
;;
;; Listening for mouse events
;; https://www.hammerspoon.org/docs/hs.eventtap.html
;;
;; Possible hs.eventtap.events.types
;;  - appKitDefined = 13,
;;  - applicationDefined = 15,
;;  - changeMode = 38,
;;  - cursorUpdate = 17,
;;  - directTouch = 37,
;;  - flagsChanged = 12,
;;  - gesture = 29,
;;  - keyDown = 10,
;;  - keyUp = 11,
;;  - leftMouseDown = 1,
;;  - leftMouseDragged = 6,
;;  - leftMouseUp = 2,
;;  - magnify = 30,
;;  - mouseEntered = 8,
;;  - mouseExited = 9,
;;  - mouseMoved = 5,
;;  - nullEvent = 0,
;;  - otherMouseDown = 25,
;;  - otherMouseDragged = 27,
;;  - otherMouseUp = 26,
;;  - periodic = 16,
;;  - pressure = 34,
;;  - quickLook = 33,
;;  - rightMouseDown = 3,
;;  - rightMouseDragged = 7,
;;  - rightMouseUp = 4,
;;  - rotate = 18,
;;  - scrollWheel = 22,
;;  - smartMagnify = 32,
;;  - swipe = 31,
;;  - systemDefined = 14,
;;  - tabletPointer = 23,
;;  - tabletProximity = 24
;;
;; Drawing on the canvas
;; https://www.hammerspoon.org/docs/hs.canvas.html


(fn ready->edit
  [state action extra]
  {:state  {:current-state :edit
            :context state.context}
   :effect :edit})

(fn edit->done
  [state action extra]
  {:state  {:current-state :ready
            :context state.context}
   :effect :exit})

(fn edit->create
  [state actions extra]
  {:state  {:current-state :create
            :context state.context}
   :effect :create-guide})

(fn edit->move-guide
  [state actions extra]
  {:state {:current-state :move
           :context state.context}
   :effect :move-guide})

;; May not be needed?
;; If we know when we are creating/moving then should be able to just set the
;; eventtap and move the guide directly on mouse move events that way we're not
;; constantly pushing move events, destroying the prev eventtap, and creating
;; a new one
;; (fn create->move
;;   [state actions extra]
;;   {:state {:current-state state.current-state
;;            :context       state.context}
;;    :effect :move-guide})

(fn create->done
  [state actions extra]
  {:state {:current-state :edit
           :context state.current-state}
   :effect :complete-guide})

(fn create->cancel
  [state actions extra]
  {:state {:current-state :edit
           :context state.context}
   :effect :remove-guide})

;; May not be needed?
;; (fn move->move
;;   [state actions extra]
;;   {:state {:current-state state.current-state
;;            :context       state.context}
;;    :effect :move-guide})

(fn move->done
  [state actions extra]
  {:state {:current-state :edit
           :context       state.context}
   :effect :complete-guide})

(fn move->cancel
  [state actions extra]
  {:state {:current-state :edit
           :context       state.context}
   :effect :reset-guide})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define State Machine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local machine
       {:state {:current-state :ready
                :context {:x 0
                          :y 0
                          :guide {}
                          :guides []}}
        :effect nil
        :states {:ready    {:edit              ready->edit}
                 :edit     {:escape            edit->done
                            :create            edit->create
                            :move              edit->move-guide}
                 :create   {;:mouse-move        create->move
                            :done              create->done
                            :escape            create->cancel}
                 :move     {;:mouse-move        move->move
                            :done              move->done
                            :escape            move->cancel}}
        :log :guide})

(local fsm (statemachine.new machine))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local events hs.eventtap.event.types)

(fn do-xpc!
  [f]
  (let [(ok err) (xpcall f debug.traceback)]
    (when (not ok)
      (hs.showError err))
    (values ok err)))

(fn eventtap-effect
  [{: events} handler]
  (fn
    [state extra]
    (let [tap (hs.eventtap.new
               events
               (fn [event]
                 (let [(ok err) (do-xpc! #(handler state event extra))]
                   (if ok
                       (values true [])
                       (values false [])))))]
      (tap:start)
      (fn cleanup
        []
        (tap:stop)))))

(local edit
       (eventtap-effect
        {:events [events.leftMouseDown]}
        (fn [state event extra]
          (let [point (event:location)
                is-clicked (event:getButtonState 0)]
            (if (and is-clicked (= point.x 0))
                (do
                  (print "\n\nAdd vertical line\n\n")
                  (fsm.send :create :vertical))
                (do
                  (and is-clicked (= point.y 0))
                  (print "\n\nAdd horizontal line\n\n")
                  (fsm.send :create :horizontal))))
          )))

(fn exit
  [state extra]
  (fn []
    nil))

(fn create-guide
  [state extra]
  (fn []
    nil))

(fn move-guide
  [state extra]
  (fn []
    nil))

(fn remove-guide
  [state extra]
  (fn []
    nil))

(fn reset-guide
  [state extra]
  (fn []
    nil))

(fn complete-guide
  [state extra]
  (fn []
    nil))

(local effect-handler
       (statemachine.effect-handler
        {: edit
         : exit
         : create-guide
         : move-guide
         : remove-guide
         : reset-guide
         : complete-guide}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local api {: fsm})


(fn api.edit
  []
  (fsm.send :edit))

(tset api :unsubscribe (fsm.subscribe effect-handler))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

api
