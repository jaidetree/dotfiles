
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

(fn create->move
  [state actions extra]
  {:state {:current-state state.current-state
           :context       state.context}
   :effect :move-guide})

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

(fn move->move
  [state actions extra]
  {:state {:current-state state.current-state
           :context       state.context}
   :effect :move-guide})

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

(local machine
       {:state {:current-state :ready
                :context {:x 0
                          :y 0
                          :guide {}
                          :guides []}}
        :effect nil
        :states {:ready    {:edit              ready->edit}
                 :edit     {:escape            edit->done
                            :mouse-down-create edit->create
                            :moue-down-guide   edit->move-guide}
                 :create   {:mouse-move        create->move
                            :mouse-up          create->done
                            :escape            create->cancel}
                 :move     {:mouse-move        move->move
                            :mouse-up          move->done
                            :escape            move->cancel}}})

(local fsm (statemachine.new machine))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn edit
  [state extra]
  (fn []
    nil))

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


(fn api.activate
  []
  (fsm.send :activate))

(tset api :unsubscribe (fsm.subscribe effect-handler))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

api
