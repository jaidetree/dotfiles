(local atom (require :lib.atom))
(local statemachine (require :lib.statemachine))
(local {:eq? eq?
        :map map
        :merge merge} (require :lib.functional))
(local {:timeout timeout} (require :lib.utils))

(local types hs.eventtap.event.types)
(local fsms (atom.new []))

(fn run-task?
  [idle-config]
  "
  Determines if a task's :idle or :active function should be called.
  Uses global :when and :unless predicates. All defined predicates must
  return true for task methods to be called.

  Takes :idle config table from config.fnl

  Returns a boolean, true if all defined predicates pass or no predicates were
  defined
  "
  (if
   (= idle-config.unless idle-config.when nil)
   true
   (eq? [(or (not idle-config.when) (and idle-config.when (idle-config.when)))
         (or (not idle-config.unless) (and idle-config.unless
                                           (not (idle-config.unless))))]
        [true true])))

(fn active->idle
  [state data idle-task idle-config fsm]
  "
  Transition when a task's idle timer fires. May call idle-task's :idle
  function if present and the :when and :unless predicates pass.

  Takes fsm state, extra data (nil) from dispatcher, the current task def,
  the :idle config from config.fnl, and the finite-state-machine fsm instance.

  Returns updated task state.
  "
  (merge state
         {:status :idle
          :custom-state (if (and idle-task.idle (run-task? idle-config))
                            (idle-task.idle state.custom-state)
                            state.custom-state)}))

(fn active->active
  [state data idle-task idle-config fsm]
  "
  Triggered when user is already active and clicks, moves the mouse, or presses
  a key. The idle timer is reset.

  Takes fsm state, extra data (nil) from dispatcher, the current task def,
  the :idle config from config.fnl, and the finite-state-machine fsm instance.

  Returns updated task state.
  "
  (when state.kill-timer
    (state.kill-timer))
  (if (run-task? idle-config)
      (merge state
             {:status     :active
              :kill-timer (timeout
                           idle-task.delay
                           (fn []
                             (fsm.dispatch :idled)))})
      (merge state
             {:status :active
              :kill-timer (fn [] nil)})))

(fn idle->active
  [state data idle-task idle-config fsm]
  "
  Transitions between idle and active when the user returns from idle.
  Only performs the active task method if the when or unless predicates pass.

  Takes fsm state, extra data (nil) from dispatcher, the current task def,
  the :idle config from config.fnl, and the finite-state-machine fsm instance.

  Returns updated task state.
  "
  (merge state
         {:status :active
          :custom-state (if (and idle-task.active (run-task? idle-config))
                            (idle-task.active state.custom-state)
                            state.custom-state)}))

(fn idle-task->fsm
  [idle-task idle-config]
  "
  Creates a finite-state-machine from an idle task definition table and the
  root idle config which contains the predicates for disabling
  idling tasks from running.

  Takes an idle-task config table like the following:
  {:delay 1800
   :idle (fn [state] state)
   :active (fn [state] state)}

  Returns a finite-state-machine table.
  "
  (var fsm nil)
  (let [initial-state {:status :active
                       :custom-state  {}
                       :kill-timer (fn [] nil)}
        transition (fn transition
                     [f]
                     (fn [state data]
                       (f state data idle-task idle-config fsm)))
        states {:idle {:activate (transition idle->active)}
                :active {:idled (transition active->idle)
                         :activate (transition active->active)}}]
    (set fsm (statemachine.new states initial-state :status))
    fsm))

(fn create-fsms
  [idle-config]
  "
  Takes the :idle config table from config.fnl
  Returns a table list of finite-state-machines (fsm)
  "
  (let [idle-tasks idle-config.tasks]
    (map #(idle-task->fsm $1 idle-config) idle-tasks)))

(fn activate
  []
  "
  Activate API dispatches the activate action against all the task fsms
  "
  (each [_ fsm (ipairs (atom.deref fsms))]
    (fsm.dispatch :activate nil)))

(fn activate-err
  [err]
  "
  Error handler when calling the activate api.
  Takes an error table.
  "
  (print (hs.inspect err)))

(fn user-activity
  [e]
  "
  User has clicked, moved the mouse, or pressed a key on a keyboard.
  Calls the activate API in a fail-safe xpcall, this prevents an error
  messing with clicks and key presses.

  Takes an hs.eventtap.event
  Must return a boolean to block the event and a list of events to post.
  "
  (xpcall activate activate-err)
  (values false []))

(fn init
  [config]
  "
  Initialize the idle module. Allows you to define tasks to perform after x
  seconds of idle. Each task may also define a function to run on return.

  In each task :idle and :active function, they will receive their task state
  and can return their task state.

  Example config.fnl:
  {:idle {:tasks [{:delay 1800
                   :idle (fn [state]
                          (alert \"You have gone idle after 30 min\")
                          state)}
                   :active (fn [state]
                            (alert \"Welcome back\")
                            state)]}}
  "
  (atom.reset! fsms (create-fsms config.idle))
  (let [tap (hs.eventtap.new [types.leftMouseUp
                              types.rightMouseUp
                              types.mouseMoved
                              types.keyDown
                              types.keyUp]
                             user-activity)]
    (: tap :start)
    {:tap tap
     :fsms fsms
     :destroy (fn []
                (: tap :stop)
                (: tap :delete))}))


{:init init}
