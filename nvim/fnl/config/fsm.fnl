(local p (require :config.validators))
(local {:core c} (require :config.utils))
(local spec-tag {:type :fsm-spec})
(local fsm-tag {:type :fsm-instance})

(local state? (p.map {:is p.str?}))
(local action? (p.map {:type p.str?}))

(fn state->validator
  [name validators-tbl]
  (let [validators-tbl (vim.tbl_map p.map validators-tbl)]
   (fn [a-state]
     (when (not (state? a-state))
       (error (.. "fsm [" name  "]: State must have an :is key received " (fennel.view a-state))))
     (let [valid-state? (. validators-tbl a-state.is)]
       (when (not valid-state?)
         (error (.. "fsm [" name "]: Could not validate state " a-state.is)))
       (when (not (valid-state? a-state))
         (error (.. "fsm [" name "]: Invalid state shape " (fennel.view a-state))))))))

(fn actions->validator
  [name validators-tbl]
  (let [validators-tbl (vim.tbl_map p.map validators-tbl)]
   (fn [action]
     (when (not (action? action))
       (error (.. "fsm [" name "]: Action must have a :type key. Unexpected shape: " (fennel.view action))))
     (let [valid-action? (. validators-tbl action.type)]
       (when (not valid-action?)
         (error (.. "fsm [" name "]: Encountered unknown action " action.type)))
       (when (not (valid-action? action))
         (error (.. "fsm [" name "]: Action failed validation " 
                    (fennel.view action))))))))
    
    
      

(fn defmachine
  [{: name : state : context : actions}]
  {:__fsm spec-tag
   : name
   :allowed {:states state
             :actions actions
             :context context}
   :validators {:state (state->validator name state)
                :context (p.map context)
                :actions (actions->validator name actions)}
   :transitions {}})

(fn fsm-spec?
  [fsm-tbl]
  (= fsm-tbl.__fsm spec-tag))

(fn validate
  [p?]
  (fn [x]
   (when (not (p? x))
     (error (.. "fsm Error: Validation failed for value " (fennel.view x))))
   true))

(fn validate-map
  [prefix validators-tbl tbl]
  (each [key p? (pairs validators-tbl)]
    (assert (p? (. tbl key)) (.. prefix " Validation failed for key " key " in " (fennel.view tbl))))
  true)
      
      
(fn validate-transition-shape
  [prefix transition-tbl]
  (and
    (assert (p.assoc? transition-tbl) 
            (.. prefix " Transition should be defined with an associative table. Received "
                (table.view transition-tbl)))
    (validate-map
      prefix
      {:on (p.listof p.str?)
       :when (p.listof p.str?)
       :do p.fn?}
      transition-tbl)))  

(fn validate-list-includes
  [prefix expected-map actual-list]
  (each [_ key (ipairs actual-list)]
    (assert (. expected-map key)
            (.. prefix 
                " Invalid option " (table.view key) 
                " expected to be " (table.view (vim.tbl_keys expected-map)))))
  true)

(fn machine-prefix
  [machine]
  (.. "fsm error [ " machine.name " ]: "))

(fn validate-transition
  [machine transition-tbl]
  (let [prefix (machine-prefix machine)]
    (validate-transition-shape prefix transition-tbl)
    (validate-list-includes
      (.. prefix "Transition on actions:")
      machine.allowed.actions 
      transition-tbl.on)
    (validate-list-includes
      (.. prefix "Transition when states:")
      machine.allowed.states
      transition-tbl.when)))

(fn append-transition
  [machine spec]
  (let [prefix (machine-prefix machine)]
   (each [_ state (ipairs transition-tbl.when)]
     (each [_ action (ipairs transition-tbl.on)]
       (let [key (.. state " " action)]
         (assert 
           (not (. machine.transitions key))
           (.. prefix "A transition for " key " already exists on this state-machine"))
         (tset machine.transitions key spec.do))))))

(fn transition
  [machine spec]
  (validate-transition spec)
  (append-transition machine spec)
  machine) 

(fn fsm-instance?
  [fsm]
  (= fsm.__fsm fsm-tag))

(fn new
  [machine {: state : context}]
  (assert (fsm-spec? machine) 
          (.. "fsm.new error: Expected a machine created with fsm.defmachine received unknown " 
              (fennel.view machine)))
  (let [prefix (.. "fsm.new [" machine.name "] error: ")]
    (assert (machine.validators.state state)
            (.. prefix "Initial state failed validation. Received " 
                (fennel.view state)))
    (assert (machine.validators.context context)
            (.. prefix "Initial context failed validation. Received " 
                (fennel.view context))))
  {:__fsm fsm-tag
   :machine machine
   :current {:state   state
             :context context
             :clear-effect (fn [] nil)}
   :listeners []})

(fn assert-fsm
  [fsm]
  (assert (fsm-instance? fsm)
          (.. "fsm error: Expected fsm instance, received "
              (fennel.view fsm))))

(fn error-prefix
  [fsm]
  (.. "fsm [" fsm.machine.name "] error: "))

(fn assert-action
  [fsm action]
  (assert (fsm.machine.validators.action action)
          (.. (error-prefix fsm) " DispatchActionError: Unrecognized action. Received " 
              (fennel.view action))))

(fn assert-state
  [fsm state]
  (assert (fsm.machine.validators.state state)
          (.. (error-prefix fsm) " DispatchStateError: Unrecognized state after transition. Received " 
              (fennel.view state))))

(fn assert-context
  [fsm context]
  (assert (fsm.machine.validators.context context)
          (.. (error-prefix fsm) " DispatchContextError: Unrecognized context after transition. Received " 
              (fennel.view context))))

(fn assert-clear-effect
  [fsm clear-effect]
  (assert (p.fn? clear-effect)
          (.. (error-prefix fsm) " DispatchEffectError: "
              "Expected effect generator to return a clean up function to cancel")))

(fn dispatch
  [fsm action]
  (assert-fsm fsm)
  (assert-action fsm action)
  (let [key (.. fsm.current.state.is " " action.type)
        transition (. fsm.machine.transitions key)
        result (transition fsm.current action)]
    (assert-state fsm result.state)
    (assert-context fsm result.context)
    ;; Update fsm
    (tset fsm.current :state result.state)
    (tset fsm.current :context result.context)
    ;; TODO: How to make this async?
    (when (p.fn? result.effect)
      (let [clear-effect (result.effect)]
        (assert-clear-effect fsm clear-effect)
        (tset fsm.current :clear-effect clear-effect)))
    ;; Call listeners
    (each [_ listener (ipairs fsm.listeners)]
      (when (= listener.event :on-transition)
        (listener.handler fsm)))
    fsm))

(fn subscribe
  [fsm event-type handler]
  (assert-fsm fsm)
  (table.insert fsm.listeners 
                {:event event-type
                 :handler handler}))

(fn unsubscribe
  [fsm event-type handler]
  (assert-fsm fsm)
  (table.insert))
  

(fn destroy
  [fsm]
  (assert-fsm fsm)
  (each [_ {: handler : event} (ipairs fsm.listeners)]
    (when (= event :on-destroy)
      (handler fsm)))
  (when (p.fn? fsm.clear-effect)
    (fsm.clear-effect))
  (each [k _v (pairs fsm)]
    (tset fsm k nil)))
    
(comment
  
  ;; Usage example
  (local my-fsm 
    (fsm.defmachine 
      {:name :test-fsm
       :state {:idle    {}
               :pending {:url (p.and 
                                p.str?
                                p.url?)
                         :started-at p.number?}
               :fulfilled {:response     (p.map {:data p.assoc?})
                           :data         p.assoc?
                           :completed-at p.number?}
               :failed    {:failed-at    p.number?}}
                                    
       :context {:request (p.or p.nil? (p.map {:url p.url?}))
                 :error   (p.or p.nil? p.error?)}

       :actions {:fetch   {:url p.url}
                 :resolve {:response p.assoc-table?}
                 :reject  {:error p.error?}}}))
                 

  (fsm.transition
    my-fsm
    {:on   [:fetch]
     :when [:idle :fulfilled :failed]
     :do
     (fn [{: state : ctx : cancel-effect &as machine} {: url &as action}]
       {:state {:is :pending
                :url url
                :started-at (now)}
        :context (merge ctx {:request {:url url}})
        ;; Pretend the fetch function exists and is async but the code below it 
        ;; is called after it completes
        :effect (fn []
                  (let [request (fetch url)
                        response request.response]
                    (if response.error
                      (dispatch {:type :reject  :error response.error})
                      (dispatch {:type :resolve :response response}))
                    (fn destroy [] 
                      (request:abort))))})})

  (fsm.transition
    my-fsm
    {:on   [:resolve]
     :when [:pending]
     :do
     (fn [{: state : ctx : cancelEffect &as machine} {: response :as}]
       {:state {:is :fulfilled
                :response response
                :data     response.data
                :completed-at (now)}
        :context (dissoc ctx :error)})})
  
  (fsm.transition
    my-fsm
    {:on   [:reject]
     :when [:pending]
     :do
     (fn [{: state : ctx : cancelEffect &as machine} {:error err &as action}]
       {:state {:is :failed
                :failed-at (now)}
        :context (merge ctx {:error err})})})

  ;; ...More transitions if desired...

  ;; Creating an instance

  (local a-request-fsm (fsm.new my-fsm {:state {:is :idle}
                                        :context {}}))
  
  (print a-request-fsm.state)
  ;; => {:state {:is :idle} :context {}}
  (a-request-fsm.dispatch {:type :fetch :url "https://example.com"})
  (print a-request-fsm.state)
  ;; => {:state {:is :pending} 
  ;;     :context {:request {:url "https://example.com"}}))
  ;;     :cancel-effect #<function: 0xaabbcc>}

  ;; About 200ms later...
  (print a-request-fsm.state))
  ;; => {:state {:is :fulfilled 
  ;;             :response #<table: 0xaabbcc> 
  ;;             :data #<table: 0xddeeff>}
  ;;     :context {:request {:url "https://example.com"}}}

  

{: defmachine
 : fsm-spec?
 : fsm-instance?
 : transition
 : dispatch
 : subscribe
 : destroy}
