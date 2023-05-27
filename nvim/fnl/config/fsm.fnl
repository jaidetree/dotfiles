(local fsm-tag {:type :fsm})

(fn defmachine
  [{: name : state : context}]
  {:__fsm fsm-tag
   : name
   :validators {:state :context}
   :transitions {}
   :listeners {:on-destroy []}})

(fn fsm?
  [fsm-tbl]
  (= fsm-tbl.__fsm fsm-tag))

(fn assert-tbl
  [maybe-tbl error-msg]
  (when (not= (type maybe-tbl) :table)
    (error error-msg))
  maybe-tbl)

(fn assert-fn
  [maybe-fn error-msg]
  (when (not= (type maybe-fn) :function)
    (error error-msg))
  maybe-fn)

(fn validate-transition
  [machine fsm-spec]
  (let [{: name} machine]
    (assert-tbl 
      fsm-spec.on
      (.. "fsm.transition error: :on should be a table of action strings"))
    (assert-tbl 
      fsm-spec.in
      (.. "fsm.transition error: :in should be a table of valid machine state strings"))
    
    (assert-fn
      fsm-spec.transition)))
    

(fn transition
  [machine fsm-spec]
  (validate-transition fsm-spec)) 
    
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
        :effect (fn [{: dispatch}]
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
 : fsm?
 : transition}
