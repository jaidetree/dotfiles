(require-macros :spacehammer.lib.macros)
(require-macros :spacehammer.lib.advice.macros)
(local statemachine (require :spacehammer.lib.statemachine))
(local {: eq?
        : filter
        : first
        : for-each
        : last
        : map
        : merge
        : reduce} (require :spacehammer.lib.functional))


;; Create some concise aliases

(local fu hs.fnutils)
(local keycodes hs.keycodes.map)
(local event-types hs.eventtap.event.types)

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

(fn create-canvas-screen
  [tbl screen]
  (let [screen-id (screen:id)
        frame (screen:fullFrame)]
    (tset tbl screen-id
          (doto (hs.canvas.new frame)
            (: :canvasMouseEvents false false false false)))
    tbl))

(fn create-canvases
  []
  "
  Creates a canvas the size of the currently active screen
  "
  (->> (hs.screen.allScreens)
       (reduce create-canvas-screen {})))

(fn screen-id
  []
  (-> (hs.screen.mainScreen) (: :id)))

(fn select-canvas
 [context]
 (merge context
        {:canvas (. context.canvases (screen-id))}))

(fn ready->edit
  [state action extra]
  {:state  {:current-state :edit
            :context
            (if state.context.canvases
                (select-canvas state.context)
                (merge
                 state.context
                 (let [canvases (create-canvases)]
                   {:canvases canvases
                    :canvas (. canvases (screen-id))})))}
   :effect :edit})

(fn ->clear
  [state action extra]
  {:state {:current-state :ready
           :context (select-canvas state.context)}
   :effect :clear})

(fn ->toggle
  [state action]
  {:state {:current-state :ready
           :context (select-canvas state.context)}
   :effect :toggle})

(fn edit->edit
  [state action extra]
  {:state  {:current-state :edit
            :context (select-canvas state.context)}
   :effect :edit})

(fn edit->done
  [state action extra]
  {:state  {:current-state :ready
            :context (select-canvas state.context)}
   :effect :exit})

(fn edit->create
  [state actions extra]
  {:state  {:current-state :create
            :context (select-canvas state.context)}
   :effect :create-guide})

(fn edit->move-guide
  [state actions extra]
  {:state {:current-state :move
           :context (select-canvas state.context)}
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
  [state actions {: point}]
  {:state {:current-state :edit
           :context (-> state.context
                        (select-canvas)
                        (merge {:last-point point}))}
   :effect :edit})

(fn create->cancel
  [state actions extra]
  {:state {:current-state :edit
           :context (select-canvas state.context)}
   :effect :edit})

;; May not be needed?
;; (fn move->move
;;   [state actions extra]
;;   {:state {:current-state state.current-state
;;            :context       state.context}
;;    :effect :move-guide})

(fn move->done
  [state actions {: point}]
  {:state {:current-state :edit
           :context       (-> state.context
                              (select-canvas)
                              (merge {:last-point point}))}
   :effect :edit})

(fn move->cancel
  [state actions extra]
  {:state {:current-state :edit
           :context       (select-canvas state.context)}
   :effect :edit})

(fn ->error
  [state actions err]
  {:state {:current-state :ready
           :context       (select-canvas state.context)}
   :effect :exit})


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
        :states {:ready    {:edit        ready->edit
                            :clear       ->clear
                            :toggle      ->toggle}
                 :edit     {:done        edit->done
                            :create      edit->create
                            :move        edit->move-guide
                            :clear       ->clear
                            :toggle      ->toggle
                            :screen      edit->edit
                            :error       ->error}
                 :create   {;:mouse-move  create->move
                            :done        create->done
                            :escape      create->cancel
                            :error       ->error}
                 :move     {;:mouse-move  move->move
                            :done        move->done
                            :escape      move->cancel
                            :error       ->error}}
        :log :guide})

(local fsm (statemachine.new machine))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local colors {})

(fn colors.cyan
  []
  {:red 0.0 :green 1.0 :blue 1.0 :alpha 1.0})

(fn colors.magenta
  []
  {:red 1.0 :green 0.0 :blue 1.0 :alpha 1.0})

(fn colors.black
  []
  {:red 0 :green 0 :blue 0 :alpha 1.0})

(fn do-xpc!
  [f]
  (let [(ok err) (xpcall f debug.traceback)]
    (when (not ok)
      (hs.showError err))
    (values ok err)))

(fn tap-fx
  [{: events} handler]
  (var tap {:start (fn [] nil)
            :stop  (fn [] nil)})
  (fn tap-handler
    [event]
    (let [state {:ret-value {}}
          (ok err) (do-xpc! (fn tap-handler-cb
                              []
                              (tset state :ret-value (handler event))))
          {: continue : post-events } (or state.ret-value {})]
      ;; Continue determines if the event handler chain should continue
      ;; as it is possible to prevent other handlers from firing while
      ;; still posting the original keyboard event
      (match [ok continue post-events]
        ;; No errors and continue event handler chain with post-events
        [true true events]    (values false events)

        ;; No errors don't continue and ignore post-events
        [true false ?events]  (values true [])

        ;; No errors but maybe nil continue and maybe nil events
        [true ?cont ?events]  (values false [event])

        ;; Caught error ignore continue and events
        [false ?cont ?events] (do
                                (tap:stop)
                                (fsm:send :error err)
                                (values false [event])))))


  (set tap (hs.eventtap.new
            (map #(. event-types $1) events)
            tap-handler))
  (tap:start)
  (fn cleanup
    []
    (tap:stop)))

(fn mouse-fx
  [{: events : canvas} handler]
  (tap-fx
   {: events}
   (fn [event]
     (let [point (event:location)
           frame (-> (hs.screen.mainScreen) (: :fullFrame))
           relative (hs.geometry.point
                     (- point.x frame.x)
                     (- point.y frame.y))]
       (handler event relative)))))

(fn key-fx
  [{: key} handler]
  "
  An effect handler for responding to a specific keyboard event like keyDown
  or keyUp

  Takes a map with a key string representing an entry of hs.keycodes.map and
  an event handler function that receives the event

  Returns a cleanup function to remove the eventtap
  "
  (tap-fx
   {:events [:keyUp]}
   (fn key-fx-handler
     [event]
     (let [keycode (event:getKeyCode)]
       (if (= keycode (. keycodes key))
           (do
             (handler event)
             {:continue false
              :post-events []})
           {:continue true
            :post-events [event]})))))

(fn combine-fx
  [...]
  (let [unsubscribe-fns [...]]
    (fn []
      (each [i unsubscribe (ipairs unsubscribe-fns)]
        (unsubscribe)))))

(fn remove-at
  [canvas index]
  (: canvas :removeElement index))

(fn remove-by-id
  [canvas id]
  (-?>> (canvas:canvasElements)
        (map (fn [element index] [index element]))
        (filter (fn [[ index element]]
                  (= element.id id)))
        (first)
        (first)
        (remove-at canvas)))

(fn create-mode-label
  [canvas]
  (canvas:appendElements
   {:action :build
    :id :mode-label
    :type :text
    :frame {:h 60 :w 300
            :y 50
            :x (- (-> (hs.screen.mainScreen)
                      (: :frame)
                      (. :w))
                  340)}


    :textAlignment :right
    :textColor (colors.cyan)
    :textSize 30
    :textFont "Helvetica"
    :text ""}))

(fn sort
  [f xs]
  (let [tbl []]
    (each [_k v (ipairs xs)]
      (table.insert tbl v))
    (table.sort tbl f)
    tbl))

(fn asc
  [f]
  (fn [v1 v2]
    (< (f v1) (f v2))))

(fn desc
  [f]
  (fn [v1 v2]
    (> (f v1) (f v2))))

(fn intersect?
  [point guide]
  (let [{: x : y : w : h} guide.frame
        point (hs.geometry.new point)
        guide-rect (hs.geometry.new (- x 5) (- y 5)
                                    (+ w 15) (+ h 15))]
    (point:inside guide-rect)))


(fn near-guide?
  [point guide]
  (and (= guide.type "rectangle")
       (string.find guide.id "guide-" 1 true)
       (intersect? point guide)))

(fn distance
  [point guide]
  (let [{: x : y : w : h} guide.frame]
    (math.min (math.abs (- y point.y))
              (math.abs (- x point.x)))))

(fn guide->direction
  [guide]
  (match (?. guide :frame)
    {:h 1} :horizontal
    {:w 1} :vertical))

(fn find-nearest-guide
  [canvas point]
  (->> (canvas:canvasElements)
       (reduce
        (fn [tbl element index]
          (when (near-guide? point element)
            (table.insert
             tbl
             {:index index
              :distance (distance point element)
              :element   element}))
          tbl)
        [])
       (sort (asc #(. $1 :distance)))
       (first)))


(fn edit
  [state extra]
  (let [canvas state.context.canvas
        watcher (hs.screen.watcher.newWithActiveScreen
                 (fn [active-changed]
                   (when active-changed
                     (remove-by-id state.context.canvas :mode-label)
                     (fsm.send :screen nil))))]

    (watcher:start)

    ;; Show the canvas
    (canvas:show)
    (canvas:clickActivating false)

    ;; Create the mode label text element if it does not exist
    (when (not canvas.mode-label)
      (create-mode-label canvas))

    (tset canvas :mode-label :text "edit guides")

    (combine-fx
     (mouse-fx
      {: canvas
       :events [:leftMouseDown]}
      (fn [event point]
        (let [is-clicked (event:getButtonState 0)
              near-guide (find-nearest-guide canvas point)]
          (if
           near-guide
           (let [element (. canvas near-guide.index)]
             (fsm.send :move
                       {: element
                        : point
                        :direction (guide->direction element)})
             {:continue false
              :post-events []})

           (and is-clicked (>= point.x 0) (< point.x 3))
           (do
             (fsm.send :create {:direction :vertical
                                :point point})
             {:continue false
              :post-events []})

           (and is-clicked (>= point.x 0) (< point.y 3))
           (do
             (fsm.send :create {:direction :horizontal
                                :point point})
             {:continue false
              :post-events []})))))

     (mouse-fx
      {: canvas
       :events [:mouseMoved]}
      (fn [event point]
        (var hover-target nil)
        ;; Going old-school on this one, performance is important here
        (each [index element (ipairs (canvas:canvasElements))]
          (when (and (= element.type "rectangle")
                     (string.find element.id "guide" 1 true))
            (if (and (intersect? point element) (not hover-target))
                (do
                  (tset canvas index :fillColor (colors.magenta))
                  (set hover-target index))
                (tset canvas index :fillColor (colors.cyan)))))))


     (key-fx
      {:key :escape}
      (fn [event]
        (remove-by-id state.context.canvas :mode-label)
        ;;(state.context.canvas:removeElement 1)
        (fsm.send :done)))

     (fn cleanup []
       (watcher:stop)
       (canvas:mouseCallback nil)))))


(fn clear
  [state extra]
  (match state.context.canvases
    ;; for is recommended here given that deleting from 1 shifts all indexes
    ;; so forms like (each) or (map) would not work here without either
    ;; reversing the table list of elements or just using for
    canvases (each [_i canvas (pairs canvases)]
               (for [index (length (canvas:canvasElements)) 1 -1]
                 (remove-at canvas index)))
    _ nil))

(fn toggle
  [state extra]
  (when-let [canvas state.context.canvas]
            (if (canvas:isShowing)
                (canvas:hide)
                (canvas:show)))
  nil)

(fn exit
  [state extra]
  nil)

(fn create-guide
  [state {: direction : point}]
  (let [canvas state.context.canvas
        screen (hs.screen.mainScreen)
        frame (screen:fullFrame)]

    (canvas:appendElements
     {:id "new-guide"
      :action "fill"
      :fillColor (colors.magenta)
      :frame (if (= direction :horizontal)
                 {:x 0 :y 0 :w frame.w :h 1}
                 (= direction :vertical)
                 {:x 0 :y 0 :w 1 :h frame.h})
      :type "rectangle"})

    (tset canvas :mode-label :text "create guide")

    (combine-fx
     (mouse-fx
      {: canvas
       :events [:leftMouseDragged]}
      (fn [event point]
        (let [guide (. canvas :new-guide)]
          (if (= direction :horizontal)
              (tset guide :frame :y point.y)
              (= direction :vertical)
              (tset guide :frame :x point.x))
          {:continue true
           :post-events []})))

     (mouse-fx
      {: canvas
       :events [:leftMouseUp]}
      (fn [event point]
        (match {:x point.x :y point.y : direction}
          {:direction :horizontal :y 0} (remove-by-id canvas :new-guide)
          {:direction :vertical :x 0} (remove-by-id canvas :new-guide)
          _ (do (tset canvas :new-guide :fillColor (colors.cyan))
                (tset canvas :new-guide :id (.. "guide-" (math.random 100 999)))))
        (fsm.send :done {:point point})
        {:continue false
         :post-events []}))

     (key-fx
      {:key :escape}
      (fn [event]
        (remove-by-id canvas :new-guide)
        (fsm.send :escape))))))



(fn move-guide
  [state {: element :point origin : direction}]
  (let [canvas state.context.canvas
        frame element.frame
        { : x : y : w : h } frame]

    (tset canvas :mode-label :text "move guide")

    (combine-fx
     (mouse-fx
      {: canvas
       :events [:leftMouseDragged]}
      (fn [event point]
        (if (= direction :horizontal)
            (do
              (tset frame :y point.y))
            (do
              (tset frame :x point.x)))))

     (mouse-fx
      {: canvas
       :events [:leftMouseUp]}
      (fn [event point]
        (match {:x point.x :y point.y : direction}
          {:direction :horizontal :y 0} (remove-by-id canvas element.id)
          {:direction :vertical :x 0} (remove-by-id canvas element.id)
          _ (tset element :fillColor (colors.cyan)))
        (fsm.send :done {:point point})
        {:continue false
         :post-events []}))

     (key-fx
      {:key :escape}
      (fn [event]
        (match direction
          :horizontal (tset frame :y origin.y)
          :vertical (tset frame :x origin.x))
        (tset element :fillColor {:red 0 :green 255 :blue 255})
        (fsm.send :escape))))))



(local primary-effects
       (statemachine.effect-handler
        {: edit
         : clear
         : toggle
         : exit
         : create-guide
         : move-guide}))

(fn format-coords
  [{: current : origin}]
  (if (not origin)
      (.. (math.floor current.x) ", " (math.floor current.y))
      (.. (math.floor current.x) ", "
          (math.floor current.y)
          "  Δ: "
          (math.floor (- current.x origin.x)) ", "
          (math.floor (- current.y origin.y)))))

(fn calc-coords-pos
  [coords-box direction point]
  "
  Determines where to position the coordinates label box.
  If the user gets close to the right edge or bottom edge place the coords box
  to the left and/or above the guide.

  Takes a canvas that contains the coordinates text, direction of the guide, and
  a point hs.geometry tbl where the mouse is currently

  Returns a table with x and y coordinates
  "
  (let [coords (coords-box:frame)
        screen (-> (hs.screen.mainScreen) (: :frame))
        bottom-edge (+ screen.y (- screen.h coords.h 10))
        right-edge (+ screen.x (- screen.w coords.w 10))]
    (match direction
      :horizontal
      {:x (if (< point.x right-edge)
              point.x
              (- point.x coords.w 10))
       :y (if (< point.y bottom-edge)
              (+ point.y 10)
              (- point.y coords.h 10))}

      :vertical
      {:x (if (< point.x right-edge)
              (+ point.x 10)
              (- point.x coords.w 10))
       :y (if (< point.y bottom-edge)
              point.y
              (- point.y coords.h 10))})))


(fn pos-canvas-frame
  [direction point]
  (match [direction point]
    [:horizontal nil] {:x point.x
                       :y (+ point.y 10)
                       :w 140
                       :h 30}
    [:vertical nil]   {:x (+ point.x 10)
                       :y point.y
                       :w 140
                       :h 30}

    [:horizontal point] {:x point.x
                         :y (+ point.y 10)
                         :w 200
                         :h 30}
    [:vertical   point] {:x (+ point.x 10)
                         :y point.y
                         :w 200
                         :h 30}))


(fn create-pos-canvas
  [{: direction : initial : origin}]
  (let [pos-canvas (hs.canvas.new (pos-canvas-frame direction initial))]
    (doto pos-canvas
      (: :appendElements
         {:action "strokeAndFill"
          :strokeColor (colors.cyan)
          :strokeWidth 1
          :fillColor (merge (colors.black) {:alpha 0.4})
          :frame {:x "0" :y "0" :w "1" :h "1"}
          :type "rectangle"}
         {:action "build"
          :id "label"
          :textColor (colors.cyan)
          :textAlignment :center
          :padding 0
          :frame {:x "0.0" :y "0.1" :w "1" :h "1"}
          :textFont "Helvetica"
          :textSize 18
          :text (format-coords
                 {:current initial
                  :origin  origin})
          :type "text"})
      (: :show))))

(fn show-pos
  [state {: direction :point initial : element}]
  (let [canvas state.context.canvas
        last-point state.context.last-point
        origin (if element initial
                   last-point last-point)
        pos-canvas (create-pos-canvas
                    {: direction
                     : initial
                     : origin})]

    (combine-fx
     (mouse-fx
      {: canvas
       :events [:leftMouseDragged]}
      (fn [event point]
        (let [current (event:location)
              pos (calc-coords-pos pos-canvas direction current)]
          (tset pos-canvas :label :text
                (format-coords
                 {:current point
                  : origin}))
          (pos-canvas:topLeft pos)
          {:continue true
           :post-events []})))

     (fn cleanup
       []
       (pos-canvas:delete)))))


(fn color->tbl
  [color]
  {:red color.red
   :blue color.blue
   :green color.green
   :alpha color.alpha})

(fn show-hints
  [state extra]
  (let [canvas state.context.canvas
        canvas-frame (canvas:frame)
        cyan (colors.cyan)
        magenta (colors.magenta)]
    (canvas:appendElements
     {:id "hint-top"
      :action "fill"
      :type "rectangle"
      :fillColor cyan
      :frame {:x 2 :y 0 :w canvas-frame.w :h 2}}
     {:id "hint-left"
      :action "fill"
      :type "rectangle"
      :fillColor cyan
      :frame {:x 0 :y 2 :w 2 :h canvas-frame.h}})

    (combine-fx
     (mouse-fx
      {: canvas
       :events [:mouseMoved]}
      (fn [event point]
        (let [top-color (color->tbl canvas.hint-top.fillColor)
              left-color (color->tbl canvas.hint-left.fillColor)
              at-left (and (>= point.x 0) (< point.x 3))
              at-top (and (>= point.x 0) (< point.y 3))]
          (if at-top
              (when (eq? top-color cyan)
                (tset canvas :hint-top :fillColor magenta))

              at-left
              (when (eq? left-color cyan)
                (tset canvas :hint-left :fillColor magenta))

              (do
                (when (eq? top-color magenta)
                  (tset canvas :hint-top :fillColor cyan))

                (when (eq? left-color magenta)
                  (tset canvas :hint-left :fillColor cyan)))))))



     (fn cleanup-hints
       []
       (remove-by-id canvas :hint-top)
       (remove-by-id canvas :hint-left)))))


(local secondary-effects
       (statemachine.effect-handler
        {:edit         show-hints
         :create-guide show-pos
         :move-guide   show-pos}))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local api {: fsm})

(fn api.edit
  []
  (fsm.send :edit))

(fn api.clear
  []
  (fsm.send :clear))

(fn api.toggle
  []
  (fsm.send :toggle))

(tset api :unsubscribe
      (combine-fx
       (fsm.subscribe primary-effects)
       (fsm.subscribe secondary-effects)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

api
