(local {:merge merge} (require :lib.functional))
(local atom (require :lib.atom))

(local log (hs.logger.new "\tshadow.fnl\t" "debug"))


(local state (atom.new {:show-icon (hs.dockIcon)}))

(fn center
  []
  (let [app (hs.application.get "com.blade.shadow-macos")
        window (app:focusedWindow)
        frame (: window :frame)
        w (/ frame._w 2)
        h (/ frame._h 2)]
    (window.focus)
    {:x w :y h}))

(fn rand
  []
  (let [app (hs.application.get "com.blade.shadow-macos")
        window (app:focusedWindow)
        frame (: window :frame)
        w frame._w
        h frame._h
        x frame._x
        y frame._y]
    (app:activate)
    (print (hs.inspect frame))
    {:x (math.random x (+ x w))
     :y (math.random y (+ y h))}))

(fn update-state
  [new-state]
  (let [old-state (atom.deref state)]
    (atom.reset! state (merge old-state new-state))))

(fn click
  [coords]
  (hs.eventtap.leftClick coords))

(fn loop-clicks
  [count]
  (when (> count 0)
    (hs.timer.doAfter
     1
     (fn []
       (let [coords (rand)]
         (hs.mouse.setAbsolutePosition coords)
         (click coords)
         (loop-clicks (- count 1)))))))

(fn caffiene
  []
  (let [timer (hs.timer.doEvery
               (hs.timer.minutes 0.5)
               #(loop-clicks 3))]
    (loop-clicks 3)
    (fn cleanup
      []
      (timer:stop))))

(fn keep-awake-on
  []
  (log.d "Turning on shadow keep-awake")
  (hs.dockicon.hide)
  (let [rect (hs.drawing.rectangle (: (hs.window.focusedWindow) :frame))]
    (: rect :setStrokeColor {:red 1 :blue 0.5 :green 1 :alpha 1})
    (: rect :setStrokeWidth 5)
    (: rect :setFill false)
    (: rect :show)
    {:active true
     :rect rect
     :cleanup-caffiene (caffiene)
     :alert-id (alert "  Keeping shadow awake  "
                      {:textFont "Menlo"
                       :textSize 16}
                      :forever)}))

(fn keep-awake-off
  [data]
  (log.d "Turning off shadow keep-awake")
  (hs.alert.closeSpecific data.alert-id)

  (hs.dockicon.show)
  (: data.rect :delete)
  (data.cleanup-caffiene)
  {:active false
   :alert-id ""
   :rect ""})


(fn keep-awake
  []
  (let [data (atom.deref state)]
    (update-state
     (if data.active
         (keep-awake-off data)
         (keep-awake-on data)))))

{:keep-awake keep-awake}
