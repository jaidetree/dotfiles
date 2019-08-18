(local atom (require :lib.atom))
(local {:concat concat
        :slice slice} (require :lib.functional))
(local fu hs.fnutils)

(local state (atom.new {:cell ""
                        :cycle (fn [] nil)}))

(fn create-cycle
  []
  (let [current (hs.screen.mainScreen)
        screens (hs.screen.allScreens)
        idx     (fu.indexOf screens current)
        sorted (if (= idx 0)
                   screens
                   (concat
                    (slice idx screens)
                    (slice 0 (- idx 1) screens)))]
    (print "Screens: ")
    (print (hs.inspect sorted))
    (fu.cycle sorted)))

(fn update-cycle
  [cell]
  (let [cycle (create-cycle)]
    (atom.reset! state {:cell cell
                        :cycle cycle})
    cycle))

(fn get-cycler
  [cell]
  (let [{:cell current
         :cycle cycle} (atom.deref state)]
    (if (= current cell)
        cycle
        (update-cycle cell))))

(fn grid-resize
  [cell]
  (fn resizer []
    (let [w (hs.window.frontmostWindow)
          get-screen (get-cycler cell)
          screen (get-screen)]
      (hs.grid.set w cell screen))))

(fn full-size
  []
  (hs.eventtap.keyStroke [:alt :cmd :ctrl :shift] :1))

(fn left-half
  []
  (hs.eventtap.keyStroke [:alt :cmd :ctrl :shift] :2))

(fn right-half
  []
  (hs.eventtap.keyStroke [:alt :cmd :ctrl :shift] :3))

(fn left-big
  []
  (hs.eventtap.keyStroke [:alt :cmd :ctrl :shift] :4))

(fn right-small
  []
  (hs.eventtap.keyStroke [:alt :cmd :ctrl :shift] :5))

{:grid-resize grid-resize
 :full-size   full-size
 :left-half   left-half
 :right-half  right-half
 :left-big    left-big
 :right-small right-small}
