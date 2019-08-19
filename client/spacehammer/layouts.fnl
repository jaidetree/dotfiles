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
  (let [w (hs.window.frontmostWindow)
        get-screen (get-cycler cell)
        screen (get-screen)]
    (hs.grid.set w cell screen)))

(fn full-size
  []
  (grid-resize "0, 0 8x2"))

(fn left-half
  []
  (grid-resize "0, 0 4x2"))

(fn right-half
  []
  (grid-resize "4,0 4x2"))

(fn left-big
  []
  (grid-resize "0,0 6x2"))

(fn right-small
  []
  (grid-resize "6,0 2x2"))

{:grid-resize grid-resize
 :full-size   full-size
 :left-half   left-half
 :right-half  right-half
 :left-big    left-big
 :right-small right-small}
