(local atom (require :lib.atom))
(local {:concat concat
        :slice slice
        :merge merge} (require :lib.functional))
(local fu hs.fnutils)

"
Window Layouts
- Defines common layouts like full screen, left half, right half, left
  two-thirds, and right one-third.
- When a layout is activated it will try to apply it to the current window on
  the current screen.
- When a layout is repeated to the same window it cycles screens
- We store prev window, layout, screen index, and window frame dimensions to
  know when to apply it to current screen versus next screen.
- By tracking the frame dimensions we can determine if the window was adjusted
  outside of the layouts system without any complex watching systems.
"

(local state (atom.new
              {:index 0
               :cell ""
               :window {}
               :frame ""}))

(fn update
  [update-data]
  "
  Update the state atom with a partial update
  Takes the new partial table data to merge into current state
  Returns previous state
  "
  (atom.swap! state (fn [current-data]
                      (merge current-data update-data))))

(fn win->frame-str
  [window]
  "
  Serialize a window's geometry frame into a string for fast comparison
  Takes a hs.window instance
  Returns a string serializing the geometry rectangle measurements
  "
  (let [rect (: window :frame)]
    rect.string))

(fn next-screen-index
  [idx total]
  "
  Calculate the next screen index. Wraps to first screen index.
  Note lua is 1-index based
  Takes the current screen index and the total number of screens.
  Returns the next screen index.
  "
  (let [idx (+ idx 1)]
    (if (<= idx total)
        idx
        1)))

(fn current-screen-index
  [screen screens]
  "
  Get current screen index
  Takes the current screen and a table of screens
  Returns the index number of the current screen
  "
  (fu.indexOf screens screen))

(fn current-state
  [cell]
  "
  Return an updated state-like table to perform all our ready side-effects
  Takes the target grid cell string
  Returns a table. The cell, window, and frame are used to compare against the
  previous state while screens and screen are used for calculating the next
  screen index.
  "
  (let [window (hs.window.frontmostWindow)
        frame (win->frame-str window)]
    {:cell cell
     :window window
     :frame frame
     :screens (hs.screen.allScreens)
     :screen (hs.screen.mainScreen)}))

(fn repeated-update?
  [prev current]
  "
  Determine if the user is repeating the same layout as they applied previously.
  We also use the frame string to see if the window was adjusted since the
  last layout. This predicate is used to determine if the window should be moved
  to the next screen or current frame to begin the cycle if repeated.

  Takes the previous state table from (atom.deref state) and the current-state
  table.
  Returns true or false if cell, window, and frame string matches between prev
  and current states.
  "
  (and (= prev.cell current.cell)
       (= prev.window current.window)
       (= prev.frame current.frame)))

(fn grid-resize
  [cell]
  "
  Main function for positioning a window to a grid layout. Gets state, resizes
  and repositions a window, then updates the stotred state to comapre on next
  layout that is triggered.

  It's advised to make any custom layouts wrap this function.

  Takes grid cell dimensions like \"x, y wxh\"
  Returns previous state table
  "
  (let [prev (atom.deref state)
        current (current-state cell)
        {:screens screens
         :screen current-screen} current
        index (if (repeated-update? prev current)
                  (next-screen-index prev.index (length screens))
                  (current-screen-index current-screen screens))
        next-screen (. screens index)]
    (hs.grid.set current.window cell next-screen)
    (update {:index index
             :cell cell
             :window current.window
             :frame (win->frame-str current.window)})))

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
