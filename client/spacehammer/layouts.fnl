(local atom (require :lib.atom))
(local {:concat concat
        :slice slice} (require :lib.functional))
(local fu hs.fnutils)

(var idx 0)
(var cell "")
(var last-window {})
(var screens [])

(fn next-idx
  [idx]
  (let [idx (+ idx 1)
        count (length screens)]
    (if (< idx count)
        idx
        1)))

(fn cycle-screens
  [win target-cell]
  (set screens (hs.screen.allScreens))
  (let [next-idx (if (and (= target-cell cell)
                          (= win last-window))
                     (next-idx idx)
                     1)
        screen (. screens next-idx)]
    (set last-window win)
    (set cell target-cell)
    (set idx next-idx)
    (print (hs.inspect {:win win
                        :cell target-cell
                        :idx next-idx}))
    screen))

(fn grid-resize
  [cell]
  (let [w (hs.window.frontmostWindow)
        screen (cycle-screens w cell)]
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
