(local stream (require :stream))
(local is (require :lib.testing.assert))

(describe
 "Stream"
 (fn []
   (-> (stream.from-list [1 2 3])
       (stream.map #(* $1 2))
       (stream.filter #(> $ 1))
       (stream.subscribe
        (fn [x]
          (is.eq? (type x) "number" "Did not receive number")
          (is.eq? (> x 1) true "Values were not doubled"))))))
