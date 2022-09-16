(local utils {:core {}
              :string {}})

(fn utils.string.starts-with?
  [s part]
  (= (string.sub s 1 (length part)) part))

(fn utils.string.includes?
  [s part]
  (string.find s part))
   
(fn utils.core.reduce
  [f init coll]
  (var acc init)
  (each [i v (ipairs coll)]
    (set acc (f acc v i)))
  acc)

utils
