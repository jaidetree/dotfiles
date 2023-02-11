(local utils {:core {}
              :string {}})

(fn utils.string.starts-with?
  [s part]
  (= (string.sub s 1 (length part)) part))

(fn utils.string.includes?
  [s part]
  (string.find s part))

(fn utils.core.each
  [f coll]
  (if (= (type coll) :function)
    (each [v coll]
      (f v coll))
    (each [i v (ipairs coll)]
      (f v i coll))))

(fn utils.core.reduce
  [f init coll]
  (var acc init)
  (utils.core.each
    (fn [v]
      (set acc (f acc v)))
    coll)
  acc)

(fn utils.core.filter
  [f coll]
  (let [filtered []]
    (utils.core.each
      (fn [v]
        (when (f v)
         (table.insert filtered v)))
      coll)
    filtered))

(fn utils.core.map
  [f coll]
  (let [mapped []]
    (utils.core.each
      (fn [v]
        (table.insert mapped (f v))))
    mapped))

(fn utils.core.prop
  [prop-name tbl]
  (. tbl prop-name))

(fn utils.core.seq
  [iterator-or-coll]
  (let [tbl []]
   (utils.core.each
     (fn [v]
       (table.insert tbl v))
     iterator-or-coll)
   tbl))

utils
