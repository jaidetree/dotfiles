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
  (if
    ;; Iterator was possibly an iterator function
    (= (type iterator-or-coll) :function)
    iterator-or-coll

    ;; Iterator is a sequential table based on having
    ;; a 1-index. Note in lua can be mixed but fennel
    ;; does not readily support that
    (and (= (type iterator-or-coll) :table)
         (. iterator-or-coll 1))
    (ipairs iterator-or-coll)

    ;; Iterator is an associative table
    (= (type iterator-or-coll) :table)
    (pairs iterator-or-coll)

    ;; No idea
    (error (.. "utils.core.seq: Unable to provide iterator for " (vim.inspect iterator-or-coll)))))

(fn utils.core.pairs->tbl
  [pairs-tbl]
  (accumulate
    [assoc-tbl {}
     _ [k v] (ipairs pairs-tbl)]
    (do
      (tset assoc-tbl k v)
      assoc-tbl)))

(fn utils.core.concat
  [& tbls]
  (utils.core.reduce
    (fn [concated tbl]
      (utils.core.each
        (fn [v]
          (table.insert concated v))
        tbl)
      concated)
    []
    tbls))

(fn utils.core.update
  [tbl k f & args]
  (let [v (. tbl k)]
   (when v
    (tset tbl k (f v (unpack args)))))
  tbl)

(fn utils.core.merge
  [& tbls]
  (accumulate
    [merged {}
     _ tbl (ipairs tbls)]
    (do
     (each [k v (pairs tbl)]
       (tset merged k v))
     merged)))

(fn utils.core.inc
  [x]
  (+ x 1))

(fn utils.core.count
  [x]
  (let [data-type (type x)]
   (case (type x)
     :string (length x)
     :table  (length (vim.tbl_keys x)))))

utils
