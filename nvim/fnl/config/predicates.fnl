(local p {})

(fn p.str?
  [x]
  (= (type x) :string))

(fn p.number?
  [x]
  (= (type x) :number))

(fn p.fn?
  [x]
  (= (type x) :function))

(fn p.nil?
  [x]
  (= x nil))

(fn p.table?
  [x]
  (= (type x) :table))

(fn p.list?
  [x]
  (and 
    (p.table? x)
    (vim.tbl_islist x)))

(fn p.assoc?
  [x]
  (and 
    (p.table? x)
    (not (vim.tbl_islist x))))

(fn p.and
  [& preds]
  (fn [x]
   (accumulate
     [res true
      _ p? (ipairs preds)
      &until (not res)]
     (and res (p? x)))))
    
(fn p.or
  [& preds]
  (fn [x]
   (accumulate
     [res false
      _ p? (ipairs preds)
      &until res]
     (p? x))))

(fn p.map
  [pred-map]
  (fn [tbl]
   (accumulate
     [res (p.assoc? tbl)
      key p? (pairs pred-map)
      &until (not res)]
     (and res (p? (. tbl key))))))
     
(fn p.tuple
  [& preds]
  (fn [tbl]
    (accumulate
      [res (p.list? tbl)
       i p? (ipairs preds)
       &until (not res)]
      (and res (p? (. tbl i))))))

(fn p.listof
  [p?]
  (fn [tbl]
    (and 
      (p.list? tbl)
      (let [[x & xs] tbl]
        (accumulate
          [res (p? x)
           i v (ipairs xs)
           &until (not res)]
          (and res (p? v)))))))
          

;; Exports

p
