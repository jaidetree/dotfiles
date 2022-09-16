(local  transform-stream {})

(fn transform-stream.new
  [source impl]
  (let [stream {:subscribers []
                :source      source
                :ended       false}]
    (fn stream.end
      [self]
      (if self.ended
          (error "Stream: Cannot close an already ended stream")
          (do (tset self :ended true)
              true)))

    (fn stream.push
      [self value]
      (each [_i subscriber (ipairs self.subscribers)]
        (subscriber value))
      self)

    (fn stream.pull
      [self]
      (source:pull))

    (fn stream.subscribe
      [self on-value]
      (table.insert self.subscribers on-value))

    (source:subscribe
     (fn [value]
       (impl.transform stream value #(stream:push $1) #(stream:end))))

    stream))

(local  source-stream {})

(fn source-stream.new
  [generator]
  (let [stream {:subscribers []
                :ended       false}]
    (fn stream.end
      [self]
      (if self.ended
          (error "Stream: Cannot close an already ended stream")
          (do (tset self :ended true)
              true)))

    (fn stream.push
      [self value]
      (each [_i subscriber (ipairs self.subscribers)]
        (subscriber value))
      self)

    (fn stream.pull
      [self]
      (generator
       #(self:push $1)
       #(self:end)))

    (fn stream.subscribe
      [self on-value]
      (table.insert self.subscribers on-value)
      (fn []
        (print "Unsubscribe")))

    stream))

(fn from-list
  [list-tbl]
  (source-stream.new
   (fn
     [push end]
     (each [_i value (ipairs list-tbl)]
       (push value))
     (end))))

(fn map
  [source map-f]
  (transform-stream.new
   source
   {:transform (fn [self value push end]
                 (push (map-f value)))
    :end       (fn [self push end done]
                 (end)
                 (done))}))


(fn filter
  [source pred]
  (transform-stream.new
   source
   {:transform (fn [self value push end]
                 (when (pred value)
                   (push value)))
    :end       (fn [self push end]
                 (end))}))

(fn subscribe
  [source on-value]
  (let [unsubscribe (source:subscribe on-value)]
    (source:pull)
    unsubscribe))

{: from-list
 : map
 : filter
 : subscribe}
