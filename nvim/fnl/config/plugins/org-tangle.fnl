(local {:core c :string s} (require :config.utils))
(local query vim.treesitter.query)
(local {: parse &as parsers} (require :config.parsers))

(fn min-spaces-len
  [str min]
  (c.reduce
    (fn [min whitespace-chunk]
      (let [len (length whitespace-chunk)]
       (if (> min len)
         len
         min)))
    min
    (string.gmatch str "\n([ \t]+)")))

(fn fix-indentation
  [str min-spaces]
  (let [min-spaces (min-spaces-len str (or min-spaces 1000))
        pattern (.. "\n" (string.rep "[ \t]" min-spaces))]
    (string.gsub str pattern "\n")))

(comment
  ;; dbg
  (each [x (string.gmatch "\n\t     text" "\n([ \t]+)")]
    (print "out" (.. "\"" x "\"")))
  (string.gmatch "\n\t     text" "\n([ \t]+)")
  ( (string.gmatch "\n\t     test" "\n([ \t]+)"))
  (min-spaces-len
   "\n\t   test"
   3)
  (fix-indentation
    "\n\t   test"
    4))

(fn process-block
  [node {: cur-file : files}]
  (comment
   (when cur-file
     (when (not (. files cur-file))
       (tset files cur-file {}))
     (each [block-prop (node:iter_children)]
       (when (= (block-prop:type) "contents")
         (let [(_ col) (block-prop:range)]
           (table.insert files cur-file (fix-indentation (query.get_node_text block-prop 0) col))))))))

(local header-arg-lang
  (parsers.seq
    (parsers.alpha)
    (parsers.maybe (parsers.drop (parsers.char ":")))))

(local header-arg-pair
  (parsers.seq
    (parsers.drop (parsers.char ":"))
    (parsers.alpha)
    (parsers.whitespace)
    (parsers.concat
     (parsers.many
       (parsers.not
         (parsers.char " "))))
    (parsers.maybe (parsers.whitespace))))


(local header-arg-pairs
  (parsers.xf
   (parsers.many
     (parsers.xf
       header-arg-pair
       (fn [result]
         (tset result :output [result.output])
         result)))
   (fn [result]
      (tset result :output [result.output])
      result)))

(local header-args-parser
  (parsers.xf
   (parsers.seq
     (parsers.maybe
       (parsers.drop (parsers.lit "header-args:")))
     (parsers.or
       (parsers.concat (parsers.seq header-arg-lang (parsers.whitespace)))
       (parsers.always "*"))
     header-arg-pairs)
   (fn [result]
     (let [[lang keypairs] result.output]
       {: lang
        :props (c.pairs->tbl keypairs)}))))

(fn parse-header-args
  [header-args-txt]
  (let [header-args (parse header-args-parser header-args-txt)]
    header-args))

(fn process-directive
  [directive-node tangle-state]
  (accumulate
    [state {:props []
            :prop-name ""}
     prop-part (directive-node:iter_children)]
    (let [prop-type (prop-part:type)
          prop-text (query.get_node_text prop-part 0)]
      (print (fennel.view {: prop-type : prop-text}))
      (if
        (and (= prop-type :expr) (or (= prop-text "property")
                                     (= prop-text "tangle")))
        (tset state :prop-name prop-text)

        (and (= prop-type :value) (s.starts-with? prop-text "header-args"))
        (table.insert state.props {state.prop-name (parse-header-args prop-text)}))


      state)))

(fn parse-drawer-props
  [drawer-child]
  (accumulate
    [state {:props []
            :prop-name   ""}
     prop-part (drawer-child:iter_children)]

    (let [prop-type (prop-part:type)
          prop-text (query.get_node_text prop-part 0)]
     (if
       (= prop-type :expr)
       (tset state :prop-name prop-text)

       (and (= prop-type :value) (= state.prop-name :header-args))
       (table.insert state.props {state.prop-name (parse-header-args prop-text)}))

     state)))

(fn process-property-drawer
  [node tangle-state]
  (->> (node:iter_children)
       (c.filter
         (fn [drawer-child]
           (= (drawer-child:type) :property)))
       (c.each
         (fn [drawer-child]
           (let [drawer-props (parse-drawer-props drawer-child)]
             (print (fennel.view drawer-props))
             drawer-props))))
  tangle-state)

(fn process-node
  [node tangle-state]
  (when node
    (each [subnode (node:iter_children)]
      (let [node-type (subnode:type)]
        (if
          (= node-type :block)
          (process-block subnode tangle-state)

          (= node-type :directive)
          (print (fennel.view (process-directive subnode tangle-state)))

          (or (= node-type :drawer) (= node-type :property_drawer))
          (print (vim.inspect (process-property-drawer subnode tangle-state)))

          (process-node subnode tangle-state)))))
  tangle-state)

(fn save-files
  [files]
  (print "files:" (vim.inspect files)))

(fn tangle
  []
  (let [tangle-state {:files []
                      :cur-file ""}
        lang-tree (vim.treesitter.get_parser 0)
        syntax-tree (lang-tree:parse)
        root (: (. syntax-tree 1) :root)
        {: files} (process-node root tangle-state)]
    (save-files files)))

(vim.api.nvim_create_autocmd
  "FileType"
  {:pattern "org"
   :callback (fn []
               (comment
                 (vim.api.nvim_buf_set_keymap
                   0 :n :<cr> ""
                   {:callback toggle-org-item
                    :noremap true}))
               (vim.api.nvim_buf_set_keymap
                 0 :n :<leader>oxt "<cmd>Tangle<cr>"
                 {:desc "tangle"
                  :noremap true}))})

(vim.api.nvim_create_user_command
  :Tangle
  #(let [tangle (require :config.plugins.org-tangle)]
     (tangle.tangle))
  {})

{: tangle
 : header-args-parser}
