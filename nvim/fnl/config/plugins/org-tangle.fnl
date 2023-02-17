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

(local header-arg-lang
  (parsers.seq
    (parsers.alpha)
    (parsers.maybe (parsers.drop (parsers.char ":")))))

(fn resolve-header-arg-value
  [[k v]]
  (if
    (= v "yes")
    [k true]

    (= v "no")
    [k false]

    [k v]))

(local header-arg-pair
  (parsers.seq
    (parsers.drop (parsers.char ":"))
    (parsers.alpha)
    (parsers.whitespace)
    (parsers.concat
     (parsers.many
       (parsers.not
         (parsers.or
          (parsers.char " ")
          (parsers.char "\n")))))
    (parsers.maybe (parsers.whitespace))))


(local header-arg-pairs
  (parsers.xf
   (parsers.many
     (parsers.xf
       header-arg-pair
       (fn [result]
         (tset result :output [(resolve-header-arg-value result.output)])
         result)))
   (fn [result]
      (tset result :output [(c.pairs->tbl result.output)])
      result)))

(local header-args-prefix
  (parsers.and
    (parsers.lit "header-args")
    (parsers.or (parsers.char ":")
                (parsers.whitespace))))

(local header-args-parser
  (parsers.xf
   (parsers.seq
     (parsers.maybe
       (parsers.drop header-args-prefix))
     (parsers.or
       (parsers.concat (parsers.seq header-arg-lang (parsers.whitespace)))
       (parsers.always "*"))
     header-arg-pairs)
   (fn [result]
     (let [[lang props] result.output]
       {: lang
        : props}))))

(local block-lang-parser
  (parsers.xf
   (parsers.seq
     (parsers.drop (parsers.lit "#+begin_src"))
     (parsers.whitespace)
     (parsers.concat
      (parsers.many (parsers.not (parsers.or
                                   (parsers.char " ")
                                   (parsers.char "\n")))))
     (parsers.maybe
       (parsers.seq
        (parsers.whitespace)
        header-arg-pairs)))
   (fn [results]
     (let [[lang props] results.output]
       {:ok true
        : lang
        : props}))))

(fn parse-lang-block
  [block-text]
  (let [block-meta (parse block-lang-parser block-text)]
    block-meta))


(comment
  (parsers.parse
    block-lang-parser
    "#+begin_src conf\n;; content")
  (parsers.parse
    block-lang-parser
    "#+begin_src conf :tangle test.conf :results none\n;; content"))

(fn resolve-conf
  [tangle-state {: lang :props block-props}]
  (let [block-props (or block-props {})
        shared-props (. tangle-state.conf :*)
        file-props (. tangle-state.conf lang)
        resolved (c.merge shared-props file-props block-props)]
    {:props resolved
     :lang  lang
     :file  (vim.fn.expand resolved.tangle)}))


(fn process-block
  [tangle-state node]
  (let [block-meta (parse-lang-block (query.get_node_text node 0))]
   (when block-meta.ok
    (each [block-prop (node:iter_children)]
       (let [block-type (block-prop:type)]
           (when (= block-type :contents)
             (let [(_ col) (block-prop:range)
                     conf (resolve-conf tangle-state block-meta)
                     mode (if (. tangle-state.files conf.file) :a+ :w)
                     text (fix-indentation (query.get_node_text block-prop 0) col)]
               (when (= mode :w)
                 (tset tangle-state.files conf.file true))
               (with-open [fout (io.open conf.file mode)]
                 (fout:write text)))))))))

(comment
 (when cur-file
   (when (not (. files cur-file))
     (tset files cur-file {}))
   (each [block-prop (node:iter_children)]
     (when (= (block-prop:type) "contents")
       (let [(_ col) (block-prop:range)]
         ()
         (table.insert files cur-file (fix-indentation (query.get_node_text block-prop 0) col)))))))


(fn parse-header-args
  [header-args-txt]
  (let [header-args (parse header-args-parser header-args-txt)]
    header-args))

(fn process-directive
  [directive-node]
  (accumulate
    [state []
     prop-part (directive-node:iter_children)]
    (let [prop-type (prop-part:type)
          prop-text (query.get_node_text prop-part 0)]
      (when (and (= prop-type :value)
                 (s.starts-with? prop-text "header-args"))
        (table.insert state (parse-header-args prop-text)))
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
       (table.insert state.props (parse-header-args prop-text)))
     state)))

(fn process-property-drawer
  [node]
  (accumulate
    [state []
     drawer-child (node:iter_children)]
    (do
     (when (= (drawer-child:type) :property)
       (table.insert state  (parse-drawer-props drawer-child)))
     state)))


(fn update-conf
  [tangle-state header-args-list]
  (each [_ header-args (ipairs header-args-list)]
    (if (not (. tangle-state.conf header-args.lang))
      (tset tangle-state.conf header-args.lang header-args.props)
      (let [lang (. tangle-state.conf header-args.lang)]
        (c.update tangle-state.conf header-args.lang c.merge header-args.props)))))


(fn process-node
  [tangle-state node]
  (when node
    (each [subnode (node:iter_children)]
      (let [node-type (subnode:type)]
        (if
          (= node-type :block)
          (process-block tangle-state subnode)

          (= node-type :directive)
          (->> subnode
              (process-directive)
              (update-conf tangle-state))

          (or (= node-type :drawer) (= node-type :property_drawer))
          (let [prop-drawer (process-property-drawer subnode)]
            (each [_ prop (ipairs prop-drawer)]
              (when (= prop.prop-name :header-args)
               (update-conf tangle-state prop.props))))

          (process-node tangle-state subnode)))))
  tangle-state)

(fn save-files
  [files]
  (print "files:" (vim.inspect files)))

(fn tangle
  []
  (let [tangle-state {:files {}
                      :conf {}
                      :cur-file ""}
        lang-tree (vim.treesitter.get_parser 0)
        syntax-tree (lang-tree:parse)
        root (: (. syntax-tree 1) :root)
        tangle-state (process-node tangle-state root)]
    (print (fennel.view tangle-state))
    (save-files tangle-state.files)))

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
