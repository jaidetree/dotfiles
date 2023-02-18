(local {:core c :string s} (require :config.utils))
(local query vim.treesitter.query)
(local {: parse &as parsers} (require :config.parsers))

;; Example: [[file:tmux.org::*Window Status][Window Status:1]]
(local comment-begin-format "[[file:%s::*%s][%s:%d]]")
;; Example: Window Status:1 ends here
(local comment-end-format "%s:%d ends here")

(local  comment-forms
  {:slashes   "//  %s"
   :dashes    "-- %s"
   :hash      "# %s"
   :percent   "%% %s"
   :semicolon ";; %s"
   :html      "<!-- %s -->"
   :jsx       "{/* %s */}"})

(local comment-langs
  [[[:c :c++ :c# :js :javascript :ts :typescript :rescript :rust :go] :slashes]
   [[:sql :pgsql :psql :mssql :haskell :elm] :dashes]
   [[:bash :fish :sh :zsh :shell :elvish :elixir :ash :conf] :hash]
   [[:jsx :tsx] :jsx]
   [[:html] :html]
   [[:lisp :clojure :cl :common-lisp :scheme :guile :guix :fennel] :semicolon]
   [[:erlang] :percent]])

(fn get-comment-format
  [block-lang tangle-file]
  (accumulate
    [format-str comment-forms.slashes
     _i [langs format-key] (ipairs comment-langs)]
    (if (vim.tbl_contains langs block-lang)
      (. comment-forms format-key)
      format-str)))

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

(comment
  (vim.fn.expand "%")
  (vim.fn.expand "%:p:.")
  (vim.fn.fnamemodify "/Users/j/dotfiles/ntmux/tmux.org" ":."))

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
     :filename  (vim.fn.expand resolved.tangle)
     :filepath  (vim.fn.resolve (.. tangle-state.context.dir "/" resolved.tangle))}))

(fn format-comment
  [{: file : headline : idx : line : lang}]
  (let [format (get-comment-format lang file)]
    [(->> (string.format comment-begin-format file headline headline idx)
          (string.format format))
     (->> (string.format comment-end-format headline idx)
          (string.format format))]))

(fn tangle-block
  [tangle-state {: line : node : conf}]
  (let [{: files : headline} tangle-state
        {: filename : filepath } conf
        mode (if (. tangle-state.files filename) :a+ :w)
        (_ col) (node:range)
        text (fix-indentation (query.get_node_text node 0) col)
        format-str (get-comment-format conf.lang conf.file)]
    ;; Used to replace the original file contents on first block for target
    ;; file
    (when (= mode :w)
      (tset tangle-state.files filename {headline 1}))

    (let [{: files : headline} tangle-state
          sections (. files filename)]
      (if (. sections headline)
        (c.update sections headline c.inc)
        (tset sections headline 1)))

    ;; @TODO Deal with mkdirp true
    ;; @TODO Nested sections should not replace parent's header-args outside of
    ;; section
    (with-open [fout (io.open filepath mode)]
      (let [[begin-comment end-comment] (format-comment
                                          {:lang     conf.lang
                                           :file     filename
                                           :headline headline
                                           :idx      (. files filename headline)
                                           :line     line})]
        (fout:write (.. begin-comment "\n"
                        text "\n"
                        end-comment "\n\n"))))))


(fn find-child
  [predicate? node]
  (accumulate
    [target nil
     child (node:iter_children) &until target]
    (if (predicate? child)
      child
      nil)))

(fn find-child-by-type
  [type-str node]
  (find-child #(= ($1:type) type-str) node))

(fn process-headline
  [tangle-state node]
  (let [item (find-child-by-type :item node)
        text (query.get_node_text item 0)]
    (set tangle-state.headline text)))

(fn process-block
  [tangle-state node]
  (let [block-meta (parse-lang-block (query.get_node_text node 0))]
   (when block-meta.ok
     (let [(row _col _count) (node:start)
           contents-node (find-child-by-type :contents node)
           conf (resolve-conf tangle-state block-meta)
           context {:line row
                    :conf conf
                    :node contents-node}]
       (when (and contents-node conf.props.tangle (not= conf.props.tangle :none))
         (tangle-block tangle-state context))))))

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
      (print (.. "process-node\n" (fennel.view tangle-state.conf)))
      (let [node-type (subnode:type)]
        (if
          (= node-type :headline)
          (process-headline tangle-state subnode)

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

(fn tangle
  []
  ;; @TODO Get filename and path of current org doc
  (let [lang-tree (vim.treesitter.get_parser 0)
        bufnr     (lang-tree:source)
        filename (vim.fn.expand "%:p")
        dir (vim.fs.dirname filename)
        tangle-state {:files {}
                      :conf {}
                      :path []
                      :context {: bufnr
                                : dir
                                : filename}}
        syntax-tree (lang-tree:parse)
        root (: (. syntax-tree 1) :root)
        tangle-state (process-node tangle-state root)]
    (print "Tangled" (c.count tangle-state.files) "files" (fennel.view (vim.tbl_keys tangle-state.files)))))


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
