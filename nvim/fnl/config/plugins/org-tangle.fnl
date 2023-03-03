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

(fn strip-trailing-slash
  [path]
  (let [last-char (string.sub path -1 -1)]
    (if (= last-char "/")
      (string.sub path 1 -2)
      path)))

(fn split-paths
  [path-str]
  (-> path-str
      (vim.fs.normalize)
      (vim.fn.resolve)
      (vim.split "/" {:plain true})))

(fn relative
  [path-a path-b]
  (let [tree-a (split-paths path-a)
        len-a (length tree-a)
        tree-b (split-paths path-b)
        len-b (length tree-b)
        min-idx (math.min len-a len-b)
        i (faccumulate
            [common-paths 1
             i 1 min-idx
             &until (not= (. tree-a i) (. tree-b i))]
            (+ common-paths 1))]
    (->
      (..
        (string.rep "../" (- len-a i))
        (table.concat tree-b "/" i))
      (strip-trailing-slash))))

(comment
  (relative
    "/Users/j/dotfiles/tmux/tmux.org"
    "/Users/j/dotfiles/tmux/tmux.conf")
  (relative
    "/Users/j/dotfiles/tmux/tmux.org"
    "/Users/j/dotfiles/install.sh")
  (relative
    "/Users/j/dayjob-express/shadow-cljs.edn"
    "/Users/j/dayjob-express/docs/cljs/readme.org")
  (relative
    "/Users/j/dotfiles/tmux/tmux.conf"
    "/Users/j/dotfiles/tmux/tmux.org")
  (relative
    "/Users/j/dayjob-express/shadow-cljs.edn"
    "/Users/j/dayjob-express/docs/cljs/readme.org"))



(fn min-spaces-len
  [str max]
  (->> (c.map
         (fn [whitespace-chunk]
          whitespace-chunk)
         (string.gmatch str "\n([ \t]+)"))
       (c.reduce
         (fn [prev-max chunk]
           (let [chunk-len (length chunk)]
             (if (>= prev-max chunk-len)
               chunk-len
               prev-max)))
         max)))



(fn fix-indentation
  [str min-spaces]
  (let [len (min-spaces-len str (or min-spaces 1000))
        head-pattern (.. "^" (string.rep "[ \t]" len))
        rest-pattern (.. "\n" (string.rep "[ \t]" len))]
    (-> str
        (string.gsub head-pattern "")
        (string.gsub rest-pattern "\n"))))


(comment
  ;; dbg
  (icollect
   [x (string.gmatch
        "dependencies:
      | reagent
  promesa"
        "\n([ \t]+)")]
   x)

  (min-spaces-len
    "
   dependencies:
   | reagent
     promesa
   "
    3)
  (fix-indentation
    "  dependencies:
        | reagent
          promesa\n"
    3)
  nil)

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
    (parsers.take-until (parsers.any-char " \t"))
    (parsers.drop (parsers.many (parsers.any-char " \t")))
    (parsers.take-until
      (parsers.any-char " \n"))
    (parsers.maybe (parsers.char " "))))

(comment

  (parsers.parse
    header-arg-pair
    ":tangle base.conf\n"))


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
  (parsers.seq
    (parsers.maybe (parsers.whitespace))
    (parsers.drop (parsers.lit "#+begin_src"))
    (parsers.drop (parsers.many (parsers.any-char " \t")))
    (parsers.take-until (parsers.whitespace))))


(fn parse-lang-block
  [block-text]
  (let [block-meta (parse block-lang-parser block-text)]
    block-meta))

(local block-header-args-parser
  (parsers.seq
    (parsers.drop (parsers.take-until (parsers.any-char "\n:")))
    header-arg-pairs))

(local block-body-parser
  (parsers.between
    (parsers.char "\n")
    (parsers.and (parsers.whitespace)
                 (parsers.lit "#+end_src"))))

(local block-parser
  (parsers.xf
   (parsers.seq
     block-lang-parser
     (parsers.or block-header-args-parser
               (parsers.always {}))
     block-body-parser)
   (fn [result]
     (let [[lang props body] result.output]
       {:ok true
        : lang
        : props
        : body}))))

(fn parse-block
  [block-text]
  (let [block (parse block-parser block-text)]
    block))


(comment
  (parsers.parse
    block-lang-parser
    "#+begin_src conf\n;; content")
  (parsers.parse
    block-lang-parser
    "#+begin_src vim\n:vert Bufferize nmap")
  (parsers.parse
    block-lang-parser
    "#+begin_src conf :tangle test.conf :results none\n;; content")

  (parsers.parse
    block-parser
    "   #+begin_src clojure
         :dependencies
         [[reagent \"1.2.0\"]
          [promesa \"1.50.0\"]
        #+end_src"))

(fn merge-confs
  [tangle-state {: lang : block-props}]
  (->> tangle-state.conf
       (c.filter #(<= $.level tangle-state.level))
       (c.map
         (fn [{:props conf}]
           (let [shared-props (or (. conf :*) {})
                 file-props   (or (. conf lang) {})]
             (c.merge shared-props file-props))))
       (c.reduce
         (fn [resolved conf]
           (c.merge resolved conf))
         (or block-props {}))))

(fn resolve-conf
  [tangle-state {: lang :props block-props}]
  (let [conf (merge-confs tangle-state {: lang : block-props})]
    (if conf.tangle
     {:props conf
      :lang lang
      :filename (vim.fn.expand conf.tangle)
      :filepath (vim.fn.resolve (.. tangle-state.context.dir "/" conf.tangle))}
     nil)))

(fn format-comment
  [{: file : headline : idx : line : lang}]
  (let [format (get-comment-format lang file)]
    [(->> (string.format comment-begin-format file headline headline idx)
          (string.format format))
     (->> (string.format comment-end-format headline idx)
          (string.format format))]))

(fn format-file
  [src dest]
  (relative src dest))

(fn tangle-block
  [tangle-state {: block : node : conf}]
  (let [{: files : headline} tangle-state
        {: filename : filepath } conf
        mode (if (. files filename) :a+ :w)
        (line _col _count) (node:start)
        (_ col) (: (node:parent) :range)
        text (fix-indentation block.body col)
        format-str (get-comment-format conf.lang conf.file)]
    ;; Create initial file entry to store block counts in headlines
    (when (= mode :w)
      (tset tangle-state.files filename {}))

    ;; Set or increment the src block index for current headline
    (let [sections (. files filename)]
      (when (not (. sections headline))
        (tset sections headline 0))
      (c.update sections headline c.inc))

    ;; Create the directory tree if mkdirp is true
    (when (and (= mode :w) conf.props.mkdirp)
      (let [dir tangle-state.context.dir]
        (vim.fn.mkdir dir "p")))

    ;; Write block text to target tangle file
    (with-open [fout (io.open filepath mode)]
      (let [[begin-comment end-comment] (format-comment
                                          {:lang     conf.lang
                                           :file     (format-file
                                                       filepath
                                                       tangle-state.context.filename)
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
        stars (find-child-by-type :stars node)
        title (query.get_node_text item 0)
        level (length (query.get_node_text stars 0))]
    (when (<= level tangle-state.level)
      (set tangle-state.conf
           (->> tangle-state.conf
                (c.filter
                  #(< $1.level level)))))
    (set tangle-state.headline title)
    (set tangle-state.level level)))

(fn process-block
  [tangle-state {: node : text}]
  (let [text (query.get_node_text node 0)
        block (or (parse-block text) {})
        contents-node (find-child-by-type :contents node)
        conf (if block.ok (resolve-conf tangle-state block) nil)]
    (when (and block.ok contents-node conf conf.props.tangle (not= conf.props.tangle :none))
      (tangle-block tangle-state
                    {:block block
                     :conf conf
                     :node contents-node}))))

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
    (let [lang (. tangle-state.conf header-args.lang)]
        (table.insert tangle-state.conf {:level tangle-state.level
                                         :props {header-args.lang header-args.props}}))))


(fn process-node
  [tangle-state node]
  (when node
    (each [subnode (node:iter_children)]
      (let [node-type (subnode:type)]
        (if
          (= node-type :headline)
          (process-headline tangle-state subnode)

          (= node-type :block)
          (let [block-text (query.get_node_text subnode 0)]
            (when (string.gmatch block-text "^#%+begin_src")
              (process-block tangle-state {:node subnode
                                           :text block-text})))

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
  (let [lang-tree (vim.treesitter.get_parser 0)
        bufnr     (lang-tree:source)
        filename (vim.fn.expand "%:p")
        dir (vim.fs.dirname filename)
        tangle-state {:level 0
                      :conf []
                      :files {}
                      :context {: bufnr
                                : dir
                                : filename}}
        syntax-tree (lang-tree:parse)
        root (: (. syntax-tree 1) :root)
        tangle-state (process-node tangle-state root)]
    (print "Tangled" (c.count tangle-state.files) "files" (fennel.view (vim.tbl_keys tangle-state.files)))))


(vim.api.nvim_create_user_command
  :Tangle
  #(let [tangle (require :config.plugins.org-tangle)]
     (tangle.tangle))
  {})

{: tangle
 : block-lang-parser
 : block-header-args-parser
 : header-arg-pair
 : header-arg-pairs
 : header-args-parser
 : header-arg-lang
 : block-parser}
