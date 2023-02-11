(local {:core c :string s} (require :config.utils))
(local query vim.treesitter.query)

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

(fn drawer-prop
  [drawer-child prop-name]
  (->> (drawer-child:iter_children)
       (c.reduce
         (fn [{: is-tangle : cur-file} prop-part]
           (let [prop-type (prop-part:type)
                 prop-text (query.get_node_text prop-part 0)]
            (if
              (and (= prop-type :expr) (= prop-text prop-name))
              {:is-tangle true : cur-file}

              (and (= prop-type :value) is-tangle)
              (:is-tangle false :cur-file prop-text)

              {: is-tangle : cur-file}))
          {:is-tangle false :cur-file ""}))
       (c.prop :cur-file)))



 ;; TODO: This can't process header-args. So it needs to support
 ;; header-args:{lang} props

;; {
;;   ["prop-text"] = "header-args",
;;   ["prop-type"] = "expr")}

;; {
;;   ["prop-text"] = ":",
;;   ["prop-type"] = ":"}

;; {
;;   ["prop-text"] = "conf: :tangle base.conf",
;;   ["prop-type"] = "value"}

;; {
;;   ["cur-file"] = "",
;;   files = {}}

(fn parse-header-args
  [header-args-txt]
  nil)

(fn parse-drawer-props
  [drawer-child]
  (accumulate
    [state {:props {}
            :prop-name   ""}
     prop-part (drawer-child:iter_children)]

    (let [prop-type (prop-part:type)
          prop-text (query.get_node_text prop-part 0)]
     (if
       (= prop-type :expr)
       (tset state :prop-name prop-text)

       (and (= prop-type :value) (= state.prop-name :header-args))
       (tset state.props state.prop-name (parse-header-args prop-text))

       (= prop-type :value)
       (tset state.props state.prop-name prop-text))
     state)))

(fn process-property-drawer
  [node tangle-state]
  (->> (node:iter_children)
       (c.filter
         (fn [drawer-child]
           (= (drawer-child:type) :property)))
       (c.each
         (fn [drawer-child]
           (let [cur-file (drawer-prop drawer-child "TANGLE")]
             (when cur-file
               (tset tangle-state :cur-file cur-file))))))
  tangle-state)

(fn process-node
  [node tangle-state]
  (when node
    (each [subnode (node:iter_children)]
      (let [node-type (subnode:type)]
        (if
          (= node-type :block)
          (process-block subnode tangle-state)

          (= node-type :property_drawer)
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
                 0 :n :<leader>oxt ""
                 {:callback #(let [tangle (require :config.plugins.org-tangle)]
                              (tangle.tangle))
                  :desc "tangle"
                  :noremap true}))})

(vim.api.nvim_create_user_command
  :Tangle
  #(let [tangle (require :config.plugins.org-tangle)]
     (tangle.tangle))
  {})

{: tangle}
