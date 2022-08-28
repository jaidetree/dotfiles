;; https://git.sr.ht/~gagbo/doom-config/tree/master/item/modules/completion/corfu/packages.el

(package! corfu)
(when (featurep! +orderless)
  (package! orderless))
(package! kind-icon)
(package! cape :recipe (:host github :repo "minad/cape" :branch "main"))
(package! corfu-doc :recipe (:host github :repo "galeo/corfu-doc" :branch "main"))
