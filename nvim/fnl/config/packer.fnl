"
Automatically configure packer
https://github.com/wbthomason/packer.nvim#quickstart
"

(fn ensure-packer
 []
 (let [vimfn vim.fn
       install_path (.. (vimfn.stdpath "data") "/site/pack/packer/start/packer.nvim")]
   (if (< 0 (vimfn.empty (vimfn.glob install_path)))
     (do
       (vimfn.system ["git" "clone" "--depth" "1" "https://github.com/wbthomason/packer.nvim" install_path])
       (vim.cmd "packadd packer.nvim")
       true)
     false)))

(local packer-bootstrap (ensure-packer))
(local packer (require :packer))

{:startup packer.startup
 :sync    packer.sync
 : packer
 :bootstrap packer-bootstrap}
