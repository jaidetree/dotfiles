" Needed on some linux distros.
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" BUNDLES
Bundle "othree/html5.vim"
Bundle "vim-scripts/Colour-Sampler-Pack"
Bundle "vim-scripts/ScrollColors"
Bundle "https://github.com/kien/ctrlp.vim.git"
Bundle "https://github.com/ddollar/nerdcommenter.git"
Bundle "https://github.com/majutsushi/tagbar.git"
Bundle "https://github.com/vim-scripts/ZoomWin.git"
Bundle "https://github.com/jeetsukumaran/vim-buffergator.git"

Bundle "comment.vim"

set t_Co=256

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
    set mouse=a
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")
    filetype plugin indent on
endif

" Jay's Edits
scriptencoding utf-8

" SETTINGS
" GUI Options
set ruler		" show the cursor position all the time
set relativenumber
set guioptions-=T
set showcmd		" display incomplete commands
set showmode
set lazyredraw
set gdefault
let loaded_matchparen = 1
set hidden
set laststatus=2
let g:closetag_html_style=1

" Spacing
set linespace=2
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smarttab
set autoindent
set copyindent

" Fold Settings
set foldenable
set foldmethod=indent
set foldminlines=3

" Cursor Settings
set guicursor=n-v-c:block-Cursor-blinkon0,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor,r-cr:hor20-Cursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
set backspace=indent,eol,start
set virtualedit=all

set history=50		" keep 50 lines of command line history
set shortmess+=filmnrxoOtT

" Search Settings
set ignorecase
set infercase
set incsearch		
set wildmenu
set showfulltag
set noshowmatch

" No backups or swapfiles
set noswapfile
set nobackup
set nowb

" Change command settings
set cpoptions+=$
set ch=2

" KEY MAPS
inoremap <C-U> <C-G>u<C-U>
nmap <Up> <Nop>
nmap <Down> <Nop>
nmap <Left> <Nop>
nmap <Right> <Nop>
nnoremap <silent> <Esc><Esc> :nohls<CR><CR>
map ,cd :cd %:p:h<CR>
noremap <silent> ,s :syntax sync fromstart<CR>
nnoremap j gj
nnoremap k gk


" Makes parts of an_underscored_word a seperate word.
" set isk-=_

if has('statusline')
    set stl=%f\ %m\ Line:%l/%L[%p%%]\ Col:%v\ Buf:#%n\ [%b][0x%B]
endif

" Auto Commands
autocmd BufEnter * silent! %foldopen!

cd ~/Projects/

" Themes
" colo freya
" colo wombat
" colo earendel 
" colo kellys
" colo mustang
" colo lucius
"colo slate2
"colo tir_black
"colo summerfruit256
"colo dusk
"color darkbone
"color denim
color fu
