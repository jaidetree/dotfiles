" Needed on some linux distros.
set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin('~/.vim/plugged')

" Plugins
Plug 'ajh17/VimCompletesMe'
Plug 'bakpakin/fennel.vim'
Plug 'bhurlow/vim-parinfer'
Plug 'briancollins/vim-jst'
Plug 'cakebaker/scss-syntax.vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'ddollar/nerdcommenter'
Plug 'digitaltoad/vim-jade'
Plug 'editorconfig/editorconfig-vim'
Plug 'guns/vim-sexp'
Plug 'hail2u/vim-css3-syntax'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'johnallen3d/made-of-code.vim'
Plug 'kchmck/vim-coffee-script'
Plug 'luochen1990/rainbow'
Plug 'majutsushi/tagbar'
Plug 'mileszs/ack.vim'
Plug 'msanders/snipmate.vim'
Plug 'mustache/vim-mustache-handlebars'
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/Colour-Sampler-Pack'
Plug 'vim-scripts/ScrollColors'
Plug 'vim-scripts/ZoomWin'
Plug 'w0rp/ale'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
" Plug 'scrooloose/nerdtree'
" Plug 'https://github.com/scrooloose/syntastic'
call plug#end()

set t_Co=256
syntax on

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
set ruler   " show the cursor position all the time
set relativenumber
set guioptions-=T
set showcmd   " display incomplete commands
set showmode
set lazyredraw
set gdefault
let loaded_matchparen = 1
set hidden
set laststatus=2
let g:closetag_html_style=1

" Grep settings
set grepprg=ack\ -k

" Spacing
set linespace=2
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smarttab
set autoindent
" set copyindent

" Fold Settings
set foldenable
set foldmethod=indent
set foldminlines=3

" Cursor Settings
set guicursor=n-v-c:block-Cursor-blinkon0,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor,r-cr:hor20-Cursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
set backspace=indent,eol,start
set virtualedit=all

set history=50    " keep 50 lines of command line history
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
set pastetoggle=<F2>

" Make vim faster in terminal
set lazyredraw
set ttyfast
set shell=/bin/bash\ -i

" JavaScript Settings
let g:javascript_plugin_jsdoc = 1

" Mac settings
" set clipboard=unnamed

" KEY MAPS
let mapleader = "\<space>"
let maplocalleader="\<space>"

iabbrev </ </<C-X><C-O>
inoremap <C-U> <C-G>u<C-U>
imap <D-V> ^O"+p

nmap <Up> <Nop>
nmap <Down> <Nop>
nmap <Left> <Nop>
nmap <Right> <Nop>

map ,cd :cd %:p:h<CR>
map <C-k>b :NERDTreeToggle<CR>

nnoremap <silent> <Esc><Esc> :nohls<CR><CR>
nnoremap j gj
nnoremap k gk
nnoremap <Up> <C-W>-
nnoremap <Down> <C-W>+
nnoremap <Left> <C-W><
nnoremap <Right> <C-W>>
nnoremap <silent> <C-t> :Tex<CR>
nnoremap <silent> <C-j><S-l> :Vexplore!<CR>
nnoremap <silent> <C-j><S-h> :Vexplore<CR>
nnoremap <silent> <C-j><S-j> :Hexplore<CR>
nnoremap <silent> <C-j><S-k> :Hexplore!<CR>
nnoremap <silent> <C-c><C-c> :<C-U>call <SID>printop(v:count)<CR>

" noremap <Leader>c "+y
noremap <silent> ,s :syntax sync fromstart<CR>
noremap <silent><Leader>pf :FZF<CR>
noremap <silent><Leader>fed :e ~/.vimrc<CR>
noremap <silent><Leader>feR :source ~/.vimrc<CR>
noremap <silent><Leader>wh <C-w>h
noremap <silent><Leader>wl <C-w>l
noremap <silent><Leader>wj <C-w>j
noremap <silent><Leader>wk <C-w>k
noremap <silent><Leader>w/ :vsplit!<CR><C-w>l
noremap <silent><Leader>w- :split!<CR><C-w>j
noremap <silent><Leader>wd <C-w>q
noremap <silent><Leader>wx :q<CR>
noremap <silent><Leader>tn :set rnu!<CR>
noremap <silent> <S-s> :w<CR>
noremap <silent> <C-s> :w<CR>

" Makes parts of an_underscored_word a seperate word.
" set isk-=_

if has('statusline')
    set stl=%f\ %m\ Line:%l/%L[%p%%]\ Col:%v\ Buf:#%n\ [%b][0x%B]
endif

" Auto Commands
autocmd BufRead,BufEnter * silent! %foldopen!

" PLUGIN SETTINGS
let g:rainbow_active = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_sign_column_always = 1

cd %:p:h

" Themes
" colo freya
" colo wombat
" colo earendel
" colo kellys
" colo mustang
" colo lucius
" colo slate2
" colo tir_black
" colo summerfruit256
" colo dusk
" color darkbone
" color denim
" color fu
" color rdark
" color railscasts
" color made-of-code
" color blacksea
color gentooish
" color lucius
color moss

" Ruler settings
highlight OverLength ctermbg=black guibg=#892929
match OverLength /\%79v.\+/
