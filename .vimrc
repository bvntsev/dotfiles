set background=dark
set number
set relativenumber
set cursorline
set clipboard=unnamedplus
set shiftwidth=4
set tabstop=4
set expandtab
set smartcase
set ignorecase
set incsearch
set hlsearch
set wrap
set scrolloff=5
set showmatch
set history=1000
set wildmenu
set wildmode=longest:full,full
set colorcolumn=80

set laststatus=2
set hidden

set nobackup
set nowritebackup
set noswapfile

" if &term =~ 'xterm\|kitty\|alacritty\|st\|tmux'
"   let &t_SI = "\e[6 q"  " Insert Mode: bar
"   let &t_SR = "\e[4 q"  " Replace Mode: underline
"   let &t_EI = "\e[2 q"  " Normal Mode: block
" endif

set ve+=onemore
set termguicolors

filetype plugin indent on
syntax enable

hi CursorLine ctermbg=233
hi Comment ctermfg=cyan cterm=italic
hi ColorColumn guibg='Gray'
hi Function ctermfg=6
hi String ctermfg=magenta
hi Search ctermfg=black ctermbg=yellow
hi IncSearch ctermfg=white ctermbg=red

" leader key
let mapleader=" "
" nnoremap <leader>e :NERDTree<CR>
nnoremap <leader>d :bd<CR>
nnoremap <leader>t :enew<CR>
nnoremap <leader><Tab> :bnext<CR>
nnoremap <leader>`     :bprev<CR>
nnoremap <leader>f     :FZF<CR>
nnoremap <silent> gd <Plug>(coc-definition)

call plug#begin('~/.vim/plugged')
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree'

Plug 'tpope/vim-eunuch'             " basic unix command in vim
Plug 'tomtom/tcomment_vim'          " mininal commenter
Plug 'cacharle/vim-syntax-extra'    " syntax highlight of C operators
Plug 'tpope/vim-fugitive'           " git wrapper

Plug 'akinsho/bufferline.nvim'
Plug 'nvim-tree/nvim-web-devicons'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()

let g:airline#extensions#tabline#enabled = 1


function! ToggleNERDTree()
  if exists("t:NERDTreeBufName") && bufwinnr(t:NERDTreeBufName) != -1
    execute bufwinnr(t:NERDTreeBufName) . "wincmd c"
  else
    NERDTreeToggle
  endif
endfunction

nnoremap <leader>e :call ToggleNERDTree()<CR>

" nnoremap <leader>w :w<CR>
" nnoremap <leader>q :q<CR>
