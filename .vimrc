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

filetype plugin indent on
syntax enable

hi ColorColumn ctermbg=236
hi CursorLine ctermbg=233
hi Comment ctermfg=cyan cterm=italic
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

Plug 'akinsho/bufferline.nvim'
Plug 'nvim-tree/nvim-web-devicons'

" Use release branch (recommended)
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
