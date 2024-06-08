"
" Mappings and functions are allowed to clobber the z mark or register.
"

" TODO: maybe disable tbufferline by default?
" TODO: use guicursor
" TODO: async grep
" TODO: set up an autocmd that checks the current mode and sets StatusLine and
"       StatusLineNC accordingly
" TODO: vim-surround appears to be disabling some <leader> operator mappings
" TODO: why did my cursor stop working?
" TODO: change i3 leader?
" TODO: use tmux c-k to switch buffers?


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.config/nvim/plugged')

Plug 'cdchawthorne/nvim-tbufferline'
Plug 'ciaranm/inkpot'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'majutsushi/tagbar', {'on' : 'TagbarToggle'}
Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

"Plug 'scrooloose/nerdtree'
Plug 'AndrewRadev/sideways.vim'
"Plug 'justinmk/vim-dirvish'
Plug 'justinmk/vim-sneak'
Plug 'luochen1990/rainbow'
" Plug 'frazrepo/vim-rainbow'
Plug 'wlangstroth/vim-racket', {'for': 'racket'}
" Plug 'benknoble/vim-racket'
Plug 'rust-lang/rust.vim'
" Plug 'nvim-tree/nvim-web-devicons'
" Plug 'nvim-tree/nvim-tree.lua'

" Plug 'bluz71/vim-nightfly-guicolors'
" Plug 'bluz71/vim-moonfly-colors'
" Plug 'haishanh/night-owl.vim'
" Plug 'neg-serg/neg'
" Plug 'tomasr/molokai'
" Plug 'chriskempson/base16-vim'

" Plug 'benekastah/neomake'
" Plug 'jalvesaq/vimcmdline'
" Plug 'junegunn/vim-easy-align'
" Plug 'kassio/neoterm'
" Plug 'mhinz/vim-grepper', {'on': 'Grepper'}
" Plug 'pgdouyon/vim-accio'
" Plug 'Shougo/deoplete.nvim'
" Plug 'terryma/vim-expand-region'
" Plug 'tpope/vim-abolish'
" Plug 'vim-utils/vim-man'
" Plug 'machakann/vim-highlightedyank'

call plug#end()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Options
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if empty($XDG_DATA_HOME)
  let $XDG_DATA_HOME=$HOME . '/.local/share'
endif

" Builtin options
set backup
set backupdir=$XDG_DATA_HOME/nvim/backup
set breakat=\ 
set colorcolumn=+1
set expandtab
set nofoldenable
set guicursor=n-v-c-sm:block,i-ci-ve-r-cr-o:hor20,a:blinkon500-blinkoff500
set hidden
set ignorecase
set inccommand=split
set laststatus=3
set matchtime=1
set mouse=
set nomodeline
set number
set relativenumber
set ruler
set shiftwidth=2
set showcmd
set showmatch
set smartcase
set softtabstop=2
set spelllang=en_ca
set splitbelow
set splitright
set tabstop=2
set tags=$XDG_DATA_HOME/nvim/tags
set tildeop
set timeoutlen=1000
set undofile
set undodir=$XDG_DATA_HOME/nvim/undo

let g:terminal_scrollback_buffer_size = 100000

let g:inkpot_black_background=1
set notermguicolors
colorscheme inkpot
highlight ColorColumn ctermbg=60

let g:tex_flavor = "latex"

let g:tbufferline_enable_on_startup = 1

let g:sneak#use_ic_scs = 1

let g:surround_{char2nr('m')} = "\\(\r\\)"
let g:surround_{char2nr('n')} = "\\[\r\n\\]"

let g:vimtex_index_hide_line_numbers = 0
let g:vimtex_index_split_pos = 'vert rightbelow'
let g:vimtex_indent_enabled = 0
let g:vimtex_imaps_enabled = 0

" Get rid of annoying & indenting in LaTeX
let g:tex_indent_and = 0

" LaTeX syntax highlighting customization
let g:tex_items = '\\bibitem\|\\item\|\\plr\|\\prl\|\\case\|\\lit\|\\pss\|'
let g:tex_items .= '\\psps\|\\pip\|\\piff'
let g:tex_itemize_env = 'itemize\|description\|enumerate\|thebibliography\|'
let g:tex_itemize_env .= 'caselist'

let g:undotree_WindowLayout = 3
let g:undotree_SetFocusWhenToggle = 1

let g:rainbow_active = 1

let g:tagbar_map_hidenonpublic = ''
let g:tagbar_map_togglesort = 'S'
let g:tagbar_foldlevel = 0
let g:tagbar_width = 39

let NERDTreeWinSize = 27

set grepprg=ag\ --vimgrep\ $*
set grepformat=%f:%l:%c:%m

set diffopt+=iwhite
set diffopt+=foldcolumn:0
set diffopt+=context:1000000

" nvim-tree shenanigans

" lua << EOF
  " -- disable netrw at the very start of your init.lua
  " vim.g.loaded_netrw = 1
  " vim.g.loaded_netrwPlugin = 1

  " -- set termguicolors to enable highlight groups
  " -- vim.opt.termguicolors = true

  " -- empty setup using defaults
  " require("nvim-tree").setup()
" EOF


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Autocommands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


augroup skeleton_files
  autocmd!

  autocmd BufNewFile *.tex
      \ 0read ${HOME}/.config/nvim/skeleton.tex | normal! Gdd6k$
  autocmd BufNewFile *.js
      \ 0read ${HOME}/.config/nvim/skeleton.js | normal! G
augroup END

augroup restore_cursor_pos
  autocmd!

  autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$")
      \|  execute "normal! g`\""
      \|endif
augroup END

augroup terminal_autocmds
  autocmd!

  autocmd TermOpen * setlocal nonumber norelativenumber
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! GetBufCwd() abort
  if exists("b:terminal_job_pid")
    let dir = system(["realpath",  "/proc/" . b:terminal_job_pid . "/cwd"])
    " Get rid of trailing newline
    return dir[0:-2]
  else
    return expand("%:p:h")
  endif
endfunction

function! SpawnShell(layout_cmd) abort
  let cwd = GetBufCwd()
  " TODO: escape spaces and stuff
  silent execute a:layout_cmd
  call termopen([$SHELL], {'cwd': cwd})
  startinsert
endfunction

function! SelectedLines() abort
  return join(getline(line("'<"), line("'>")), "\n")
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


" Non-leader mappings

noremap / /\v
noremap ? ?\v
noremap <Tab> %
nmap <Space> <Plug>tbufferline#Buffer
nnoremap S m
nnoremap <silent> x :write<CR>
nnoremap q xp
nnoremap Q q
nmap t <Plug>Sneak_s
nmap T <Plug>Sneak_S
vmap t <Plug>Sneak_s
vmap T <Plug>Sneak_S
noremap v <C-b>
noremap m <C-f>

nnoremap <silent> <Up> <Cmd>.m-2<CR>
nnoremap <silent> <Down> <Cmd>.m+1<CR>
nnoremap <silent> <Left> <Cmd>SidewaysLeft<CR>
nnoremap <silent> <Right> <Cmd>SidewaysRight<CR>


omap aa <Plug>SidewaysArgumentTextobjA
xmap aa <Plug>SidewaysArgumentTextobjA
omap ia <Plug>SidewaysArgumentTextobjI
xmap ia <Plug>SidewaysArgumentTextobjI

vnoremap < <gv
vnoremap > >gv

noremap ) 0
noremap 0 )
noremap ( 9
noremap 9 (
noremap * 8
noremap 8 *
noremap & 7
noremap 7 &
noremap ^ 6
noremap 6 ^

tnoremap ,. <C-\><C-n>

inoremap fj <C-]><Esc>
inoremap Fj <C-]><Esc>
inoremap fJ <C-]><Esc>
inoremap FJ <C-]><Esc>
inoremap jf <C-]><Esc>
inoremap Jf <C-]><Esc>
inoremap jF <C-]><Esc>
inoremap JF <C-]><Esc>
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> NO

inoremap fdo <C-o>
inoremap fdn <C-n>
inoremap fdp <C-p>
inoremap fdu <C-k>
inoremap fd. <C-]><Esc>>>A
inoremap fd, <C-]><Esc><<A
inoremap fdh <C-]><Cmd>nohlsearch<CR>
inoremap fdd <C-g>u<C-R>=strftime("%Y-%m-%d")<CR>
inoremap fds <C-g>u<C-]><Esc>gqgqA<Space>

" Leader mappings
let mapleader = "s"
let maplocalleader = mapleader . "l"

noremap <Leader> <NOP>

noremap <LocalLeader> <NOP>

" fzf
noremap <Leader>f <NOP>

nnoremap <silent> <Leader>fk <Cmd>Files<CR>
nnoremap <silent> <Leader>fb <Cmd>Buffers<CR>
nnoremap <silent> <Leader>fl <Cmd>Lines<CR>
nnoremap <silent> <Leader>fo <Cmd>execute 'Files ' . GetBufCwd()<CR>
nnoremap <silent> <Leader>fh <Cmd>History<CR>
nnoremap <silent> <Leader>fm <Cmd>Helptags<CR>
nnoremap <silent> <Leader>f; <Cmd>History:<CR>
nnoremap <silent> <Leader>f/ <Cmd>History/<CR>
nnoremap <silent> <Leader>f? <Cmd>History?<CR>
nnoremap <silent> <Leader>fs <Cmd>new<CR>:Files<CR>
nnoremap <silent> <Leader>fv <Cmd>vnew<CR>:Files<CR>

" config files
noremap <Leader>r <NOP>

nnoremap <silent> <Leader>re <Cmd>edit $MYVIMRC<CR>
nnoremap <silent> <Leader>rs <Cmd>source $MYVIMRC \| filetype detect<CR>
nnoremap <silent> <Leader>rk <Cmd>edit $HOME/.config/nvim/skeleton.%:e<CR>
nnoremap <silent> <Leader>ra
    \ :edit $HOME/.config/nvim/after/ftplugin/<C-r>=&filetype<CR>.vim<CR>

" Settings and one-off plugins
noremap <Leader>s <NOP>

nnoremap <silent> <Leader>sh <Cmd>nohlsearch<CR>
nnoremap <silent> <Leader>sd <Cmd>diffoff!<CR>:bdelete<CR>:bdelete<CR>
nnoremap <silent> <Leader>su <Cmd>UndotreeToggle<CR>
nnoremap <silent> <Leader>sn <Cmd>set number! \| set relativenumber!<CR>
nnoremap <silent> <Leader>ss <Cmd>syntax sync fromstart<CR>
nnoremap <silent> <Leader>st <Cmd>TagbarToggle<CR>
nnoremap <silent> <Leader>sf <Cmd>NERDTreeToggle mediary-system<CR>
nnoremap <leader>sm :<C-u><C-r><C-r>='let @'. v:register .' = '.
    \ string(getreg(v:register))<CR><C-f><left>

" Buffers
noremap <Leader>k <NOP>
noremap <Leader>ks <NOP>
noremap <Leader>kv <NOP>

nnoremap <silent> <Leader>kc <Cmd>call SpawnShell('enew')<CR>
nnoremap <Leader>kf <C-^>
nmap <silent> <Leader>ksl <Plug>tbufferline#SplitBuffer
nnoremap <silent> <Leader>ksc <Cmd>call SpawnShell('new')<CR>
nnoremap <silent> <Leader>ksf <Cmd>sbuffer #<CR>
nmap <silent> <Leader>kvl <Plug>tbufferline#VSplitBuffer
nnoremap <silent> <Leader>kvc <Cmd>call SpawnShell('vnew')<CR>
nnoremap <silent> <Leader>kvf <Cmd>vertical sbuffer #<CR>
nnoremap <silent> <Leader>kd <Cmd>bdelete<CR>
nmap <silent> <Leader>kj <Plug>tbufferline#StepForward
nmap <silent> <Leader>kk <Plug>tbufferline#StepBack
nnoremap <Leader>ko :edit <C-r>=GetBufCwd()<CR>/<C-f>a

" Window mapping
noremap <Leader>d <C-w>
noremap <Leader>dd <C-w><C-w>

" Miscellaneous top-level leader mappings
nnoremap <Leader>j gq
vnoremap <Leader>j gq

nnoremap <Leader>; q:i
vnoremap <Leader>; q:i

nnoremap <silent> <Leader>a :qa<CR>

nnoremap <Leader>. G
nnoremap <Leader>, gg
noremap <Leader>v v
nnoremap <Leader>q q


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Commands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


command! DiffOrig vertical new | set buftype=nofile | read # | 0delete_
    \| diffthis | wincmd p | diffthis

command! -nargs=0 Sexe execute SelectedLines()
