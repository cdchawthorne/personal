"
" Mappings and functions are allowed to clobber the z mark or buffer.
"

" TODO: close on last exit
" TODO: shiftwidth 2?
" TODO: async grep
" TODO: dwm better mappings for switching layouts
" TODO: swap s and m?


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.config/nvim/plugged')

Plug '~/.config/nvim/my_plugged/cdc-bufferline'

Plug 'ciaranm/inkpot'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'lervag/vimtex', {'for': 'tex'}
Plug 'majutsushi/tagbar', {'on' : 'TagbarToggle'}
Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

Plug 'AndrewRadev/sideways.vim'
Plug 'justinmk/vim-dirvish'
Plug 'justinmk/vim-sneak'

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

call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Options
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


syntax on
filetype plugin indent on

" Builtin options
set backup
set backupdir=${HOME}/utilities/vim_backups
set colorcolumn=80
set cursorline
set expandtab
set nofoldenable
set hidden
set ignorecase
set matchtime=1
set mouse=
set nomodeline
set number
set relativenumber
set ruler
" set shada=!,'1000,<50,s10,h
set shiftwidth=4
set showcmd
set showmatch
set smartcase
set softtabstop=4
set spelllang=en_ca
set splitbelow
set splitright
set tabstop=4
set tags=${HOME}/utilities/databases/tags
set tildeop
set timeoutlen=1000
set undofile
set undodir=${HOME}/utilities/vim_undo

let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1

let g:terminal_scrollback_buffer_size = 100000

let g:inkpot_black_background=1
colorscheme inkpot
highlight ColorColumn ctermbg=60

let g:tex_flavor = "latex"

let g:gundo_right = 1

let g:tagbar_width = 28
let g:tagbar_sort = 0
let g:tagbar_compact = 1

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

set grepprg=ag\ --vimgrep\ $*
set grepformat=%f:%l:%c:%m

set diffopt+=iwhite
set diffopt+=foldcolumn:0
set diffopt+=context:1000000


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Autocommands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


augroup skeleton_files
    autocmd!

    autocmd BufNewFile *.tex
      \ 0r ${HOME}/.config/nvim/skeleton.tex | normal! Gdd6k$
augroup END

augroup restore_cursor_pos
    autocmd!

    autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
augroup END

augroup terminal_autocmds
    autocmd!

    autocmd TermOpen * setlocal nocursorline
    autocmd TermClose * execute 'bdelete! ' . expand('<abuf>')
augroup END

augroup cursorline
    autocmd!

    autocmd WinEnter    * set cursorline
    autocmd WinLeave    * set nocursorline
    autocmd InsertEnter * set nocursorline
    autocmd InsertLeave * set cursorline
augroup END


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


function! GetBufCwd()
    if exists("b:terminal_job_pid")
        let dir = system(["realpath",  "/proc/" . b:terminal_job_pid . "/cwd"])
        " Get rid of trailing newline
        return dir[0:-2]
    else
        return expand("%:p:h")
    endif
endfunction

function! SpawnShell(layout_cmd)
    let nvim_cwd = getcwd()
    execute 'cd ' . GetBufCwd()
    silent execute a:layout_cmd
    call termopen([$SHELL])
    execute 'cd ' . nvim_cwd
    startinsert
endfunction

function! SwitchBuffer(buffer_command)
    if v:count && v:count <= len(g:cbl_bufnummap)
        execute a:buffer_command . ' ' . g:cbl_bufnummap[v:count-1]
    else
        buffers
        call feedkeys(':' . a:buffer_command . ' ')
    endif
endfunction

function! StepBuffer(multiplier)
    let steps = (v:count ? v:count : 1) * a:multiplier
    let idx = (index(g:cbl_bufnummap, bufnr('%')) + steps)
    execute 'buffer ' . g:cbl_bufnummap[idx % len(g:cbl_bufnummap)]
endfunction

function! SelectedLines()
    return join(getline(line("'<"), line("'>")), "\n")
endfunction



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


" Non-leader mappings
noremap / /\v
noremap ? ?\v
noremap <Tab> %
noremap G m
noremap v <C-b>
noremap m <C-f>

nnoremap <silent> <Up> :.m-2<CR>
nnoremap <silent> <Down> :.m+1<CR>

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

inoremap fdn <C-n>
inoremap fdp <C-p>
inoremap fdk <C-k>
inoremap fds <C-g>u<C-]><Esc>gqgqA
inoremap fdh <Esc>:nohlsearch<CR>a
inoremap fdd <C-g>u<C-R>=strftime("%Y-%m-%d")<CR>

" Leader mappings
let mapleader = "\<Space>"
let maplocalleader = mapleader . "l"

noremap <Leader> <NOP>

noremap <LocalLeader> <NOP>

" Navigation and editing
noremap <Leader>f <NOP>

nnoremap <silent> <Leader>fk :Files<CR>
nnoremap <Leader>fi :Files 
nnoremap <silent> <Leader>fb :Buffers<CR>
nnoremap <silent> <Leader>fl :Lines<CR>
nnoremap <silent> <Leader>fo :execute 'Files ' . GetBufCwd()<CR>
nnoremap <silent> <Leader>ff :History<CR>
nnoremap <silent> <Leader>fh :Helptags<CR>
nnoremap <silent> <Leader>f; :History:<CR>
nnoremap <silent> <Leader>f/ :History/<CR>
nnoremap <silent> <Leader>f? :History?<CR>
nnoremap <silent> <Leader>fs :new<CR>:Files<CR>
nnoremap <silent> <Leader>fv :vnew<CR>:Files<CR>

" config files
noremap <Leader>r <NOP>

nnoremap <silent> <Leader>re :edit $MYVIMRC<CR>
nnoremap <silent> <Leader>rs :source $MYVIMRC \| filetype detect<CR>
nnoremap <silent> <Leader>rk :edit $HOME/.config/nvim/skeleton.%:e<CR>
nnoremap <silent> <Leader>ra
    \ :edit $HOME/.config/nvim/after/ftplugin/<C-r>=&filetype<CR>.vim<CR>

" Settings and one-off plugins
noremap <Leader>s <NOP>

nnoremap <silent> <Leader>sh :nohlsearch<CR>
nnoremap <silent> <Leader>sd :windo diffoff<CR>:bdelete<CR>
nnoremap <silent> <Leader>su :UndotreeToggle<CR>
nnoremap <silent> <Leader>sn :set number! \| set relativenumber!<CR>
nnoremap <silent> <Leader>st :TagbarToggle<CR>
nnoremap <silent> <Leader>ss :syntax sync fromstart<CR>
nnoremap <leader>sm :<C-u><C-r><C-r>='let @'. v:register .' = '. string(getreg(v:register))<CR><C-f><left>

" Buffers
noremap <Leader>k <NOP>

nnoremap <silent> <Leader>kl :<C-u>call SwitchBuffer('buffer')<CR>
nnoremap <silent> <Leader>ks :<C-u>call SwitchBuffer('sbuffer')<CR>
nnoremap <silent> <Leader>kv :<C-u>call SwitchBuffer('vertical sbuffer')<CR>
nnoremap <Leader>kf <C-^>
nnoremap <silent> <Leader>kc :bdelete<CR>
nnoremap <silent> <Leader>kd :bprevious \| bdelete #<CR>
nnoremap <silent> <Leader>kj :<C-u>call StepBuffer(1)<CR>
nnoremap <silent> <Leader>kk :<C-u>call StepBuffer(-1)<CR>

" Writing, quitting, opening terminals, and formatting
noremap <Leader>j <NOP>

nnoremap <silent> <Leader>jf :write<CR>
nnoremap <silent> <Leader>jw :wall<CR>
nnoremap <silent> <Leader>je :qall<CR>
nnoremap <silent> <Leader>jE :qall!<CR>
nnoremap <silent> <Leader>jc :call SpawnShell('enew')<CR>
nnoremap <silent> <Leader>js :call SpawnShell('new')<CR>
nnoremap <silent> <Leader>jv :call SpawnShell('vnew')<CR>
nnoremap <Leader>jo :edit <C-r>=GetBufCwd()<CR>/<C-f>a
nnoremap <Leader>jk gq

vnoremap <Leader>jk gq

" Sideways
nnoremap <silent> <Leader>ah :SidewaysLeft<CR>
nnoremap <silent> <Leader>al :SidewaysRight<CR>

" Window mapping
noremap <Leader>d <C-w>
noremap <Leader>dd <C-w><C-w>

" Miscellaneous top-level leader mappings
nnoremap <Leader>; q:i
vnoremap <Leader>; q:i

noremap <Leader>, gg
noremap <Leader>. G
noremap <Leader>v v


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


command! DiffOrig vertical new | set buftype=nofile | read # | 0delete_
        \ | diffthis | wincmd p | diffthis

command! -nargs=0 Sexe execute SelectedLines()
