"
" Mappings and functions are allowed to clobber the z mark or buffer.
"

" TODO: diff
" TODO: close on last exit
" TODO: ag for fzf?
" TODO: cvim?
" TODO: replace f with sneak
" TODO: replace e with scrolling?


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.config/nvim/plugged')

" Plug 'benekastah/neomake'
" Plug 'AndrewRadev/sideways.vim'
Plug '~/.config/nvim/my_plugged/cdc-bufferline'
Plug 'ciaranm/inkpot'
Plug 'LaTeX-Box-Team/LaTeX-Box', {'for': 'tex'}
" Plug 'jalvesaq/vimcmdline'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" Plug 'junegunn/vim-easy-align'
Plug 'justinmk/vim-sneak'
" Plug 'kassio/neoterm'
" Plug 'lervag/vimtex'
Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
Plug 'mhinz/vim-grepper'
Plug 'majutsushi/tagbar', {'on' : 'TagbarToggle'}
" Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
" Plug 'pgdouyon/vim-accio'
" Plug 'Shougo/deoplete.nvim'
" Plug 'terryma/vim-expand-region'
" Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
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
let g:sneak#absolute_dir = 1

let g:surround_{char2nr('m')} = "\\(\r\\)"
let g:surround_{char2nr('n')} = "\\[\r\n\\]"

let g:LatexBox_no_mappings = 1
let g:LatexBox_custom_indent = 0
let g:LatexBox_split_side = 'belowright'

" Get rid of annoying & indenting in LaTeX
let g:tex_indent_and = 0

" LaTeX syntax highlighting customization
let g:tex_items = '\\bibitem\|\\item\|\\plr\|\\prl\|\\case\|\\lit\|\\pss\|'
let g:tex_items .= '\\psps\|\\pip\|\\piff'
let g:tex_itemize_env = 'itemize\|description\|enumerate\|thebibliography\|'
let g:tex_itemize_env .= 'caselist'

let g:grepper = {'switch':0}

let g:undotree_WindowLayout = 3
let g:undotree_SetFocusWhenToggle = 1

set diffopt+=iwhite
set diffopt+=foldcolumn:0
set diffopt+=context:20
if &diff
    set foldminlines=99999
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Autocommands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


augroup skeleton_files
    autocmd!

    autocmd BufNewFile *.pl
      \ 0r ${HOME}/.vim/skeleton.pl |
      \ normal G

    autocmd BufNewFile *.tex
      \ 0r ${HOME}/.vim/skeleton.tex |
      \ normal! Gdd6k$
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
    " HACK: pending https://github.com/neovim/neovim/pull/4296
    autocmd TermClose * call CloseTerminal()
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

function! CloseTerminal()
    if exists('b:terminal_job_pid')
        call feedkeys("l\<BS>")
    else
        only
    endif
endfunction

function! SwitchBuffer(buffer_command)
    if v:count
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

function! ToggleNumber()
    if (&number == 1 || &relativenumber == 1)
        set nonumber
        set norelativenumber
    else
        set number
        set relativenumber
    endif
endfunction

function! SelectedLines()
    return join(getline(line("'<"), line("'>")), "\n")
endfunction



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


" Non-leader mappings
nnoremap / /\v
nnoremap ? ?\v
nnoremap <Tab> %
nnoremap G m
nnoremap x <C-b>
nnoremap m <C-f>

vnoremap < <gv
vnoremap > >gv

nnoremap <expr> n 'Nn'[v:searchforward]
nnoremap <expr> N 'nN'[v:searchforward]

vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

let g:semiforward = 1
nnoremap <silent> f :let g:semiforward=1<CR>f
nnoremap <silent> F :let g:semiforward=0<CR>F
nnoremap <silent> t :let g:semiforward=1<CR>t
nnoremap <silent> T :let g:semiforward=0<CR>T

nnoremap ) 0
nnoremap 0 )
nnoremap ( 9
nnoremap 9 (
nnoremap * 8
nnoremap 8 *
nnoremap & 7
nnoremap 7 &
nnoremap ^ 6
nnoremap 6 ^

vnoremap ) 0
vnoremap 0 )
vnoremap ( 9
vnoremap 9 (
vnoremap * 8
vnoremap 8 *
vnoremap & 7
vnoremap 7 &
vnoremap ^ 6
vnoremap 6 ^

onoremap ) 0
onoremap 0 )
onoremap ( 9
onoremap 9 (
onoremap * 8
onoremap 8 *
onoremap & 7
onoremap 7 &
onoremap ^ 6
onoremap 6 ^

tnoremap ,, <C-\><C-n>
tnoremap <Space>; <C-\><C-n>

vnoremap <silent> Q gq
vnoremap / /\v
vnoremap ? ?\v
vnoremap <Tab> %
vnoremap x <C-b>
vnoremap m <C-f>

onoremap <Tab> %
onoremap x <C-b>
onoremap m <C-f>

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

nnoremap <Leader> <NOP>
vnoremap <Leader> <NOP>

nnoremap <LocalLeader> <NOP>
vnoremap <LocalLeader> <NOP>

" Navigation and editing
nnoremap <Leader>f <NOP>
vnoremap <Leader>f <NOP>

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

" vimrc and other config
nnoremap <Leader>v <NOP>
vnoremap <Leader>v <NOP>

nnoremap <silent> <Leader>ve :edit $MYVIMRC<CR>
nnoremap <silent> <Leader>vs :source $MYVIMRC \| filetype detect<CR>
nnoremap <silent> <Leader>vk :edit $HOME/.config/nvim/skeleton.%:e<CR>
nnoremap <silent> <Leader>va
    \ :edit $HOME/.config/nvim/after/ftplugin/<C-r>=&filetype<CR>.vim<CR>

" Settings and plugins
nnoremap <Leader>s <NOP>
vnoremap <Leader>s <NOP>

nnoremap <silent> <Leader>sh :nohlsearch<CR>
nnoremap <silent> <Leader>sn :call ToggleNumber()<CR>
nnoremap <silent> <Leader>sd :filetype detect<CR>
nnoremap <silent> <Leader>su :UndotreeToggle<CR>
nnoremap <silent> <Leader>st :TagbarToggle<CR>
nnoremap <silent> <Leader>ss :syntax sync fromstart<CR>
nnoremap <leader>sm :<C-u><C-r><C-r>='let @'. v:register .' = '. string(getreg(v:register))<CR><C-f><left>
nnoremap <Leader>sg
    \ :Grepper -tool ag -grepprg ag --vimgrep $* <C-r>=GetBufCwd()<CR><CR>
" TODO: allow specifying the directory
" TODO: figure out how to get it to take a motion and use GetBufCwd

" TODO: ctrl-k?
" Buffers
nnoremap <Leader>k <NOP>
vnoremap <Leader>k <NOP>

nnoremap <silent> <Leader>kl :<C-u>call SwitchBuffer('buffer')<CR>
nnoremap <silent> <Leader>ks :<C-u>call SwitchBuffer('sbuffer')<CR>
nnoremap <silent> <Leader>kv :<C-u>call SwitchBuffer('vertical sbuffer')<CR>
nnoremap <Leader>kf <C-^>
nnoremap <silent> <Leader>kd :bdelete<CR>
nnoremap <silent> <Leader>kj :<C-u>call StepBuffer(1)<CR>
nnoremap <silent> <Leader>kk :<C-u>call StepBuffer(-1)<CR>

" Writing, quitting, opening terminals, and formatting
nnoremap <Leader>j <NOP>
vnoremap <Leader>j <NOP>

nnoremap <silent> <Leader>jf :write<CR>
nnoremap <silent> <Leader>jw :wall<CR>
nnoremap <silent> <Leader>je :qall<CR>
nnoremap <silent> <Leader>jc :call SpawnShell('enew')<CR>
nnoremap <silent> <Leader>js :call SpawnShell('new')<CR>
nnoremap <silent> <Leader>jv :call SpawnShell('vnew')<CR>
nnoremap <Leader>jo :edit <C-r>=GetBufCwd()<CR>/<C-f>a
nnoremap <Leader>jk gq

" Window mapping
nnoremap <Leader>d <C-w>
nnoremap <Leader>dd <C-w><C-w>

" Miscellaneous top-level leader mappings
nnoremap <Leader>; q:i
vnoremap <Leader>; q:i

nnoremap <Leader>, gg
nnoremap <Leader>. G
onoremap <Leader>, gg
onoremap <Leader>. G
vnoremap <Leader>, gg
vnoremap <Leader>. G


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


command! DiffOrig vertical new | set bt=nofile | r # | 0d_ | diffthis
      \ | wincmd p | diffthis

command! -nargs=0 Sexe execute SelectedLines()
