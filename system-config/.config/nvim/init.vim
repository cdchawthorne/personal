"
" Mappings and functions are allowed to clobber the z mark or buffer.
"

" TODO: diff


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.config/nvim/plugged')

" Plug 'benekastah/neomake'
" Plug 'bling/vim-bufferline'
Plug '~/.config/nvim/my_plugged/vim-bufferline'
Plug 'ciaranm/inkpot'
Plug 'LaTeX-Box-Team/LaTeX-Box', {'for': 'tex'}
" Plug 'jalvesaq/vimcmdline'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" Plug 'junegunn/vim-easy-align'
Plug 'majutsushi/tagbar'
" Plug 'mhinz/vim-grepper'
" Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
" Plug 'pgdouyon/vim-accio'
Plug 'scrooloose/nerdcommenter'
" Plug 'Shougo/deoplete.nvim'
Plug 'sjl/gundo.vim', {'on': 'GundoToggle'}
" Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
" Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'
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

" Colours
let g:inkpot_black_background=1
colorscheme inkpot
highlight ColorColumn ctermbg=60

" Alternatives: badwolf, dark, raven, lucius, laederon
" let g:airline_theme='badwolf'

let g:bufferline_echo = 0
augroup BufferLine
    autocmd VimEnter *
    \ let &statusline='%{bufferline#refresh_status()}'
    \ .bufferline#get_status_string()
augroup END

let g:tex_flavor="latex"

" Gundo options
let g:gundo_right = 1

" NERDCommenter options
let NERDSpaceDelims=1

" Tagbar options
let g:tagbar_width=28
let g:tagbar_sort=0
let g:tagbar_compact=1

let g:terminal_scrollback_buffer_size = 100000
"
" vim-surround options
" Keys mapped are m, M, n, and N, respectively
let g:surround_109 = "\\(\r\\)"
let g:surround_77 = "\\( \r \\)"
let g:surround_110 = "\\[\r\\]"
let g:surround_78 = "\\[ \r \\]"

" LaTeX Box options
let g:LatexBox_no_mappings = 1
let g:LatexBox_custom_indent = 0
let g:LatexBox_split_side='belowright'

" Get rid of annoying & indenting in LaTeX
let g:tex_indent_and = 0

" LaTeX syntax highlighting customization
let g:tex_items='\\bibitem\|\\item\|\\plr\|\\prl\|\\case\|\\lit\|\\pss\|'
let g:tex_items.='\\psps\|\\pip\|\\piff'
let g:tex_itemize_env='itemize\|description\|enumerate\|thebibliography\|'
let g:tex_itemize_env.='caselist'

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

    " TODO: autocmd BufLeave * if exists('b:terminal_job_pid') update_name
    " Probably process name if not zsh, else cwd
    " Or not
    autocmd TermOpen * setlocal nocursorline
    " Hack pending https://github.com/neovim/neovim/pull/4296
    autocmd TermClose * call CloseTerminal()
    autocmd BufEnter zsh[0-9]\\\{1,2\} startinsert
augroup END


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


if !exists('g:shell_num')
    let g:shell_num = 1
endif

function! SpawnShell(layout_cmd)
    if exists("b:terminal_job_pid")
        let dir = system(["realpath",  "/proc/" . b:terminal_job_pid . "/cwd"])
        " Get rid of trailing newline
        let dir = dir[0:-2]
    else
        let dir = expand("%:p:h")
    endif
    let nvim_cwd = getcwd()
    execute 'cd ' . dir
    silent execute a:layout_cmd
    call termopen([$SHELL])
    execute 'cd ' . nvim_cwd
    execute "file zsh" . g:shell_num
    let g:shell_num += 1
    startinsert
endfunction

function! OpenFromCwd()
    if exists("b:terminal_job_pid")
        let dir = system(["realpath",  "/proc/" . b:terminal_job_pid . "/cwd"])
        " Get rid of trailing newline
        let dir = dir[0:-2]
    else
        let dir = expand("%:p:h")
    endif
    execute 'Files ' . dir
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
        execute a:buffer_command . ' ' . v:count
    else
        buffers
        call feedkeys(':' . a:buffer_command . ' ')
    endif
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
nnoremap S m
nnoremap m <C-b>
nnoremap <Space> <C-f>

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

tnoremap <C-k> <C-\><C-n>

vnoremap <silent> Q gq
vnoremap / /\v
vnoremap ? ?\v
vnoremap <Tab> %
vnoremap m <C-b>
vnoremap <Space> <C-f>

onoremap <Tab> %
onoremap m <C-b>
onoremap <Space> <C-f>

inoremap fj <C-]><Esc>
inoremap Fj <C-]><Esc>
inoremap fJ <C-]><Esc>
inoremap FJ <C-]><Esc>
inoremap jf <C-]><Esc>
inoremap Jf <C-]><Esc>
inoremap jF <C-]><Esc>
inoremap JF <C-]><Esc>
inoremap <C-u> <C-g>u<C-u>

inoremap fdk <C-k>
inoremap fdu <C-g>u<C-u>
inoremap fdn <C-n>
inoremap fdp <C-p>
inoremap fds <C-g>u<C-]><Esc>gqgqA
inoremap fdw <C-w>
inoremap fdh <Esc>:nohlsearch<CR>a
inoremap fdd <C-g>u<C-R>=strftime("%Y-%m-%d")<CR>

" Leader mappings
let mapleader = "s"
let maplocalleader = "sl"

nnoremap <Leader> <NOP>
vnoremap <Leader> <NOP>

nnoremap <LocalLeader> <NOP>
vnoremap <LocalLeader> <NOP>

" Navigation and editing
nnoremap <Leader>f <NOP>
vnoremap <Leader>f <NOP>

nnoremap <silent> <Leader>fo :Files<CR>
nnoremap <Leader>fi :Files 
nnoremap <silent> <Leader>fb :Buffers<CR>
nnoremap <silent> <Leader>fl :Lines<CR>
nnoremap <silent> <Leader>fw :Lines<CR>
nnoremap <silent> <Leader>ff :History<CR>
nnoremap <silent> <Leader>fh :Helptags<CR>
nnoremap <silent> <Leader>f; :History:<CR>
nnoremap <silent> <Leader>f/ :History/<CR>
nnoremap <silent> <Leader>fc :Commands<CR>

" vimrc and other config
nnoremap <Leader>v <NOP>
vnoremap <Leader>v <NOP>

nnoremap <silent> <Leader>ve :edit $MYVIMRC<CR>
nnoremap <silent> <Leader>vs :source $MYVIMRC \| filetype detect<CR>
nnoremap <silent> <Leader>vk :edit $HOME/.config/nvim/skeleton.%:e<CR>
nnoremap <silent> <Leader>va
    \ :edit $HOME/.config/nvim/after/ftplugin/<C-R>=&filetype<CR>.vim<CR>

" Settings and plugins
nnoremap <Leader>s <NOP>
vnoremap <Leader>s <NOP>

nnoremap <silent> <Leader>sh :nohlsearch<CR>
nnoremap <silent> <Leader>sn :call ToggleNumber()<CR>
nnoremap <silent> <Leader>sd :filetype detect<CR>
nnoremap <silent> <Leader>su :GundoToggle<CR>
nnoremap <silent> <Leader>sU :GundoToggle<CR>:GundoToggle<CR>
nnoremap <silent> <Leader>st :TagbarToggle<CR>
nnoremap <silent> <Leader>sT :TagbarToggle<CR>:TagbarToggle<CR>
nnoremap <silent> <Leader>sf :NERDTreeToggle<CR>
nnoremap <silent> <Leader>sF :NERDTreeToggle<CR>:NERDTreeToggle<CR>
nnoremap <silent> <Leader>ss :syntax sync fromstart<CR>

" Buffers
nnoremap <Leader>k <NOP>
vnoremap <Leader>k <NOP>

nnoremap <silent> <Leader>kl :<C-u>call SwitchBuffer('buffer')<CR>
nnoremap <silent> <Leader>ks :<C-u>call SwitchBuffer('sbuffer')<CR>
nnoremap <silent> <Leader>kv :<C-u>call SwitchBuffer('vertical sbuffer')<CR>
nnoremap <Leader>kf <C-^>
nnoremap <silent> <Leader>kd :bdelete<CR>
nnoremap <silent> <Leader>kn :bnext<CR>
nnoremap <silent> <Leader>kp :bprevious<CR>

" Writing, quitting, opening terminals, and formatting
nnoremap <Leader>j <NOP>
vnoremap <Leader>j <NOP>

nnoremap <silent> <Leader>jf :write<CR>
nnoremap <silent> <Leader>jw :wall<CR>
nnoremap <silent> <Leader>je :qall<CR>
nnoremap <silent> <Leader>jc :call SpawnShell('enew')<CR>
nnoremap <silent> <Leader>js :call SpawnShell('new')<CR>
nnoremap <silent> <Leader>jv :call SpawnShell('vnew')<CR>
nnoremap <silent> <Leader>jo :call OpenFromCwd()<CR>
nnoremap <Leader>jk gq

" Window mapping
nnoremap <Leader>d <C-w>
nnoremap <Leader>dd <C-w><C-w>

" NERD commenter
nnoremap <Leader>c <NOP>
vnoremap <Leader>c <NOP>

" Miscellaneous top-level leader mappings
nnoremap <Leader>; q:i
vnoremap <Leader>; q:i


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


command! DiffOrig vertical new | set bt=nofile | r # | 0d_ | diffthis
      \ | wincmd p | diffthis

command! -nargs=? -complete=tag Tagge tag <args>
command! -nargs=? -complete=tag STagge stag <args>
command! -nargs=? -complete=tag VSTagge vertical stag <args>
command! -nargs=1 -complete=tag Cfs cscope find s <args>
command! -nargs=1 -complete=tag Csfs scscope find s <args>
command! -nargs=1 -complete=tag Vcsfs vertical scscope find s <args>
command! -nargs=1 -complete=tag Cfc cscope find c <args>
command! -nargs=1 -complete=tag Csfc scscope find c <args>
command! -nargs=1 -complete=tag Vcsfc vertical scscope find c <args>
command! -nargs=1 -complete=file Cff cscope find f
command! -nargs=1 -complete=file Scff scscope find f
command! -nargs=1 -complete=file Vscff vertical scscope find f
command! -nargs=0 Sexe execute SelectedLines()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Command abbreviations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


augroup cmd_window_au
    autocmd!

    autocmd CmdwinEnter * iabbrev <buffer> vsn vertical snext
    autocmd CmdwinEnter * iabbrev <buffer> vsN vertical sNext
    autocmd CmdwinEnter * iabbrev <buffer> vstag vertical stag
    autocmd CmdwinEnter * iabbrev <buffer> tbn tabnew
    autocmd CmdwinEnter * iabbrev <buffer> tagge Tagge
    autocmd CmdwinEnter * iabbrev <buffer> stagge STagge
    autocmd CmdwinEnter * iabbrev <buffer> vstagge VSTagge
    autocmd CmdwinEnter * iabbrev <buffer> ff Cff
    autocmd CmdwinEnter * iabbrev <buffer> sff Scff
    autocmd CmdwinEnter * iabbrev <buffer> vsff Vscff
    autocmd CmdwinEnter * iabbrev <buffer> fs Cfs
    autocmd CmdwinEnter * iabbrev <buffer> sfs Csfs
    autocmd CmdwinEnter * iabbrev <buffer> vsfs Vcsfs
    autocmd CmdwinEnter * iabbrev <buffer> fc Cfc
    autocmd CmdwinEnter * iabbrev <buffer> sfc Csfc
    autocmd CmdwinEnter * iabbrev <buffer> vsfc Vcsfc

augroup END
