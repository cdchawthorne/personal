"
" Mappings and functions are allowed to clobber the z mark or buffer.
"


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.config/nvim/plugged')

" Plug 'benekastah/neomake'
Plug 'ciaranm/inkpot'
Plug 'LaTeX-Box-Team/LaTeX-Box', {'for': 'tex'}
" Plug 'jalvesaq/vimcmdline'
" Plug 'junegunn/goyo.vim'
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" Plug 'junegunn/fzf.vim'
" Plug 'junegunn/limelight.vim'
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

" Colours
let g:inkpot_black_background=1
colorscheme inkpot
highlight ColorColumn ctermbg=60

" Statusline
set statusline=%t
set statusline+=\ (%l
set statusline+=/
set statusline+=%L)
set statusline+=\ %m
set statusline+=\ %r
set statusline+=%=
set statusline+=\ Column
set statusline+=\ %c
set statusline+=\ Buffer
set statusline+=\ %n

" Gundo options
let g:gundo_right = 1

" NERDCommenter options
let NERDSpaceDelims=1

" Tagbar options
let g:tagbar_width=28
let g:tagbar_sort=0
let g:tagbar_compact=1

" vim-surround options
let g:surround_109 = "\\(\r\\)"
let g:surround_77 = "\\( \r \\)"
let g:surround_110 = "\\[\r\\]"
let g:surround_78 = "\\[ \r \\]"

" LaTeX Box options
let g:LatexBox_no_mappings = 1

" Get rid of annoying & indenting in LaTeX
let g:tex_indent_and = 0

" LaTeX syntax highlighting customization
let g:tex_flavor="latex"
let g:tex_items='\\bibitem\|\\item\|\\plr\|\\prl\|\\case\|\\lit\|\\pss\|'
let g:tex_items.='\\psps\|\\pip\|\\piff'
let g:tex_itemize_env='itemize\|description\|enumerate\|thebibliography\|'
let g:tex_itemize_env='caselist'

" Most recent tab
let g:tab_stack = [1]

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

    autocmd TermClose * call feedkeys("\<CR>")
    autocmd BufEnter zsh[0-9]\\\{1,3\} startinsert
augroup END

augroup set_last_tab
    autocmd!

    autocmd TabClosed * call TabJustClosed()
    autocmd TabLeave *
        \ call add(filter(g:tab_stack, 'v:val != ' . tabpagenr()), tabpagenr())
augroup END


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


let g:shell_num = 0
function! SpawnShell(layout_cmd)
    if exists("b:terminal_job")
        let dir = system("realpath /proc/" . jobpid(b:terminal_job) . "/cwd")
    else
        let dir = expand("%:p:h")
    endif
    let cwd = getcwd()
    execute 'cd ' . dir
    silent execute a:layout_cmd
    let b:terminal_job = termopen([$SHELL])
    execute 'cd ' . cwd
    execute "file zsh" . g:shell_num
    let g:shell_num = g:shell_num + 1
    startinsert
endfunction

function! TabJustClosed()
    let dead_num = remove(g:tab_stack, -1)
    call map(g:tab_stack, 'v:val > ' . dead_num . ' ? v:val - 1 : v:val')
    echom string(g:tab_stack)
    let dup_tab_stack = copy(g:tab_stack)
    execute "tabnext " . g:tab_stack[-1]
    let g:tab_stack = copy(dup_tab_stack)
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

function! LaTeXCompileAndView()
    " TODO: change to directory containing file
    write
    silent ![[ "$(tmux list-panes | wc -l)" -ge 2 ]] && tmux kill-pane -a
    let command = "!tmux split-window -dh -l 65 zsh -c \"latexmk "
    let command .= shellescape(expand("%")) . " && (pgrep -f '^zathura --fork "
    let command .= shellescape(expand("%:p:r")) . ".pdf$' &> /dev/null || "
    let command .= "zathura --fork " . shellescape(expand("%:p:r"))
    let command .= ".pdf &> /dev/null) || cat\""
    silent execute command
endfunction

function! LaTeXCompileAndViewNeo()
    " TODO: change to directory containing file
    write
    let is_hidden = &hidden
    set nohidden
    silent only
    let &hidden = is_hidden
    let command = "latexmk " . shellescape(expand("%"))
    let command .= " && (pgrep -f '^zathura --fork "
    let command .= shellescape(expand("%:p:r")) . ".pdf$' &> /dev/null || "
    let command .= "zathura --fork " . shellescape(expand("%:p:r"))
    let command .= ".pdf &> /dev/null)"
    67vnew
    call termopen(command)
    execute "normal \<C-w>h"
endfunction

function! LilyPondCompile()
    write
    silent !lilypond %
    redraw!
    if v:shell_error != 0
        !
    endif
endfunction

function! LaTeXClean()
    silent !latexmk -C
    redraw!
endfunction

function! PdfView()
    silent !pgrep -f '^zathura --fork %:p:r.pdf$' &> /dev/null
    if v:shell_error != 0
        silent !zathura --fork %:p:r.pdf &> /dev/null
    endif
    redraw!
endfunction

function! LaTeXEnvironment(env, labelOrNot)
    let current_line = getline(".")
    if current_line =~ '[^ ]'
        let keys = 'o'
    else
        let keys = 'cc'
    endif
    if a:env ==# "embedlua"
        let begenv = "\\embedlua"
        let endenv = "\\endembedlua"
    else
        let begenv = "\\begin{\<C-R>=a:env\<CR>}"
        let endenv = "\\end{\<C-R>=a:env\<CR>}"
    endif
    let keys .= begenv . "\<CR>" . endenv . "\<Esc>"
    if a:labelOrNot ==# "label"
        let keys .= "kA[]"
        execute "normal! " . keys
        startinsert
    else
        let keys .= "O \<BS>"
        execute "normal! " . keys
        startinsert!
    endif
endfunction

" TODO: optionally indent
function! LaTeXEnvironmentAroundOp(type)
    if a:type ==# 'v' || a:type ==# 'V'
        let start = line("'<")
        let end = line("'>")
    else
        let start = line("'[")
        let end = line("']")
    endif
    let env = input("","","customlist,LaTeXEnvironmentComplete")
    " Indent twice for itemize environments
    if env =~ g:tex_itemize_env
        execute start . "," . end . ">"
    endif
    execute start . "," . end . ">"

    if env ==# "embedlua"
        let begenv = "\\embedlua"
        let endenv = "\<BS>\\endembedlua"
    else
        let begenv = "\\begin{\<C-R>=env\<CR>}"
        let endenv = "\\end{\<C-R>=env\<CR>}"
    endif

    execute "normal! o" . endenv
    call cursor(start, 0)
    execute "normal! O" . begenv
endfunction

function! LaTeXEnvironmentComplete(ArgLead, CmdLine, CursorPos)
    let envs = [
                \ "pf", "rpf", "lrpf", "ea", "tcd", "equation",
                \ "caselist", "theorem", "lemma", "proposition", "claim",
                \ "corollary", "fact", "todo", "definition", "notation",
                \ "question", "remark", "exercise", "example", "enumerate",
                \ "itemize", "description", "pmatrix", "verbatim",
                \ "tabular", "menumerate", "mitemize", "mdescription",
                \ "cases", "aside", "subclaim", "embedlua", "luacode" ]
    call sort(envs)

    return filter(envs, 'v:val =~# "^' . a:ArgLead . '"')
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

" Tab mapping
" TODO: kill, break, move tabs
" TODO: move panes
" TODO: tnoremap pageup pagedown?
" TODO: ctrlspace?
nnoremap <C-k><C-k> :execute "tabnext " . g:tab_stack[-1]<CR>
nnoremap <C-k>c :call SpawnShell('$tabnew')<CR>
nnoremap <C-k>s :call SpawnShell('new')<CR>
nnoremap <C-k>v :call SpawnShell('vnew')<CR>
nnoremap <C-k>- :call SpawnShell('new')<CR>
nnoremap <C-k>\ :call SpawnShell('vnew')<CR>
nnoremap <C-k>n :tabprevious<CR>
nnoremap <C-k>p :tabnext<CR>
nnoremap <C-k>h <C-w>h
nnoremap <C-k>j <C-w>j
nnoremap <C-k>k <C-w>k
nnoremap <C-k>l <C-w>l
nnoremap <C-k>q :qall<CR>

nnoremap <C-k>1 :tabnext 1<CR>
nnoremap <C-k>2 :tabnext 2<CR>
nnoremap <C-k>3 :tabnext 3<CR>
nnoremap <C-k>4 :tabnext 4<CR>
nnoremap <C-k>5 :tabnext 5<CR>
nnoremap <C-k>^ :tabnext 6<CR>
nnoremap <C-k>& :tabnext 7<CR>
nnoremap <C-k>* :tabnext 8<CR>
nnoremap <C-k>( :tabnext 9<CR>
nnoremap <C-k>) :tabnext 10<CR>

tnoremap <C-k><C-k> <C-\><C-n>:execute "tabnext " . g:lasttab<CR>
tnoremap <C-k><C-k> <C-\><C-n>:execute "tabnext " . g:tab_stack[-1]<CR>
tnoremap <C-k>c <C-\><C-n>:call SpawnShell('tabnew')<CR>
tnoremap <C-k>s <C-\><C-n>:call SpawnShell('new')<CR>
tnoremap <C-k>v <C-\><C-n>:call SpawnShell('vnew')<CR>
tnoremap <C-k>- <C-\><C-n>:call SpawnShell('new')<CR>
tnoremap <C-k>\ <C-\><C-n>:call SpawnShell('vnew')<CR>
tnoremap <C-k>p <C-\><C-n>:tabprevious<CR>
tnoremap <C-k>n <C-\><C-n>:tabnext<CR>
tnoremap <C-k>h <C-\><C-n><C-w>h
tnoremap <C-k>j <C-\><C-n><C-w>j
tnoremap <C-k>k <C-\><C-n><C-w>k
tnoremap <C-k>l <C-\><C-n><C-w>l
tnoremap <C-k>q <C-\><C-n>:qall<CR>

tnoremap <C-k>1 <C-\><C-n>:tabnext 1<CR>
tnoremap <C-k>2 <C-\><C-n>:tabnext 2<CR>
tnoremap <C-k>3 <C-\><C-n>:tabnext 3<CR>
tnoremap <C-k>4 <C-\><C-n>:tabnext 4<CR>
tnoremap <C-k>5 <C-\><C-n>:tabnext 5<CR>
tnoremap <C-k>^ <C-\><C-n>:tabnext 6<CR>
tnoremap <C-k>& <C-\><C-n>:tabnext 7<CR>
tnoremap <C-k>* <C-\><C-n>:tabnext 8<CR>
tnoremap <C-k>( <C-\><C-n>:tabnext 9<CR>
tnoremap <C-k>) <C-\><C-n>:tabnext 10<CR>

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

nnoremap <Leader>fk gq

" vimrc and other config
nnoremap <Leader>v <NOP>
vnoremap <Leader>v <NOP>

nnoremap <silent> <Leader>ve :split $MYVIMRC<CR>
nnoremap <silent> <Leader>vs :source $MYVIMRC \| filetype detect<CR>
nnoremap <silent> <Leader>vk :split $HOME/.vim/skeleton.%:e<CR>

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

" Writing, quitting, and compiling
nnoremap <Leader>k <NOP>
vnoremap <Leader>k <NOP>

nnoremap <silent> <Leader>ks :write<CR>
nnoremap <silent> <Leader>kc :quit<CR>
nnoremap <silent> <Leader>kC :quit!<CR>
nnoremap <silent> <Leader>kl :wq<CR>
nnoremap <silent> <Leader>kw :wall<CR>
nnoremap <silent> <Leader>ke :qall<CR>
nnoremap <silent> <Leader>kE :qall!<CR>
nnoremap <silent> <Leader>ka :wqall<CR>
nnoremap <silent> <Leader>kn :next<CR>
nnoremap <silent> <Leader>kp :previous<CR>
nnoremap <silent> <Leader>kb :!<CR>

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
