"
" Mappings and functions are allowed to clobber the z mark or buffer.
"


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Options
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


set nocompatible

call pathogen#infect()

syntax on
filetype plugin indent on

" Builtin options
set backspace=indent,eol,start
set backup
set backupdir=${HOME}/utilities/vim_backups
set colorcolumn=80
set cryptmethod=blowfish2
set expandtab
set hidden
set history=10000
set hlsearch
set ignorecase
set incsearch
set matchtime=1
set nomodeline
set number
set relativenumber
set ruler
set shiftwidth=4
set showcmd
set showmatch
set smartcase
set smarttab
set softtabstop=4 
set spelllang=en_ca
set splitbelow
set splitright
set tabstop=4
set tags=${HOME}/utilities/databases/tags
set tildeop
set timeoutlen=1000
set ttyfast
set undofile
set undodir=${HOME}/utilities/vim_undo

" Colour and cursor trickery
if $STY !=# ''
    let &t_SI = "\<Esc>P\<Esc>[3 q\<Esc>\\"
    let &t_EI = "\<Esc>P\<Esc>[1 q\<Esc>\\"

    let &t_Co = 256

    let g:inkpot_black_background=1
    colorscheme inkpot

    highlight ColorColumn ctermbg=60
    set cursorline

elseif $DISPLAY !=# '' || $zsh_parent_process ==# 'sshd'
    let &t_SI = "\<Esc>[3 q"
    let &t_EI = "\<Esc>[1 q"

    let &t_Co = 256

    let g:inkpot_black_background=1
    colorscheme inkpot

    highlight ColorColumn ctermbg=60
    set cursorline

else
    let &t_SI = "\<Esc>[?2c"
    let &t_EI = "\<Esc>[?6c"
    " The following options in their default form override the above two
    " So we change them here
    let &t_ve = "\<Esc>[?25h"
    let &t_vi = "\<Esc>[?25l"
    let &t_vs = "\<Esc>[?25h"

    let &t_Co = 8
    colorscheme peachpuff

    highlight ColorColumn ctermbg=blue
    set nocursorline
endif

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

" Coquille options
" let g:coquille_coqtop_executable = "/home/cdchawthorne/documents/uw/courses/"
" let g:coquille_coqtop_executable .= "pmath911/assignments/HoTT/hoqtop"

" Gundo options
let g:gundo_right = 1

" NERDTree options
" TODO: get relativenumber set correctly, close NERDTree on last open
let NERDTreeShowLineNumbers=1
let NERDChristmasTree=0
let NERDTreeWinPos="right"
let NERDTreeWinSize=28

" NERDCommenter options
" TODO: fill this
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

let g:tex_flavor="latex"
let g:tex_items='\\bibitem\|\\item\|\\plr\|\\prl\|\\case\|\\lit\|\\pss\|\\psps'
let g:tex_itemize_env='itemize\|description\|enumerate\|thebibliography\|caselist'

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


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


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

function! LaTeXCompile()
    write
    silent !latexmk %
    redraw!
    if v:shell_error != 0
        !
    endif
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

" TODO: fix this to work on a blank line
"       Maybe find the indent of the next and previous non-blank lines and use
"       the larger of those
function! EscapeIndent(upOrDown)
    let oldLine = line(".")
    let oldColumn = col(".")

    normal ^
    let currentIndent = col(".") - 1
    let nextIndent = max([currentIndent - &shiftwidth, 0])
    let searchPattern = "^ \\{0," . string(nextIndent) . "\\}[^ ]"

    let flags = "nW"
    if a:upOrDown ==# "up"
        let flags .= "b"
    endif

    let targetLine = search(searchPattern, flags)

    if targetLine != 0
        call cursor(targetLine, 0)
        normal ^
    else
        call cursor(oldLine, oldColumn)
    endif
endfunction

function! LaTeXEnvironment(env, labelOrNot)
    let current_line = getline(".")
    if current_line =~ '[^ ]'
        let keys = 'o'
    else
        let keys = 'cc'
    endif
    let keys .= "\\begin{\<Esc>\"=a:env\<CR>pa}\<CR>"
    let keys .= "\\end{\<Esc>\"=a:env\<CR>pa}\<Esc>"
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
    execute "normal! o\\end{\<Esc>\"=env\<CR>pa}"
    call cursor(start, 0)
    execute "normal! O\\begin{\<Esc>\"=env\<CR>pa}"
endfunction

function! LaTeXEnvironmentComplete(ArgLead, CmdLine, CursorPos)
    let envs = [
                \ "pf", "rpf", "lrpf", "ea", "tcd", "equation",
                \ "caselist", "theorem", "lemma", "proposition", "claim",
                \ "corollary", "fact", "todo", "definition", "notation",
                \ "question", "remark", "exercise", "example", "enumerate",
                \ "itemize", "description", "pmatrix", "verbatim",
                \ "tabular", "menumerate", "mitemize", "mdescription",
                \ "cases", "aside", "subclaim" ]
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

inoremap fdm <C-g>u<C-]><Esc>viwUgi
inoremap fdk <C-k>
inoremap fdu <C-g>u<C-u>
inoremap fdn <C-n>
inoremap fdp <C-p>
inoremap fds <C-g>u<C-]><Esc>gqgqA
inoremap fdw <C-w>
inoremap fdx <NOP>
inoremap fdd <C-g>u<C-R>=strftime("%Y-%m-%d")<CR>

" Leader mappings
let mapleader = "s"
let maplocalleader = "sl"

nnoremap <Leader> <NOP>
nnoremap <Leader>x <NOP>
vnoremap <Leader> <NOP>
vnoremap <Leader>x <NOP>

nnoremap <LocalLeader> <NOP>
nnoremap <LocalLeader>x <NOP>
vnoremap <LocalLeader> <NOP>
vnoremap <LocalLeader>x <NOP>

" Navigation and editing
nnoremap <Leader>f <NOP>
nnoremap <Leader>fx <NOP>
vnoremap <Leader>f <NOP>
vnoremap <Leader>fx <NOP>

nnoremap <Leader>fr <C-r>
nnoremap <Leader>fd 0D
nnoremap <Leader>fn :call EscapeIndent("down")<CR>
nnoremap <Leader>fp :call EscapeIndent("up")<CR>
nnoremap <Leader>fk gq

vnoremap <Leader>fd <C-v>0o$x
vnoremap <Leader>fn :call EscapeIndent("down")<CR>
vnoremap <Leader>fp :call EscapeIndent("up")<CR>

" vimrc and other config
nnoremap <Leader>v <NOP>
nnoremap <Leader>vx <NOP>
vnoremap <Leader>v <NOP>
vnoremap <Leader>vx <NOP>

nnoremap <silent> <Leader>ve :split $MYVIMRC<CR>
nnoremap <silent> <Leader>vs :source $MYVIMRC \| filetype detect<CR>
nnoremap <silent> <Leader>vk :split $HOME/.vim/skeleton.%:e<CR>

" Settings and plugins
nnoremap <Leader>s <NOP>
nnoremap <Leader>sx <NOP>
vnoremap <Leader>s <NOP>
vnoremap <Leader>sx <NOP>

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
nnoremap <Leader>kx <NOP>
vnoremap <Leader>k <NOP>
vnoremap <Leader>kx <NOP>

nnoremap <silent> <Leader>ks :write<CR>
nnoremap <silent> <Leader>kc :quit<CR>
nnoremap <silent> <Leader>kC :quit!<CR>
nnoremap <silent> <Leader>kl :wq<CR>
nnoremap <silent> <Leader>kw :wall<CR>
nnoremap <silent> <Leader>ke :qa<CR>
nnoremap <silent> <Leader>kE :qa!<CR>
nnoremap <silent> <Leader>ka :wqa<CR>
nnoremap <silent> <Leader>kn :n<CR>
nnoremap <silent> <Leader>kp :N<CR>
nnoremap <silent> <Leader>kb :!<CR>

" Window mapping
nnoremap <Leader>d <C-w>
nnoremap <Leader>dd <C-w><C-w>

" NERD commenter
nnoremap <Leader>c <NOP>
nnoremap <Leader>cx <NOP>
vnoremap <Leader>c <NOP>
vnoremap <Leader>cx <NOP>

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
