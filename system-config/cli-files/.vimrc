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
set expandtab
set gdefault
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
if $DISPLAY !~ '^$' || $zsh_parent_process ==# 'sshd'
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

" LaTeX suite options

" Apparently necessary for vim-latex
" See :help latex-suite.txt
set grepprg=grep\ -nH\ $*
let g:tex_flavor="latex"
let g:Tex_DefaultTargetFormat="pdf"
let g:Tex_MultipleCompileFormats="dvi,pdf"

" Avoid folding in tex documents
let Tex_FoldedEnvironments=""
let Tex_FoldedMisc=""
let Tex_FoldedSections=""

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
      \ normal Gdd6k$
augroup END

augroup set_filetype
    autocmd!
    autocmd BufNewFile,BufRead *.txt set filetype=text
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
    if (&number == 1)
        set relativenumber
    else
        set number
    endif
endfunction

function! SelectedLines()
    return join(getline(line("'<"), line("'>")), "\n")
endfunction

function! LaTeXCompile()
    write
    silent !latexmk -pdf %
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

function! LaTeXEnvironment(env, beforeOrAfter)
    if a:beforeOrAfter ==# "before"
        let keys = "O"
    else
        let keys = "o"
    endif
    let keys .= "\\begin{\<Esc>\"=a:env\<CR>pa}\<CR> \<BS>\<CR>"
    let keys .= "\\end{\<Esc>\"=a:env\<CR>pa}\<Esc>k"
    execute "normal! " . keys
    startinsert!
endfunction

function! LaTeXBeginEnvironment(env)
    execute "normal! o\\begin{\<Esc>\"=a:env\<CR>pa}\<CR> \<BS>"
    startinsert!
endfunction

function! LaTeXEndEnvironment()
    let oldLine = line(".")
    let oldColumn = col(".")

    let deleteOld = 0
    if getline(".") =~# '\v^ *$'
        let deleteOld = 1
        call search('\v[^ ]', 'Wb')
    endif

    let endLine = line(".")
    let endColumn = col(".")

    call EscapeIndent("up")
    while getline(".") !~# '\v^ *\\begin\{[[:alpha:]]+\*?\}' &&
                \ col(".") != 1 && line(".") != 1
        call EscapeIndent("up")
    endwhile

    let beginEnv = matchstr(getline("."), '\v^ *\\begin\{[[:alpha:]]+\*?\}')

    if beginEnv !=# ''
        if deleteOld
            call cursor(oldLine, oldColumn)
            normal dd
        endif

        call cursor(endLine, endColumn)

        " Without the g flag, it only gets the first match
        let endEnv = substitute(beginEnv, "begin", "end", "")
        exe "normal! o\<C-u>\<Esc>\"=endEnv\<CR>p"
    else
        call cursor(oldLine, oldColumn)
    endif
endfunction

function! LaTeXEnvironmentComplete(ArgLead, CmdLine, CursorPos)
    let envs = [
                \ "pf", "rpf", "dpf", "drpf", "ea", "tcd", "case", "subcase",
                \ "caselist", "theorem", "lemma", "proposition", "claim",
                \ "corollary", "fact", "todo", "definition", "notation",
                \ "question", "remark", "note", "conjecture", "dconjecture",
                \ "pconjecture", "exercise", "example", "counterexample",
                \ "enumerate", "itemize", "description", "proof", "tikzcd"
                \ ]
    call sort(envs)

    return filter(envs, 'v:val =~# "^' . a:ArgLead . '"')
endfunction


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


" Non-leader mappings
nnoremap <silent> Q gq
nnoremap <silent> QQ gqgq
nnoremap / /\v
nnoremap ? ?\v
nnoremap <silent> <Tab> %
nnoremap <silent> S m
nnoremap <silent> m <C-b>
nnoremap <silent> <Space> <C-f>

vnoremap <silent> Q gq
vnoremap / /\v
vnoremap ? ?\v
vnoremap <silent> <Tab> %
vnoremap <silent> m <C-b>
vnoremap <silent> <Space> <C-f>

onoremap <silent> <Tab> %
onoremap <silent> m <C-b>
onoremap <silent> <Space> <C-f>

inoremap <silent> fj <C-]><Esc>
inoremap <silent> Fj <C-]><Esc>
inoremap <silent> fJ <C-]><Esc>
inoremap <silent> FJ <C-]><Esc>
inoremap <silent> jf <C-]><Esc>
inoremap <silent> Jf <C-]><Esc>
inoremap <silent> jF <C-]><Esc>
inoremap <silent> JF <C-]><Esc>

inoremap <silent> ldm <C-g>u<Esc>viwUgi
inoremap <silent> ldk <C-k>
inoremap <silent> ldu <C-g>u<C-u>
inoremap <silent> ldn <C-n>
inoremap <silent> ldp <C-p>
inoremap <silent> lds <C-g>u<Esc>gqgqA
inoremap <silent> ldw <C-w>
inoremap <silent> ldx <NOP>

" Leader mappings
let mapleader = "s"
let maplocalleader = "sl"

nnoremap <silent> <Leader> <NOP>
nnoremap <silent> <Leader>x <NOP>
vnoremap <silent> <Leader> <NOP>
vnoremap <silent> <Leader>x <NOP>

nnoremap <silent> <LocalLeader> <NOP>
nnoremap <silent> <LocalLeader>x <NOP>
vnoremap <silent> <LocalLeader> <NOP>
vnoremap <silent> <LocalLeader>x <NOP>

" Navigation and editing
nnoremap <silent> <Leader>f <NOP>
nnoremap <silent> <Leader>fx <NOP>
vnoremap <silent> <Leader>f <NOP>
vnoremap <silent> <Leader>fx <NOP>

nnoremap <silent> <Leader>fr <C-r>
nnoremap <silent> <Leader>fd 0D
nnoremap <silent> <Leader>fn :call EscapeIndent("down")<CR>
nnoremap <silent> <Leader>fp :call EscapeIndent("up")<CR>
nnoremap <silent> <Leader>fb :call BracketReplace()<CR>

vnoremap <silent> <Leader>fd <C-v>0o$x
vnoremap <silent> <Leader>fn :call EscapeIndent("down")<CR>
vnoremap <silent> <Leader>fp :call EscapeIndent("up")<CR>

" vimrc and other config
nnoremap <silent> <Leader>v <NOP>
nnoremap <silent> <Leader>vx <NOP>
vnoremap <silent> <Leader>v <NOP>
vnoremap <silent> <Leader>vx <NOP>

nnoremap <silent> <Leader>ve :split $MYVIMRC<CR>
nnoremap <silent> <Leader>vs :source $MYVIMRC \| filetype detect<CR>
nnoremap <silent> <Leader>vk :split $HOME/.vim/skeleton.%:e<CR>

" Settings and plugins
nnoremap <silent> <Leader>s <NOP>
nnoremap <silent> <Leader>sx <NOP>
vnoremap <silent> <Leader>s <NOP>
vnoremap <silent> <Leader>sx <NOP>

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
nnoremap <silent> <Leader>k <NOP>
nnoremap <silent> <Leader>kx <NOP>
vnoremap <silent> <Leader>k <NOP>
vnoremap <silent> <Leader>kx <NOP>

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

" Window mapping
nnoremap <silent> <Leader>d <C-w>
nnoremap <silent> <Leader>dd <C-w><C-w>

" NERD commenter
nnoremap <silent> <Leader>c <NOP>
nnoremap <silent> <Leader>cx <NOP>
vnoremap <silent> <Leader>c <NOP>
vnoremap <silent> <Leader>cx <NOP>

" Miscellaneous top-level leader mappings
nnoremap <silent> <Leader>; q:i
vnoremap <silent> <Leader>; q:i


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


command! DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
      \ | wincmd p | diffthis

command! -nargs=? -complete=tag Tagge tag <args>
command! -nargs=? -complete=tag STagge stag <args>
command! -nargs=? -complete=tag VSTagge vert stag <args>
command! -nargs=1 -complete=tag Cfs cs f s <args>
command! -nargs=1 -complete=tag Csfs scs f s <args>
command! -nargs=1 -complete=tag Vcsfs vert scs f s <args>
command! -nargs=1 -complete=tag Cfc cs f c <args>
command! -nargs=1 -complete=tag Csfc scs f c <args>
command! -nargs=1 -complete=tag Vcsfc vert scs f c <args>
command! -nargs=1 -complete=file Cff cs f f
command! -nargs=1 -complete=file Scff scs f f
command! -nargs=1 -complete=file Vscff vert scs f f
command! -nargs=0 Sexe execute SelectedLines()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Command abbreviations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


augroup cmd_window_au
    autocmd!

    autocmd CmdwinEnter * iabbrev <buffer> vsn vert snext
    autocmd CmdwinEnter * iabbrev <buffer> vsN vert sNext
    autocmd CmdwinEnter * iabbrev <buffer> vstag vert stag
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
