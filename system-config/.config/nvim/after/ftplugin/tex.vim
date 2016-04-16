setlocal shiftwidth=2

" Functions

function! LaTeXCompileAndView()
    write
    let is_hidden = &hidden
    set nohidden
    silent only
    let &hidden = is_hidden
    let command = "latexmk -cd " . shellescape(expand("%"))
    let command .= " && (pgrep -f "
    let command .= shellescape('^zathura --fork ' . expand("%:p:r") . '.pdf$')
    let command .= " &> /dev/null || zathura --fork "
    let command .= shellescape(expand("%:p:r") . '.pdf') . " &> /dev/null)"
    "TODO: determine the width automatically?
    66vnew
    call termopen(command)
    setlocal nobuflisted
    execute "normal \<C-w>h"
endfunction

function! LaTeXClean()
    call jobstart(['latexmk', '-C', '-cd', expand('%')])
    echo 'LaTeXClean'
endfunction

" TODO: jobs?
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
                \ "cases", "aside", "subclaim", "embedlua", "luacode",
                \ "tikzcd" ]
    call sort(envs)

    return filter(envs, 'v:val =~# "^' . a:ArgLead . '"')
endfunction

command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Le call LaTeXEnvironment("<args>", "no_label")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lel call LaTeXEnvironment("<args>", "label")
command! -buffer -nargs=1 -range -complete=customlist,LaTeXEnvironmentComplete
            \ Lea call LaTeXEnvironmentAround(<line1>, <line2>, "<args>")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lce execute "normal \<Plug>LatexChangeEnv<args>\<CR>"

" TODO: check if current line is empty
nnoremap <silent> <buffer> <LocalLeader>l o\[<CR>\]<Esc>kA
nnoremap <buffer> <LocalLeader>e :Le 
nnoremap <buffer> <LocalLeader>f :Lel 
nnoremap <silent> <buffer> <LocalLeader>a
            \ :set operatorfunc=LaTeXEnvironmentAroundOp<CR>g@
nnoremap <buffer> <LocalLeader>s :Lce 
nnoremap <silent> <buffer> <LocalLeader>d
            \ :call search('^\\begin{document}$', 'ws')<CR>
nnoremap <silent> <buffer> <LocalLeader>t
            \ A\tfdc{}:<Esc>:Le tcd<CR>
vnoremap <silent> <buffer> <LocalLeader>a
            \ :<C-u>call LaTeXEnvironmentAroundOp(visualmode())<CR>
vnoremap <silent> <buffer> <LocalLeader>d
            \ :call search('^\\begin{document}$', 'ws')<CR>

inoremap <buffer> kd \
" inoremap <buffer> _ -
" inoremap <buffer> - _

inoremap <buffer> fde <C-]><C-g>u<Esc>:Le 
inoremap <buffer> fdf <C-]><C-g>u<Esc>:Lel 
inoremap <buffer> fdl <C-]><C-g>u<Esc>o\[<CR>\]<Esc>kA
inoremap <buffer> fdt <Space><C-g>u\tfdc{}:<C-]><Esc>gqgq:Le tcd<CR>

" TODO: semigroups
iabbrev <buffer> group grape
iabbrev <buffer> groups grapes
iabbrev <buffer> Group Grape
iabbrev <buffer> Groups Grapes
iabbrev <buffer> subgroup subgrape
iabbrev <buffer> subgroups subgrapes
iabbrev <buffer> Subgroup Subgrape
iabbrev <buffer> Subgroups Subgrapes
iabbrev <buffer> groupoid grape-oid
iabbrev <buffer> groupoids grape-oids
iabbrev <buffer> Groupoid Grape-oid
iabbrev <buffer> Groupoids Grape-oids
iabbrev <buffer> group: grape:
iabbrev <buffer> groups: grapes:
iabbrev <buffer> Group: Grape:
iabbrev <buffer> Groups: Grapes:
iabbrev <buffer> subgroup: subgrape:
iabbrev <buffer> subgroups: subgrapes:
iabbrev <buffer> Subgroup: Subgrape:
iabbrev <buffer> Subgroups: Subgrapes:
iabbrev <buffer> groupoid: grape-oid:
iabbrev <buffer> groupoids: grape-oids:
iabbrev <buffer> Groupoid: Grape-oid:
iabbrev <buffer> Groupoids: Grape-oids:
iabbrev <buffer> == &=&
iabbrev <buffer> tfdc tfdc{}
iabbrev <buffer> tfdc: tfdc{}:
iabbrev <buffer> Tfdc Tfdc{}
iabbrev <buffer> Tfdc: Tfdc{}:
iabbrev <buffer> tfsc tfsc{}
iabbrev <buffer> tfsc: tfsc{}:
iabbrev <buffer> Tfsc Tfsc{}
iabbrev <buffer> Tfsc: Tfsc{}:
iabbrev <buffer> tfae tfae{}
iabbrev <buffer> tfae: tfae{}:
iabbrev <buffer> Tfae Tfae{}
iabbrev <buffer> Tfae: Tfae{}:
iabbrev <buffer> i.e. i.e.\
iabbrev <buffer> (i.e. (i.e.\
iabbrev <buffer> e.g. e.g.\
iabbrev <buffer> (e.g. (e.g.\

nnoremap <silent> <buffer> <LocalLeader>c
            \ :call LaTeXCompileAndView()<CR>
nnoremap <silent> <buffer> <LocalLeader>v :call PdfView()<CR>
nnoremap <silent> <buffer> <LocalLeader>t :call LatexBox_TOC()<CR>
nnoremap <silent> <buffer> <LocalLeader>r :call LaTeXClean()<CR>

" Because I don't use the default LatexBox mappings
nmap <buffer> <Tab> <Plug>LatexBox_JumpToMatch
vmap <buffer> <Tab> <Plug>LatexBox_JumpToMatch
omap <buffer> <Tab> <Plug>LatexBox_JumpToMatch
vmap <buffer> ie <Plug>LatexBox_SelectCurrentEnvInner
vmap <buffer> ae <Plug>LatexBox_SelectCurrentEnvOuter
onoremap <silent> <buffer> ie :normal vie<CR>
onoremap <silent> <buffer> ae :normal vae<CR>
vmap <buffer> i$ <Plug>LatexBox_SelectInlineMathInner
vmap <buffer> a$ <Plug>LatexBox_SelectInlineMathOuter
onoremap <silent> <buffer> i$ :normal vi$<CR>
onoremap <silent> <buffer> a$ :normal va$<CR>

augroup LatexBox_HighlightPairs
    autocmd!
augroup END
