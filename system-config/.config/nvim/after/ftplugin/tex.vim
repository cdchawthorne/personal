setlocal shiftwidth=2
setlocal iskeyword+=:


command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Le call LaTeXEnvironment("<args>", "no_label")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lel call LaTeXEnvironment("<args>", "label")
command! -buffer -nargs=1 -range -complete=customlist,LaTeXEnvironmentComplete
            \ Lea call LaTeXEnvironmentAround(<line1>, <line2>, "<args>")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lce execute "normal \<Plug>LatexChangeEnv<args>\<CR>"

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
