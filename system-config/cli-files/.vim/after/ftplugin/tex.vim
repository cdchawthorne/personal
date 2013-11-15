setlocal shiftwidth=2
setlocal iskeyword+=:


command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lea call LaTeXEnvironment("<args>", "after")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Leb call LaTeXEnvironment("<args>", "before")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lba call LaTeXBeginEnvironment("<args>", "after")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lbb call LaTeXBeginEnvironment("<args>", "before")
command! -buffer -nargs=0 Lla call LaTeXEndEnvironment("after")
command! -buffer -nargs=0 Llb call LaTeXEndEnvironment("before")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lce execute "normal \<Plug>LatexChangeEnv<args>\<CR>"

nnoremap <buffer> <LocalLeader>b :Lba 
nnoremap <buffer> <LocalLeader>B :Lbb 
nnoremap <silent> <buffer> <LocalLeader>l :Lla<CR>
nnoremap <silent> <buffer> <LocalLeader>L :Lla<CR>
nnoremap <buffer> <LocalLeader>e :Lea 
nnoremap <buffer> <LocalLeader>E :Leb 
nnoremap <buffer> <LocalLeader>s :Lce 
nnoremap <silent> <buffer> <LocalLeader>d
            \ :call search('^\\begin{document}$', 'ws')<CR>

inoremap <silent> <buffer> kd \

inoremap <buffer> fdb <C-]><Esc>:Lba 
inoremap <buffer> fdB <C-]><Esc>:Lbe 
inoremap <silent> <buffer> fdl <C-]><Esc>:Lle<CR>A
inoremap <silent> <buffer> fdL <C-]><Esc>:LLe<CR>A
inoremap <buffer> fde <C-]><Esc>:Lea 
inoremap <buffer> fdE <C-]><Esc>:Leb 

iabbrev <buffer> group grape
iabbrev <buffer> groups grapes
iabbrev <buffer> Group Grape
iabbrev <buffer> Groups Grapes
iabbrev <buffer> generator venerator
iabbrev <buffer> generators venerators
iabbrev <buffer> Generator Venerator
iabbrev <buffer> Generators Venerators
iabbrev <buffer> == &=&
iabbrev <buffer> tfdc tfdc{}
iabbrev <buffer> tfdc: tfdc{}:
iabbrev <buffer> Tfdc Tfdc{}
iabbrev <buffer> Tfdc: Tfdc{}:
iabbrev <buffer> tfdsc tfdsc{}
iabbrev <buffer> tfdsc: tfdsc{}:
iabbrev <buffer> Tfdsc Tfdsc{}
iabbrev <buffer> Tfdsc: Tfdsc{}:

nnoremap <silent> <buffer> <LocalLeader>c
            \ :call LaTeXCompile() \| call PdfView()<CR>
nnoremap <silent> <buffer> <LocalLeader>v :call PdfView()<CR>
nnoremap <silent> <buffer> <LocalLeader>t :call LatexBox_TOC()<CR>
nnoremap <silent> <buffer> <LocalLeader>r :call LaTeXClean()<CR>

nmap <buffer> % <Plug>LatexBox_JumpToMatch
vmap <buffer> % <Plug>LatexBox_JumpToMatch
omap <buffer> % <Plug>LatexBox_JumpToMatch
nmap <buffer> <Tab> <Plug>LatexBox_JumpToMatch
vmap <buffer> <Tab> <Plug>LatexBox_JumpToMatch
omap <buffer> <Tab> <Plug>LatexBox_JumpToMatch
vmap <buffer> ie <Plug>LatexBox_SelectCurrentEnvInner
vmap <buffer> ae <Plug>LatexBox_SelectCurrentEnvOuter
onoremap <buffer> ie :normal vie<CR>
onoremap <buffer> ae :normal vae<CR>
vmap <buffer> i$ <Plug>LatexBox_SelectInlineMathInner
vmap <buffer> a$ <Plug>LatexBox_SelectInlineMathOuter
onoremap <buffer> i$ :normal vi$<CR>
onoremap <buffer> a$ :normal va$<CR>

augroup LatexBox_HighlightPairs
    autocmd!
augroup END
