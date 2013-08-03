setlocal shiftwidth=2
setlocal iskeyword+=:


command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lea call LaTeXEnvironment("<args>", "after")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Leb call LaTeXEnvironment("<args>", "before")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lbe call LaTeXBeginEnvironment("<args>")
command! -buffer -nargs=0 Lee call LaTeXEndEnvironment()
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lce execute "normal \<Plug>LatexChangeEnv<args>\<CR>"

nnoremap <buffer> <LocalLeader>b :Lbe 
nnoremap <silent> <buffer> <LocalLeader>l :Lee<CR>
nnoremap <buffer> <LocalLeader>e :Lea 
nnoremap <buffer> <LocalLeader>E :Leb 
nnoremap <buffer> <LocalLeader>s :Lce 
nnoremap <silent> <buffer> <LocalLeader>d
            \ :call search('^\\begin{document}$', 'ws')<CR>

inoremap <buffer> pdb <C-]><Esc>:Lbe 
inoremap <silent> <buffer> pdl <C-]><Esc>:Lee<CR>A
inoremap <buffer> pde <C-]><Esc>:Lea 
inoremap <buffer> pdE <C-]><Esc>:Leb 

iabbrev <buffer> == &=&
iabbrev tfdc tfdc{}
iabbrev tfdc: tfdc{}:
iabbrev Tfdc Tfdc{}
iabbrev Tfdc: Tfdc{}:
iabbrev tfdsc tfdsc{}
iabbrev tfdsc: tfdsc{}:
iabbrev Tfdsc Tfdsc{}
iabbrev Tfdsc: Tfdsc{}:

nnoremap <silent> <buffer> <LocalLeader>c
            \ :call VimLaTeXCompile() \| call PdfView()<CR>
nnoremap <silent> <buffer> <LocalLeader>v :call PdfView()<CR>
nnoremap <silent> <buffer> <LocalLeader>t :call LatexBox_TOC()<CR>

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
