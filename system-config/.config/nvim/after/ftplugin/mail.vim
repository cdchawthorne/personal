setlocal formatoptions+=an
setlocal cc=

" inoremap <buffer> fds <C-o>gqip
" setlocal linebreak
" setlocal colorcolumn=
" setlocal nonumber
" setlocal norelativenumber
" " Disable auto-wrapping
" " Could also be done by formatoptions -='t', but this seems more semantically
" " correct? I'm not sure if textwidth does anything else; if not it sounds like
" " the formatoption is subsumed by textwidth
" setlocal textwidth=0

" noremap <buffer> k gk
" noremap <buffer> j gj
" noremap <buffer> $ g$
" noremap <buffer> ) g0
" noremap <buffer> 6 g^
" nnoremap <buffer> A g$a

" TODO: should probably use gw instead of gq
" function! Format() abort
"   let state = winsaveview()
"   normal gqip
"   call winrestview(state)
" endfunction

" augroup autowrap
"   autocmd!

"   " autocmd TextChanged,TextChangedI,TextChangedP <buffer> call Format()
"   autocmd InsertLeave <buffer> call Format()
" augroup END
