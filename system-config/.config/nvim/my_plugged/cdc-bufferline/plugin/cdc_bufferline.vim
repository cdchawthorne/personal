" TODO: proper plugin. This probably just means better comments for the scary
"       stuff.
"       Oh, and docs.
"       This should export an off/on option which should disable the
"       autocommand, a command for turning it off and on, and the <Plug>
"       mappings
"       Rename it?
" TODO: should bufnummap be exported as a function?

augroup buffer_line
  autocmd!

  " TODO: Do we need bufwinenter? Is this a bug in setlocal statusline?
  autocmd WinEnter,VimEnter,BufWinEnter *
      \ execute 'setlocal statusline=' . cdc_bufferline#StatusLine(winnr())
augroup END

" Mappings and mapping-related functions

" TODO: can we put these in autoload and still have them script-local?
function! s:SwitchBuffer(buffer_command) abort
  if v:count && v:count <= len(g:cdc_bufferline#bufnummap)
    execute a:buffer_command . ' ' . g:cdc_bufferline#bufnummap[v:count-1]
  endif
endfunction

function! s:StepBuffer(multiplier) abort
  let steps = (v:count ? v:count : 1) * a:multiplier
  let idx = (index(g:cdc_bufferline#bufnummap, bufnr('%')) + steps)
  execute 'buffer '
      \ . g:cdc_bufferline#bufnummap[idx % len(g:cdc_bufferline#bufnummap)]
endfunction
 
nnoremap <silent> <Plug>CdcBufferlineBuffer
    \ :<C-u>call <SID>SwitchBuffer('buffer')<CR>
nnoremap <silent> <Plug>CdcBufferlineSplitBuffer
    \ :<C-u>call <SID>SwitchBuffer('sbuffer')<CR>
nnoremap <silent> <Plug>CdcBufferlineVSplitBuffer
    \ :<C-u>call <SID>SwitchBuffer('vertical sbuffer')<CR>
nnoremap <silent> <Plug>CdcBufferlineStepForward
    \ :<C-u>call <SID>StepBuffer(1)<CR>
nnoremap <silent> <Plug>CdcBufferlineStepBack
    \ :<C-u>call <SID>StepBuffer(-1)<CR>
