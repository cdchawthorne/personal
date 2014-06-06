call coquille#FNMapping()
nnoremap <silent> <buffer> <LocalLeader>c :CoqToCursor<CR>

function! Cdcoqlaunch()
    CoqLaunch
    nnoremap <silent> <buffer> <LocalLeader>l :call Cdcoqkill()<CR>
endfunction

function! Cdcoqkill()
    CoqKill
    nnoremap <silent> <buffer> <LocalLeader>l :call Cdcoqlaunch()<CR>
endfunction

call Cdcoqlaunch()
