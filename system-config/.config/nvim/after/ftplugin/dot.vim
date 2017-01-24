function! MakeGraphs()
    call jobstart(['make_graphs', expand('%')])
    echo 'MakeGraphs'
endfunction

nnoremap <buffer> <LocalLeader>c :call MakeGraphs()<CR>
