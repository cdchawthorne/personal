function! MakeGraphs()
    call jobstart(['make_graphs', expand('%')])
    echo 'MakeGraphs'
endfunction

nnoremap <buffer> <LocalLeader>c <Cmd>call MakeGraphs()<CR>
