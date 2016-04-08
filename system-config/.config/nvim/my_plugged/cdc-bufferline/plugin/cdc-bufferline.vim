" Basically a fork of bufferline
" TODO: proper plugin

function! GetBufferData(window)
    let shell_buffers = []
    let file_buffers = []

    let i = 1
    let last_buffer = bufnr('$')
    while i <= last_buffer
        if bufexists(i) && buflisted(i)
            let modified = getbufvar(i, '&mod')

            " WHY, VIM. WHY
            let is_term = getbufvar(i, 'terminal_job_pid', 'def') !=# 'def'
            let fname = bufname(i)

            if is_term
                " TODO: display program or CWD?
                call add(shell_buffers, i)
            else
                call add(file_buffers, [i, modified, fname])
            endif
        endif
        let i += 1
    endwhile
    let current_buffer = winbufnr(a:window)
    " TODO: special behaviour here?
    " if !bufexists(current_buffer) || !buflisted(current_buffer)
        " let current_buffer = -1
    " endif
    return [shell_buffers, file_buffers, current_buffer]
endfunction

function! MakeShellName(index, bufnum, current_buffer)
    if a:bufnum ==# a:current_buffer
        return '%#StatusLine#[' . a:index . ']%#StatusLineNC#'
    else
        return a:index
    endif
endfunction

function! MakeFileName(index, file_data, current_buffer)
    let [bufnum, modified, fname] = a:file_data
    let line = a:index . ':'
    let line .= substitute(fnamemodify(fname, ':t'), '%', '%%', 'g')
    let line .= (modified ? '+' : '')
    if bufnum ==# a:current_buffer
        let line = '%#StatusLine#[' . line . ']%#StatusLineNC#'
    endif
    return line
endfunction

function! MakeStatusLine(shell_buffers, file_buffers, current_buffer)
    let shell_names = map(a:shell_buffers, 'MakeShellName(v:key+len(a:file_buffers)+1, v:val, a:current_buffer)')
    let file_names = map(a:file_buffers, 'MakeFileName(v:key+1, v:val, a:current_buffer)')
    return '%#StatusLineNC#' . join(file_names, '  ') . '%=' . join(shell_names, '  ')
endfunction

" Convenience function for returning new status line and updating
" g:cbl_bufnummap
function! CdcBufferlineUpdate(window)
    let [shell_buffers, file_buffers, current_buffer] = GetBufferData(a:window)
    let g:cbl_bufnummap = map(copy(file_buffers), 'v:val[0]') + shell_buffers
    return MakeStatusLine(shell_buffers, file_buffers, current_buffer)
endfunction
