" Basically a fork of bufferline
" TODO: proper plugin
" TODO: spacing

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
    let alternate_buffer = a:window ==# winnr() ? bufnr('#') : getwinvar(a:window, 'alternate_buffer', -1)
    let current_buffer = winbufnr(a:window)
    " TODO: special behaviour here?
    " if !bufexists(current_buffer) || !buflisted(current_buffer)
        " let current_buffer = -1
    " endif
    return [shell_buffers, file_buffers, current_buffer, alternate_buffer]
endfunction

function! MakeShellName(index, bufnum, current_buffer, alternate_buffer)
    if a:bufnum ==# a:current_buffer
        return '%#StatusLine#[' . a:index . ']%#StatusLineNC#'
    elseif a:bufnum ==# a:alternate_buffer
        return '(' . a:index . ')'
    else
        return a:index
    endif
endfunction

function! MakeFileName(index, file_data, current_buffer, alternate_buffer)
    let [bufnum, modified, fname] = a:file_data
    let line = a:index . ':'
    let line .= substitute(fnamemodify(fname, ':t'), '%', '%%', 'g')
    let line .= (modified ? '+' : '')
    if bufnum ==# a:current_buffer
        let line = '%#StatusLine#[' . line . ']%#StatusLineNC#'
    elseif bufnum ==# a:alternate_buffer
        let line = '(' . line . ')'
    endif
    return line
endfunction

function! MakeStatusLine(shell_buffers, file_buffers, current_buffer, alternate_buffer)
    let shell_names = map(a:shell_buffers, 'MakeShellName(v:key+len(a:file_buffers)+1, v:val, a:current_buffer, a:alternate_buffer)')
    let file_names = map(a:file_buffers, 'MakeFileName(v:key+1, v:val, a:current_buffer, a:alternate_buffer)')
    return '%#StatusLineNC#' . join(file_names, '  ') . '%=' . join(shell_names, '  ')
endfunction

" Convenience function for returning new status line and updating
" g:cbl_bufnummap
function! CdcBufferlineUpdate(window)
    call UpdateVarsIfNecessary(a:window)
    let [shell_buffers, file_buffers, current_buffer, alternate_buffer] = GetBufferData(a:window)
    call setwinvar(a:window, 'alternate_buffer', alternate_buffer)
    let g:cbl_bufnummap = map(copy(file_buffers), 'v:val[0]') + shell_buffers
    return MakeStatusLine(shell_buffers, file_buffers, current_buffer, alternate_buffer)
endfunction

function! StatusLine(window)
    return '%!CdcBufferlineUpdate(' . a:window . ')'
endfunction

" This is frightening on *so* many levels...
function! UpdateVarsIfNecessary(window)
    if getwinvar(a:window, '&statusline') ==# StatusLine(a:window)
        return 0
    endif
    " Well, something got confused
    " Update EVERYTHING
    let i = 1
    let last_window = winnr('$')
    while i <= last_window
        call setwinvar(i, '&statusline', StatusLine(i))
        let i += 1
    endwhile
    return 1
endfunction

augroup BufferLine
    autocmd!

    " TODO: Do we need bufwinenter? Is this a bug in setlocal statusline?
    autocmd WinEnter,VimEnter,BufWinEnter *
        \ execute 'setlocal statusline=' . StatusLine(winnr())
augroup END
