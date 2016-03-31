" Basically a fork of bufferline
" TODO: proper plugin

function! GetBufferData()
    let shell_names = []
    let file_names = []

    let i = 1
    let last_buffer = bufnr('$')
    while i <= last_buffer
        if bufexists(i) && buflisted(i)
            let modified = getbufvar(i, '&mod')

            " WHY, VIM. WHY
            let is_term = getbufvar(i, 'terminal_job_pid', 'def') !=# 'def'
            let fname = bufname(i)

            if is_term
                call add(shell_names, i)
            else
                call add(file_names, [i, modified, fname])
            endif
        endif
        let i += 1
    endwhile
    return [shell_names, file_names, bufnr('%')]
endfunction

" TODO: check for help file
" Ugh... why
function! StatusLineParts(shell_names, file_names, current_buffer)
    let lines = ['']
    let i = 0
    " TODO: bold current
    while i < len(a:shell_names)
        if a:shell_names[i] == a:current_buffer
            call add(lines, '[')
        endif

        " TODO: display program or CWD?
        let lines[-1] .= (i+1) . ':zsh'
        if a:shell_names[i] == a:current_buffer
            let lines[-1] .= ']'
            " I have no idea why this is necessary...
            " The new section seems to eat a space for whatever reason
            call add(lines, ' ')
        endif
        let lines[-1] .= '  '
        let i += 1
    endwhile

    if !empty(a:shell_names) && !empty(a:file_names)
        let lines[-1] .= '|  '
    endif
    let i = 0
    while i < len(a:file_names)
        let [bufnum, modified, fname] = a:file_names[i]
        if bufnum == a:current_buffer
            call add(lines, '[')
        endif
        let lines[-1] .= (i+len(a:shell_names) + 1) . ':'
        let lines[-1] .= substitute(fnamemodify(fname, ':t'), '%', '%%', 'g')
        " TODO: space if unmodified?
        let lines[-1] .= (modified ? '+' : '')
        if bufnum == a:current_buffer
            let lines[-1] .= ']'
            call add(lines, ' ')
        endif
        let lines[-1] .= '  '
        let i += 1
    endwhile
    return lines
endfunction

" Convenience function for returning new status line and updating
" g:cbl_bufnummap
function! CdcBufferlineUpdate()
    let [shell_names, file_names, current_buffer] = GetBufferData()
    let [g:cbl_before, g:cbl_current, g:cbl_after] = 
                \ StatusLineParts(shell_names, file_names, current_buffer)
    let g:cbl_bufnummap = shell_names + map(file_names, 'v:val[0]')
    return ''
    let line = '%#StatusLineNC#'
endfunction

function! CdcBufferlineStatusString()
    let line = '%#StatusLineNC#%{g:cbl_before}'
    let line .= '%#StatusLine#%{g:cbl_current}'
    let line .= '%#StatusLineNC#%{g:cbl_after}'
    return line
endfunction
