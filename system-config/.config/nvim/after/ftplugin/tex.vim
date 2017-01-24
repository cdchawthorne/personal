" TODO: should all of this really be in after?
" TODO: poke harder at vimtex
setlocal shiftwidth=2

" Functions

function! LaTeXCompileAndView()
    write
    let is_hidden = &hidden
    set nohidden
    silent only
    let &hidden = is_hidden
    let command = "latexmk -cd " . shellescape(expand("%"))
    let command .= " && (pgrep -f "
    let command .= shellescape('^zathura --fork ' . expand("%:p:r") . '.pdf$')
    let command .= " &> /dev/null || zathura --fork "
    let command .= shellescape(expand("%:p:r") . '.pdf') . " &> /dev/null)"
    "TODO: determine the width automatically?
    66vnew
    call termopen(command)
    setlocal nobuflisted
    execute "normal \<C-w>h"
endfunction

function! LaTeXClean()
    call jobstart(['latexmk', '-C', '-cd', expand('%')])
    echo 'LaTeXClean'
endfunction

" TODO: jobs?
function! PdfView()
    silent !pgrep -f '^zathura --fork %:p:r.pdf$' &> /dev/null
    if v:shell_error != 0
        silent !zathura --fork %:p:r.pdf &> /dev/null
    endif
    redraw!
endfunction

function! LaTeXEnvironment(env, labelOrNot)
    if getline('.') =~ '[^ ]'
        let keys = 'o'
    else
        let keys = 'cc'
    endif
    if a:env ==# "embedlua"
        let begenv = "\\embedlua"
        let endenv = "\\endembedlua"
    else
        let begenv = "\\begin{\<C-R>=a:env\<CR>}"
        let endenv = "\\end{\<C-R>=a:env\<CR>}"
    endif
    let keys .= begenv . "\<CR>" . endenv . "\<Esc>"
    if a:labelOrNot ==# "label"
        let keys .= "kA[]"
        execute "normal! " . keys
        startinsert
    else
        let keys .= "O \<BS>"
        execute "normal! " . keys
        startinsert!
    endif
endfunction

function! LaTeXDisplayMath()
    if getline('.') =~ '[^ ]'
        let keys = 'o'
    else
        let keys = 'cc'
    endif
    let keys .= "\\[\<CR>\\]\<Esc>k"
    execute 'normal! ' . keys
    startinsert!
endfunction

" TODO: optionally indent
function! LaTeXEnvironmentAroundOp(type)
    if a:type ==# 'v' || a:type ==# 'V'
        let start = line("'<")
        let end = line("'>")
    else
        let start = line("'[")
        let end = line("']")
    endif
    let env = input("","","customlist,LaTeXEnvironmentComplete")
    " Indent twice for itemize environments
    if env =~ g:tex_itemize_env
        execute start . "," . end . ">"
    endif
    execute start . "," . end . ">"

    if env ==# "embedlua"
        let begenv = "\\embedlua"
        let endenv = "\<BS>\\endembedlua"
    else
        let begenv = "\\begin{\<C-R>=env\<CR>}"
        let endenv = "\\end{\<C-R>=env\<CR>}"
    endif

    execute "normal! o" . endenv
    call cursor(start, 0)
    execute "normal! O" . begenv
endfunction

let g:latex_envs = [
            \ "pf", "rpf", "lrpf", "ea", "tcd", "equation",
            \ "caselist", "theorem", "lemma", "proposition", "claim",
            \ "corollary", "fact", "todo", "definition", "notation",
            \ "question", "remark", "exercise", "example", "enumerate",
            \ "itemize", "description", "pmatrix", "verbatim",
            \ "tabular", "menumerate", "mitemize", "mdescription",
            \ "cases", "aside", "subclaim", "embedlua", "luacode",
            \ "tikzcd", "center", "figure", "table", "multline",
            \ "align", "split", "conjecture" ]
call sort(g:latex_envs)
let g:vimtex_env_complete_list = g:latex_envs + ['\[']
function! LaTeXEnvironmentComplete(ArgLead, CmdLine, CursorPos)
    return filter(copy(g:latex_envs), 'v:val =~# "^' . a:ArgLead . '"')
endfunction

command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Le call LaTeXEnvironment("<args>", "no_label")
command! -buffer -nargs=1 -complete=customlist,LaTeXEnvironmentComplete
            \ Lel call LaTeXEnvironment("<args>", "label")

" TODO: check if current line is empty
nnoremap <silent> <buffer> <LocalLeader>m :call LaTeXDisplayMath()<CR>
nnoremap <buffer> <LocalLeader>e :Le 
nnoremap <buffer> <LocalLeader>f :Lel 
nnoremap <silent> <buffer> <LocalLeader>a
            \ :set operatorfunc=LaTeXEnvironmentAroundOp<CR>g@
nnoremap <silent> <buffer> <LocalLeader>d
            \ :call search('^\\begin{document}$', 'ws')<CR>
vnoremap <silent> <buffer> <LocalLeader>a
            \ :<C-u>call LaTeXEnvironmentAroundOp(visualmode())<CR>
vnoremap <silent> <buffer> <LocalLeader>d
            \ :call search('^\\begin{document}$', 'ws')<CR>
nnoremap <silent> <buffer> <LocalLeader>c
            \ :call LaTeXCompileAndView()<CR>
nnoremap <silent> <buffer> <LocalLeader>v :call PdfView()<CR>
nnoremap <silent> <buffer> <LocalLeader>t :VimtexTocOpen<CR>
nnoremap <silent> <buffer> <LocalLeader>r :call LaTeXClean()<CR>

nmap <buffer> <Tab> <Plug>(vimtex-%)
vmap <buffer> <Tab> <Plug>(vimtex-%)
omap <buffer> <Tab> <Plug>(vimtex-%)


inoremap <buffer> kd \

inoremap <buffer> fde <C-]><C-g>u<Esc>:Le 
inoremap <buffer> fdf <C-]><C-g>u<Esc>:Lel 
inoremap <silent> <buffer> fdm <C-]><C-g>u<Esc>:call LaTeXDisplayMath()<CR>
inoremap <buffer> fdt <Space><C-g>u\tfdc{}:<C-]><Esc>gqgq:Le tcd<CR>
imap <buffer> fdl <C-]><C-g>u<Esc>hcac
inoremap <buffer> fdv <C-]><C-g>u<Esc>dFvxa
inoremap <buffer> fdc <C-]><C-g>u<C-o>:call LaTeXCompileAndView()<CR>

" TODO: semigroups
iabbrev <buffer> group grape
iabbrev <buffer> groups grapes
iabbrev <buffer> Group Grape
iabbrev <buffer> Groups Grapes
iabbrev <buffer> subgroup subgrape
iabbrev <buffer> subgroups subgrapes
iabbrev <buffer> Subgroup Subgrape
iabbrev <buffer> Subgroups Subgrapes
iabbrev <buffer> groupoid grape-oid
iabbrev <buffer> groupoids grape-oids
iabbrev <buffer> Groupoid Grape-oid
iabbrev <buffer> Groupoids Grape-oids
iabbrev <buffer> semigroup semigrape
iabbrev <buffer> semigroups semigrapes
iabbrev <buffer> Semigroup Semigrape
iabbrev <buffer> Semigroups Semigrapes
iabbrev <buffer> groupification grape-ification
iabbrev <buffer> groupifications grape-ifications
iabbrev <buffer> Groupification Grape-ification
iabbrev <buffer> Groupifications Grape-ifications
iabbrev <buffer> group: grape:
iabbrev <buffer> groups: grapes:
iabbrev <buffer> Group: Grape:
iabbrev <buffer> Groups: Grapes:
iabbrev <buffer> subgroup: subgrape:
iabbrev <buffer> subgroups: subgrapes:
iabbrev <buffer> Subgroup: Subgrape:
iabbrev <buffer> Subgroups: Subgrapes:
iabbrev <buffer> groupoid: grape-oid:
iabbrev <buffer> groupoids: grape-oids:
iabbrev <buffer> Groupoid: Grape-oid:
iabbrev <buffer> Groupoids: Grape-oids:
iabbrev <buffer> semigroup: semigrape:
iabbrev <buffer> semigroups: semigrapes:
iabbrev <buffer> Semigroup: Semigrape:
iabbrev <buffer> Semigroups: Semigrapes:
iabbrev <buffer> groupification: grape-ification:
iabbrev <buffer> groupifications: grape-ifications:
iabbrev <buffer> Groupification: Grape-ification:
iabbrev <buffer> Groupifications: Grape-ifications:
iabbrev <buffer> == &=&
iabbrev <buffer> tfdc tfdc{}
iabbrev <buffer> tfdc: tfdc{}:
iabbrev <buffer> Tfdc Tfdc{}
iabbrev <buffer> Tfdc: Tfdc{}:
iabbrev <buffer> tfsc tfsc{}
iabbrev <buffer> tfsc: tfsc{}:
iabbrev <buffer> Tfsc Tfsc{}
iabbrev <buffer> Tfsc: Tfsc{}:
iabbrev <buffer> tfae tfae{}
iabbrev <buffer> tfae: tfae{}:
iabbrev <buffer> Tfae Tfae{}
iabbrev <buffer> Tfae: Tfae{}:
iabbrev <buffer> i.e. i.e.\
iabbrev <buffer> (i.e. (i.e.\
iabbrev <buffer> e.g. e.g.\
iabbrev <buffer> (e.g. (e.g.\

let font_leader = 'vk'
let command_leader = 'vm'
let letter_leader = 'vl'

" NOTE: DO NOT BIND vkd
let upper_font_maps = {'b' : 'bf', 'c': 'cal', 'l': 'bb', 'f': 'frak',
            \ 's': 'scr'}
let lower_font_maps = {'t' : 'bf', 'k': 'frak'}
for c in range(char2nr('a'),char2nr('z'))
    for [key, fontspec] in items(upper_font_maps)
        execute printf('inoremap <buffer> %s \math%s{%s}',
                    \ font_leader . key . nr2char(c),
                    \ fontspec, toupper(nr2char(c)))
    endfor
    for [key, fontspec] in items(lower_font_maps)
        execute printf('inoremap <buffer> %s \math%s{%s}',
                    \ font_leader . key . nr2char(c),
                    \ fontspec, nr2char(c))
    endfor
endfor

let command_maps = {
            \  ',' : 'subseteq ',
            \ '<' : 'subsetneqq ',
            \ '.' : 'supseteq ',
            \ '>' : 'supsetneqq ',
            \ 'e' : 'emptyset ',
            \ '*' : 'otimes ',
            \ '+' : 'oplus ',
            \ 'c' : 'colon ',
            \ 'o' : 'circ ',
            \ 'm' : 'mapsto ',
            \ '-' : 'setminus ',
            \ 'x' : 'times ',
            \ '(' : 'preceq ',
            \ ')' : 'succeq ',
            \ '9' : 'prec ',
            \ '0' : 'succ ',
            \ '=' : 'equiv ',
            \ '^' : 'partial ',
            \ '&' : 'infty ',
            \ 'w' : 'wedge ',
            \ 'r' : 'restriction ',
            \ 'n' : 'notin ',
            \ 'i' : 'emph{',
            \ ';' : 'models ',
            \ 'l' : 'ell ',
            \ 'g' : 'lit{',
            \ 'v' : 'vee ',
            \ "'" : 'enquote{',
            \ 'si' : 'implies ',
            \ 'ss' : 'substack{',
            \ 'sa' : 'forall ',
            \ 'se' : 'exists ',
            \ 'so' : 'operatorname{',
            \ 'sd' : 'mathrm{d}',
            \ 'st' : 'text{',
            \ 'dl' : 'ldots ',
            \ 'dc' : 'cdots ',
            \ 'dv' : 'vdots ',
            \ 'dd' : 'ddots ',
            \ 'ds' : 'cdot ',
            \ 'b+' : 'bigoplus ',
            \ 'b*' : 'bigotimes ',
            \ 'bx' : 'bigtimes ',
            \ 'bu' : 'bigcup ',
            \ 'bi' : 'bigcap ',
            \ 'ba' : 'bigcap ',
            \ 'bw' : 'bigwedge ',
            \ 'bv' : 'bigvee ',
            \ 'bs' : 'bigsqcup ',
            \ 'al' : 'overline{',
            \ 'at' : 'widetilde{',
            \ 'ah' : 'widehat{',
            \ 'ab' : 'overbrace{',
            \ 'aa' : 'overrightarrow{',
            \ 'ul' : 'underline{',
            \ 'ub' : 'underbrace{',
            \ 'qu' : 'uparrow ',
            \ 'qr' : 'rightarrow ',
            \ 'qd' : 'downarrow ',
            \ 'ql' : 'leftarrow ',
            \ 'qb' : 'leftrightarrow ',
            \ 'qsr' : 'rightsquigarrow ',
            \ 'qtr' : 'twoheadrightarrow ',
            \ 'qtl' : 'twoheadleftarrow ',
            \ 'qhr' : 'hookrightarrow ',
            \ 'qhl' : 'hookleftarrow ',
            \ 'qxr' : 'xrightarrow{',
            \ 'qxl' : 'xleftarrow{',
            \ 'qxhr' : 'xhookrightarrow{',
            \ 'qxhl' : 'xhookleftarrow{',
            \ }
for [key, mapping] in items(command_maps)
    execute printf('inoremap <buffer> %s \%s', command_leader . key, mapping)
endfor
execute printf('inoremap <buffer> %s %s', command_leader . 't', '<CR>\item<CR>')

let letter_maps = {
            \ 'a': 'alpha ',
            \ 'b': 'beta ',
            \ 'c': 'chi ',
            \ 'd': 'delta ',
            \ 'e': 'epsilon ',
            \ 'f': 'phi ',
            \ 'g': 'gamma ',
            \ 'h': 'eta ',
            \ 'i': 'iota ',
            \ 'k': 'kappa ',
            \ 'l': 'lambda ',
            \ 'm': 'mu ',
            \ 'n': 'nu ',
            \ 'p': 'pi ',
            \ 'q': 'theta ',
            \ 'r': 'rho ',
            \ 's': 'sigma ',
            \ 't': 'tau ',
            \ 'y': 'psi ',
            \ 'u': 'upsilon ',
            \ 'w': 'omega ',
            \ 'z': 'zeta ',
            \ 'x': 'xi ',
            \ 'G': 'Gamma ',
            \ 'D': 'Delta ',
            \ 'F': 'Phi ',
            \ 'L': 'Lambda ',
            \ 'P': 'Pi ',
            \ 'Q': 'Theta ',
            \ 'S': 'Sigma ',
            \ 'U': 'Upsilon ',
            \ 'W': 'Omega ',
            \ 'X': 'Xi ',
            \ 'Y': 'Psi ',
            \ ';': 'aleph ',
            \ }
for [key, mapping] in items(letter_maps)
    execute printf('inoremap <buffer> %s \%s', letter_leader . key, mapping)
endfor
