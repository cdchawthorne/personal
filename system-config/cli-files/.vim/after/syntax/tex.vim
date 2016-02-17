call TexNewMathZone("cea","ea",0)
call TexNewMathZone("ctcd","tcd",0)
call TexNewMathZone("ccea","IEEEeqnarray",1)

" syntax files won't load with b:current_syntax set
unlet b:current_syntax
syn include @LUA syntax/lua.vim

syn region luatexSnip matchgroup=Snip
    \ start='\\begin{\z(luacode\|luacode*\)}'
    \ end='\\end{\z1}'
    \ contains=@LUA

syn region luatexSnip matchgroup=Snip
    \ start='\\\(directlua\|luadirect\|luaexec\){'
    \ end='}'
    \ contains=@LUA
    \ containedin=ALLBUT,texComment

syn region luatexSnip matchgroup=Snip
    \ start='\\begin{filecontents\*}{.*\.lua}'
    \ end='\\end{filecontents\*}'
    \ contains=@LUA
    \ containedin=ALLBUT,texComment

highlight link Snip SpecialComment
