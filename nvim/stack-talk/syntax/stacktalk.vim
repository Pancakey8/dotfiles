" TODO: Figure out how to show nested JS as syntax highlighted
" syntax include @JavaScript syntax/javascript.vim



syntax keyword Operator + - * div eq? gt? lt? and or not -- -/- js:
" Numbers
syntax match stacktalkNumber "\v<\d+>"
syntax match stacktalkNumber "\v<\d*.\d+>"
syntax match stacktalkNumber "\v<\d+.\d*>"

" Stack-ops
syntax match stacktalkStackOp "\v\S+\."
syntax match stacktalkStackOp "\v\>\S+"
syntax match stacktalkStackOp "\v\S+\>"


" Booleans
syntax match stacktalkBool "\v<true>"
syntax match stacktalkBool "\v<false>"

" Warp
syntax match stacktalkWarp "\v\S+\[@="

" Private
syntax match stacktalkPriv "\v#[^: \t\r\n]+"

" Strings
syntax match stacktalkAtom "\v\S+\:@="
syntax region stacktalkLongString start=/\v"/ skip=/\\"/ end=/"/
syntax region stacktalkJsString start=/\v(js: )@<="/ skip=/\v\\"/ end=/\v"/ " contains=@JavaScript

" Splitwords
syntax match stacktalkSplitword "\v\:\S+"
syntax match stacktalkSplitword "\v\|"

" Comment
syntax region stackTalkComment start=/\vNB\[/  end=/\v\]/ contains=stackTalkComment


highlight link stacktalkNumber Number
highlight link stacktalkString String
highlight link stacktalkLongString String
highlight link stacktalkJsString PreProc
highlight link stacktalkSplitword Identifier
highlight link stacktalkAtom String
highlight link stacktalkBool Boolean
highlight link stacktalkStackOp Statement
highlight link stacktalkWarp Special
highlight link stacktalkPriv Special
highlight link stacktalkComment Comment
highlight link stacktalkCommentQuotation Comment
