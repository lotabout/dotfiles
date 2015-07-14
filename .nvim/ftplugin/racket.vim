" Language: Racket

if exists("g:enable_my_racket_config")
    finish
endif

set dictionary-=/usr/share/dict/words
set dictionary+=~/.vim/dict/racket/racket-dict.vim
setl iskeyword+=#,%,^
setl lisp
set complete+=k
setl comments=:;
setl comments^=:;;;,:;;,sr:#\|,mb:\|,ex:\|#
setl commentstring=;\ %s
setl formatoptions+=crql
setl textwidth=102
setl colorcolumn=103
setl indentexpr=RacketIndent(v:lnum)


let s:skip_sc = 'synIDattr(synID(line("."), col("."), 0), "name") =~ "[Ss]tring\\|[Cc]omment"'
let s:Auto_prefix = ['def', 'with-', 'let', 'match']
let s:racket_indent_maxlines = 50
" Handle indent with certain prefix
function! RacketIndent(lnum)
    let oldpos = winsaveview()
    let backline = max([a:lnum-s:racket_indent_maxlines, 1])

    normal! 0
    " Find containing form
    let [lhead1, chead1] = searchpairpos( '(', '', ')', 'bW', s:skip_sc, backline )
    " search for []
    call winrestview( oldpos )
    let [lhead2, chead2] = searchpairpos( '\[', '', '\]', 'bW', s:skip_sc, backline )

    if lhead1 > lhead2
        let lhead = lhead1
        let chead = chead1
    elseif lhead1 == lhead2
        let lhead = lhead1
        if chead1 > chead2
            let chead = chead1
        else
            let chead = chead2
        endif
    else
        let lhead = lhead2
        let chead = chead2
    endif
    " echomsg 'lhead = ' . lhead . ' chead = ' . chead
    if lhead == 0
        call winrestview( oldpos )
        return lispindent(a:lnum)
    endif

    let s:search_pat = '[^ \t\n]'
    " echomsg 'search_pat = ''' . s:search_pat . ''''

    call cursor(lhead, chead)
    call search(s:search_pat)
    let regcontent = @"
    silent! exec "normal! ye"
    let key = @"
    let @" = regcontent

    "echomsg 'key = ' . key

    if (match(&lispwords, key) == -1 && key[0] =~ '[[:alpha:]]')
        " valid key-word, check prefix
        for pref in s:Auto_prefix
            if match(key, pref, 0) != -1
                let &lispwords .= ',' . key
            endif
        endfor
    endif

    call winrestview( oldpos )
    return lispindent(a:lnum)
endfunction

" set lispwords
setl lispwords-=if
setl lispwords+=call-with-break-parameterization
setl lispwords+=call-with-composable-continuation
setl lispwords+=call-with-continuation-barrier
setl lispwords+=call-with-continuation-prompt
setl lispwords+=call-with-current-continuation
setl lispwords+=call-with-escape-continuation
setl lispwords+=call-with-exception-handler
setl lispwords+=call-with-immediate-continuation-mark
setl lispwords+=call-with-input-file
setl lispwords+=call-with-input-file*
setl lispwords+=call-with-output-file
setl lispwords+=call-with-output-file*
setl lispwords+=call-with-parameterization
setl lispwords+=call-with-semaphore
setl lispwords+=call-with-semaphore/enable-break
setl lispwords+=call-with-values
setl lispwords+=call/cc
setl lispwords+=call/ec
setl lispwords+=define
setl lispwords+=define-for-syntax
setl lispwords+=define-logger
setl lispwords+=define-namespace-anchor
setl lispwords+=define-sequence-syntax
setl lispwords+=define-struct
setl lispwords+=define-struct/derived
setl lispwords+=define-syntax
setl lispwords+=define-syntax-rule
setl lispwords+=define-syntaxes
setl lispwords+=define-values
setl lispwords+=define-values-for-syntax
setl lispwords+=for
setl lispwords+=for*
setl lispwords+=for*/and
setl lispwords+=for*/first
setl lispwords+=for*/fold
setl lispwords+=for*/fold/derived
setl lispwords+=for*/hash
setl lispwords+=for*/hasheq
setl lispwords+=for*/hasheqv
setl lispwords+=for*/last
setl lispwords+=for*/list
setl lispwords+=for*/lists
setl lispwords+=for*/or
setl lispwords+=for*/product
setl lispwords+=for*/sum
setl lispwords+=for*/vector
setl lispwords+=for*/bit-vector
"setl lispwords+=for-each
"setl lispwords+=for-label
"setl lispwords+=for-meta
"setl lispwords+=for-template
setl lispwords+=for/and
setl lispwords+=for/first
setl lispwords+=for/fold
setl lispwords+=for/fold/derived
setl lispwords+=for/hash
setl lispwords+=for/hasheq
setl lispwords+=for/hasheqv
setl lispwords+=for/last
setl lispwords+=for/list
setl lispwords+=for/lists
setl lispwords+=for/or
setl lispwords+=for/product
setl lispwords+=for/sum
setl lispwords+=for/vector
setl lispwords+=for/bit-vector
setl lispwords+=let
setl lispwords+=let*
setl lispwords+=let*-values
setl lispwords+=let-syntax
setl lispwords+=let-syntaxes
setl lispwords+=let-values
setl lispwords+=let/cc
setl lispwords+=let/ec
setl lispwords+=letrec
setl lispwords+=letrec-syntax
setl lispwords+=letrec-syntaxes
setl lispwords+=letrec-syntaxes+values
setl lispwords+=letrec-values
setl lispwords+=with-continuation-mark
setl lispwords+=with-handlers
setl lispwords+=with-handlers*
setl lispwords+=with-input-from-file
setl lispwords+=with-output-to-file
setl lispwords+=with-syntax
setl lispwords+=match
setl lispwords+=syntax-parse

