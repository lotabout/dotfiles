" Language: Clojure

if exists("g:enable_my_clojure_config")
    finish
endif

"---------------------------------------------------------------------
" SuperTab

" support for clojure
let b:SuperTabCompletionContexts =
  \ ['ClojureContext'] + g:SuperTabCompletionContexts

function! ClojureContext()
  let curline = getline('.')
  let cnum = col('.')
  let synname = synIDattr(synID(line('.'), cnum - 1, 1), 'name')
  if curline =~ '(\S\+\%' . cnum . 'c' && synname !~ '\(String\|Comment\)'
    return "\<c-x>\<c-o>"
  endif
endfunction

"---------------------------------------------------------------------
" Fireplace

" Evaluate a clojure 'define' statement
function! clojure#eval_helper(motion_string, action_string)
    let pos = getpos(".")
    let regContent = @"
    let s:skip_sc = 'synIDattr(synID(line("."), col("."), 0), "name") =~ "[Ss]tring\\|[Cc]omment"'
    let [lhead, chead] = searchpairpos( '(', '', ')', 'bW', s:skip_sc)
    call cursor(lhead, chead)
    silent! exec a:motion_string
    exec a:action_string
    " restore contents
    let @" = regContent
    call setpos('.', pos)
endfunction

function! clojure#eval_defn()
    call clojure#eval_helper("normal! 99[(", "normal cpp")
endfunction

function! clojure#eval_buffer()
    call clojure#eval_helper("normal! gg", "normal cpG")
endfunction

if exists("g:loaded_fireplace")
    " add keybindings for fireplace
    noremap <buffer> <silent> <leader>d :Eval<CR>
    noremap <buffer> <silent> <leader>b :%Eval<CR>
endif
