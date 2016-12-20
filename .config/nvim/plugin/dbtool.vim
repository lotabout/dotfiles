" helper function for tranlating dbscripts format and dbtool format

if !exists("g:loaded_dbtool")
    let g:loaded_dbtool= 1
endif

if g:loaded_dbtool == 0
    finish
endif

function! s:quote_component(str)
    if match(a:str, '^\d\+$') < 0
        " not a pure number
        return '"' . a:str . '"'
    endif
    return a:str
endfunction

function! s:unquote_component(str)
    return substitute(a:str, '^"\(.*\)"$', '\1', '')
endfunction

function! ToDBScript()
    let l:columns = split(getline('.'), '\t')
    if !empty(l:columns)
        call map(l:columns, string(function('s:quote_component')) . '(v:val)')
        call setline('.', join(l:columns, ','))
    endif
endfunction

function! ToDBTool()
    let l:columns = split(getline('.'), ',')
    if !empty(l:columns)
        call map(l:columns, string(function('s:unquote_component')) . '(v:val)')
        call setline('.', join(l:columns, "\t"))
    endif
endfunction

" add corresponding commands
command! -range Todbscript <line1>,<line2>call ToDBScript()
command! -range Todbtool <line1>,<line2>call ToDBTool()
