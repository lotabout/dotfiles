"==============================================================================
" markdown specific text objects

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" select code fence
" ref: https://github.com/plasticboy/vim-markdown/issues/282
function! s:SelectFencedCode(type) " type = 'i' or 'a'
    " move cursor to the end of the line, so that '| ```' style fenced code
    " could be matched
    execute 'normal! $'
    let l:saved_pos = getpos('.')

    let l:head = search('\s*```\S*$', 'Wbn')
    let l:tail = search('\s*```$', 'Wcn')
    if !l:head || !l:tail
        setpos('.', l:saved_pos)
        echom "text object: Code fence not found"
        return
    endif

    if a:type ==# 'i'
        let l:head += 1
        let l:tail -= 1
    endif

    let buf_num = bufnr()
    call setpos("'<", [buf_num, l:head, 1, 0])
    call setpos("'>", [buf_num, l:tail, 1, 0])
    execute 'normal! `<V`>'
endfunction

vnoremap <buffer><silent> if :<C-U>call <SID>SelectFencedCode('i')<CR>
onoremap <buffer><silent> if :<C-U>call <SID>SelectFencedCode('i')<CR>
vnoremap <buffer><silent> af :<C-U>call <SID>SelectFencedCode('a')<CR>
onoremap <buffer><silent> af :<C-U>call <SID>SelectFencedCode('a')<CR>

