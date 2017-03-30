""
" Some convenient Emacs bindings for insert mode

if !exists("g:i_emacs_loaded")
    let g:i_emacs_loaded = 1
endif


" set g:i_emacs_loaded = 0 to disable it.
if g:i_emacs_loaded == 0
    finish
endif

"=============================================================================
" ==> Mappings. <==
"=============================================================================


"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Command line editing
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Navigation
cmap <C-b> <Left>
cmap <C-f> <Right>
cnoremap <M-f> <S-Right>
cnoremap <M-b> <S-Left>
cmap <C-a> <Home>
cmap <C-e> <End>

" Editing
cmap <M-p> <Up>
cmap <M-n> <Down>
cmap <C-d> <Del>
cnoremap <C-y> <C-r><C-o>"
cnoremap <C-k> <C-f>d$<C-c><End>
cnoremap <M-d> <C-o>de

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Insert mode mapping
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

" Navigation
imap <C-b> <Left>
imap <C-f> <Right>
imap <C-a> <Home>
imap <C-e> <End>

inoremap <M-f> <S-Right>
inoremap <M-b> <S-Left>

" Editing
imap <C-d> <Del>
inoremap <M-d> <C-o>de
inoremap <C-s> <C-o>n
inoremap <silent> <C-k> <C-r>=<SID>KillLine()<CR>

function! <SID>KillLine()
    if col('.') > strlen(getline('.'))
        " At EOL; join with next line
        return "\<Del>"
    else
        " Not at EOL; kill until end of line
        return "\<C-o>d$"
    endif
endfunction
