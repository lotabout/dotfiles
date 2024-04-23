" fix Cmd-v paste in command mode
" https://github.com/equalsraf/neovim-qt/issues/699
cnoremap <D-v> <C-r>+
nnoremap <D-v> <C-\><C-n>"+p
inoremap <D-v> <C-r>+

" Set Editor Font
if exists(':GuiFont')
    " Use GuiFont! to ignore font errors
    GuiFont JetBrains Mono:h16
    GuiRenderLigatures 1
endif

" ginit.vim
if exists(':GuiClipboard')
  call GuiClipboard()
endif
