" Settings for python

if exists("g:my_python_settings_loaded")
    finish
endif
let g:my_python_settings_loaded = 1

if exists("g:enable_my_python_config") && g:enable_my_python_config == 0
    finish
endif

set tabstop=8
set expandtab
set cindent
set softtabstop=4
set shiftwidth=4
set formatoptions-=c formatoptions-=o formatoptions-=r

" enable modeline
set modeline

" hightlight column 80
set colorcolumn=80
