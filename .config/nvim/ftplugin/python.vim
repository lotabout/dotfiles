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
set colorcolumn=80,120
set textwidth=100

if exists('g:plugs["vim-textobj-python"]') && exists('g:plugs["slimux"]')
    function Slimux_python_eval_defun()
        " backup the position
        let vb = getpos("'<")
        let ve = getpos("'>")
        let pos = getpos(".")
        let quote_reg = @"
        " evaluate the class
        silent! exec "normal yac"
        if @" == ''
            " class not exists, try to evaluate the def form
            silent! exec "normal yaf"
            if @" == ''
                " defun not exists, try current line
                silent! exec "normal yy"
            endif
        endif

        if @" != ''
            call SlimuxSendCode(@" . "\n")
        endif

        let @" = quote_reg
        call setpos(".", pos)
        call setpos("'>", ve)
        call setpos("'<", vb)
    endfunction
    command! SlimuxPythonEvalDefun call Slimux_python_eval_defun()
    " bind it to Control-Enter
    nmap <buffer> <C-M> :SlimuxPythonEvalDefun<CR>
endif
