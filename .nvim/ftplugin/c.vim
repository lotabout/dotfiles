" Settings for python

if exists("g:my_c_settings_loaded")
    finish
endif
let g:my_c_settings_loaded = 1

if exists("g:enable_my_c_config") && g:enable_my_c_config == 0
    finish
endif

if exists("*SuperTabSetDefaultCompletionType")
    call SuperTabSetDefaultCompletionType("<c-x><c-u>")
endif
