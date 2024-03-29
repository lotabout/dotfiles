"===============================================================================
" General Settings

" Inform that the script is written in UTF-8 (including special characters)
set encoding=utf-8
scriptencoding utf-8

if has('nvim')
    let s:vimvariant = "nvim"
    let $VIMHOME = expand('~/.config/nvim/')
else
    let s:vimvariant = "vim"
    let $VIMHOME = expand('~/.vim/')
endif

if has("termguicolors")
    " vim need this
    " check https://github.com/vim/vim/issues/981#issuecomment-241941032
    set t_8f=[38;2;%lu;%lu;%lum
    set t_8b=[48;2;%lu;%lu;%lum

    " enable true color
    set termguicolors
endif

" Use Vim settings instead of vi settings.
set nocompatible

" Set how many lines of history VIM has to remember
if &history < 1000
  set history=1000
endif

if &tabpagemax < 50
  set tabpagemax=50
endif

" Enable filetype plugins
filetype plugin indent on

" Set to auto read when a file is changed from the outside
set autoread

" Set map leader
let mapleader = "\<Space>"
let g:mapleader = "\<Space>"

" Show line number
set nu

" don't treat numbers as octal when performing Ctrl-A and Ctrl-X
set nrformats-=octal

" enable mouse support
set mouse=a

"----------------------------------------------------------------------
" VIM user interface

" Turn on the Wild menu
set wildmenu
set wildmode=longest,list,full

" Ignore certain files and folders when globbing
set wildignore+=*~
set wildignore+=*.o,*.obj,*.bin,*.dll,*.exe
set wildignore+=*/.git/*,*/.svn/*,*/__pycache__/*,*/build/**
set wildignore+=*.pyc
set wildignore+=*.DS_Store
set wildignore+=*.aux,*.bbl,*.blg,*.brf,*.fls,*.fdb_latexmk,*.synctex.gz,*.pdf

" Always show current position
set ruler

" A buffer becomes hidden when it is abandoned
set hid

" Congifure backspace so it acts as it should act
set backspace=eol,start,indent

" When searching try to be smart about cases
set ignorecase
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

" Show cursor line
set cursorline

" Always show the status line
set laststatus=2
"set stl=%F%y%m\ [%l,%c,%p%%]\ [%n/%{len(filter(range(1,bufnr('$')),'buflisted(v:val)'))}]

" Do not add two spaces after a period when joining lines or formatting texts,
" see https://stackoverflow.com/q/4760428/6064933
set nojoinspaces

" disable syntax highlighting for lone lines
set synmaxcol=4096

" set showtabline=2
set completeopt=longest,menuone,preview

" Virtual edit is useful for visual block edit
set virtualedit=block

" neovim >= 0.17, preview the substitution like :%s/foo/bar/g
if exists('&inccommand')
    set inccommand=nosplit
endif

"----------------------------------------------------------------------
" Files, backups and undo

" Set possible file encodings
"set fileencodings=ucs-bom,utf-8,cp936,gb18030,utf-16,big5,euc-jp,euc-kr,latin1
set fileencodings=ucs-bom,utf-8,cp932,cp936,gb18030,big5,euc-jp,euc-kr,latin1

" Use Unix as the standard file type
set ffs=unix,dos,mac

" Change backup directory to a less annoying place under linux.
if has("unix")
    let s:backupdir = '.' . s:vimvariant . '-backup'
    let s:backuppath = $HOME . '/' . s:backupdir
    if isdirectory(s:backuppath) == 0
        execute ':silent !mkdir -p ' . s:backuppath . ' > /dev/null 2>&1'
    endif

    set backupdir-=.
    set backupdir+=.
    set backupdir-=~/
    execute 'set backupdir^=' . s:backuppath
    execute 'set backupdir^=./' . s:backupdir

    let s:swapdir = '.' . s:vimvariant . '-swap'
    let s:swappath = $HOME . '/' . s:swapdir
    if isdirectory(s:swappath) == 0
        execute ':silent !mkdir -p ' . s:swappath . ' > /dev/null 2>&1'
    endif
    execute 'set directory=./' . s:swapdir
    execute 'set directory+=' . s:swappath
    set directory+=~/tmp//
    set directory+=.

    if exists('+undofile')
        let s:undopath = $HOME.'/.' . s:vimvariant . '-undo'
        if isdirectory(s:undopath) == 0
            execute ':silent !mkdir -p ' . s:undopath . ' > /dev/null 2>&1'
        endif

        execute 'set undodir=' . s:undopath
        set undofile
    endif

endif

"----------------------------------------------------------------------
" Text, tab and indent related

" Use spaces instead of tabs
set expandtab

" Be smart when using tabs
set smarttab

" 1 tab == 8 spaces
set tabstop=8
set shiftwidth=4
set softtabstop=4
set textwidth=78

set ai " Auto Indent
set si " Smart indent
set wrap " Wrap lines
set cinoptions=l1,t0,g0,Ws

" Dictionary completion
set dictionary+=/usr/share/dict/words

"----------------------------------------------------------------------
" Moving around, tabs, windows and buffers

" Treat long lines as break lines(like emacs)
noremap j gj
noremap gj j
noremap k gk
noremap gk k

" swap these too because ' is easier to type and ` is what I want
noremap ' `
noremap ` '

" Close current buffer
" need vim-bbye
nmap <leader>q :Bdelete<cr>

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

au FileType crontab setlocal backupcopy=yes

"===============================================================================
" settings for manually installed plugins


"----------------------------------------------------------------------
" vim-solarized8
let g:solarized_extra_hi_groups = 1

"----------------------------------------------------------------------
" vimim

if has ("win32")
    set guifont=Courier_New:h16:w7
    set guifontwide=NSimSun-18030,NSimSun
endif

"===============================================================================
" package manager settings
let package_manager = "vim-plug"

if package_manager == "vim-plug"
    call plug#begin($VIMHOME . 'plugged')

    "------------------------------------------------------------------
    " UI enhancement

    Plug 'lifepillar/vim-solarized8'

    " powerline alternative; for better status line
    Plug 'itchyny/lightline.vim'

    Plug 'majutsushi/tagbar'
    Plug 'lvht/tagbar-markdown', {'for': 'markdown'}

    Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
    Plug 'Xuyuanp/nerdtree-git-plugin'

    Plug 'mhinz/vim-startify'

    Plug 'lotabout/vim-signature' " show sign for native marks

    if has('nvim')
        Plug 'lukas-reineke/indent-blankline.nvim' " neovim 0.5
    endif

    "------------------------------------------------------------------
    " Basic feature enhancement

    Plug 'moll/vim-bbye'
    " Plug 'ervandew/supertab'    " you'll need it

    Plug 'tpope/vim-abolish' " Enhance `s` command
    Plug 'lambdalisue/suda.vim', {'on': ['SudaRead', 'SudaWrite']} " for vim's sudo tee trick

    Plug 'kana/vim-textobj-user'
    Plug 'kana/vim-textobj-indent'

    Plug 'wellle/targets.vim'

    "------------------------------------------------------------------
    " Completion Framework

    " Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'github/copilot.vim'

    Plug 'neovim/nvim-lspconfig'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'hrsh7th/nvim-cmp' " deps of obsidian.nvim
    Plug 'quangnguyen30192/cmp-nvim-ultisnips'

    "------------------------------------------------------------------
    " Handy commands

    Plug 'junegunn/vim-easy-align', {'on': '<Plug>(EasyAlign)'}
    Plug 'sbdchd/neoformat', {'on': 'Neoformat'} " enhance the format function (press '=' key)

    Plug 'sjl/gundo.vim', {'on': 'GundoToggle'}

    Plug 'schickling/vim-bufonly' " close all buffers except current one

    " work with git
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-rhubarb' " for Gbrowser with github
    Plug 'shumphrey/fugitive-gitlab.vim' " for Gbrowser with gitlab
    Plug 'airblade/vim-gitgutter'

    Plug 'will133/vim-dirdiff', {'on': 'DirDiff'}
    Plug 'rootkiter/vim-hexedit'

    Plug 'ggandor/leap.nvim'

    "------------------------------------------------------------------
    " Better defaults

    Plug 'vim-scripts/LargeFile' " disable some features for faster opening large files.

    "------------------------------------------------------------------
    " Integration with Linux environment

    Plug 'lotabout/slimux', {'on': ['SlimuxREPLSendLine', 'SlimuxREPLSendSelection'],
                \ 'for': ['python', 'clojure']}
    Plug 'kana/vim-fakeclip'

    Plug 'lotabout/skim', { 'dir': '~/.skim', 'do': './install' }
    Plug 'lotabout/skim.vim'
    " Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    " Plug 'junegunn/fzf.vim'

    "------------------------------------------------------------------
    " Support more filetype specific feature

    if has('nvim')
        " neovim 0.5
        Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
        Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    endif

    Plug 'tomtom/tcomment_vim'
    Plug 'SirVer/ultisnips'
    Plug 'honza/vim-snippets'
    Plug 'lotabout/vim-ultisnippet-private' " private snippets

    Plug 'w0rp/ale' " async version of Syntastic

    " in replace of paredit.vim
    Plug 'guns/vim-sexp', {'for': ['clojure', 'scheme', 'racket']}
    Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': ['clojure', 'scheme', 'racket']}

    Plug 'https://github.com/wlangstroth/vim-racket', {'for': 'racket'}

    " for javascript
    Plug 'pangloss/vim-javascript'
    Plug 'mxw/vim-jsx', {'for': 'javascript.jsx'} " for react.js
    Plug 'othree/javascript-libraries-syntax.vim', {'for': 'javascript'}
    "Plug 'mattn/emmet-vim', {'for': ['html', 'xml', 'css', 'nhtml', 'javascript', 'javascript.jsx', 'typescript']}
    Plug 'adriaanzon/vim-emmet-ultisnips', {'for': ['html', 'xml', 'css', 'nhtml', 'javascript', 'javascript.jsx', 'typescript']}

    " for rust
    Plug 'rust-lang/rust.vim', {'for': 'rust'}

    " for markdown
    Plug 'lotabout/vim-markdown', {'for': 'markdown'}
    Plug 'dkarter/bullets.vim', {'for': ['markdown', 'text', 'gitcommit']}
    Plug 'lotabout/orgmark.vim', {'branch': 'main', 'for': ['markdown']}

    " for typescript
    Plug 'HerringtonDarkholme/yats.vim', {'for': 'typescript'}

    " for Julia
    Plug 'JuliaEditorSupport/julia-vim', {'for': 'julia'}

    " for zig
    Plug 'ziglang/zig.vim', {'for': 'zig'}

    "------------------------------------------------------------------
    " Others

    Plug 'christoomey/vim-tmux-navigator'
    Plug 'lotabout/calendar-vim'
    " Plug 'lotabout/ywvim' " Chinese input method
    Plug 'wakatime/vim-wakatime' " time tracking

    Plug 'epwalsh/obsidian.nvim', {'branch': 'main'}
    Plug 'nvim-lua/plenary.nvim' " deps of obsidian.nvim

    call plug#end()
elseif package_manager == "pathogen"
    execute pathogen#infect()
endif

"===============================================================================
" Misc Maps & Functions(for convenience)

" Remove the Windows ^M - when the encodings gets messed up
nnoremap <Leader>mm mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" strip the spaces at the end of line
nnoremap <leader><Space><Space> :%s/\s\+$//<cr>:<C-u>nohlsearch<CR>

" merge multiple continuous lines into one.
nmap <leader><cr> :%s/\(^[[:blank:]]*\n\)\{2,}/\r/<cr>

" for neovim to integrate with tmux/iterm2, copy over ssh
if executable('clipboard-provider')
    " checkout https://github.com/agriffis/skel/blob/master/neovim/bin/clipboard-provider
    let g:clipboard = {
          \ 'name': 'myClipboard',
          \     'copy': {
          \         '+': 'clipboard-provider copy',
          \         '*': 'env COPY_PROVIDERS=tmux clipboard-provider copy',
          \     },
          \     'paste': {
          \         '+': 'clipboard-provider paste',
          \         '*': 'env COPY_PROVIDERS=tmux clipboard-provider paste',
          \     },
          \ }
endif

" quick access to system's clipboard
vmap <leader>y "+y
vmap <leader>Y "+Y
nmap <leader>y "+y
nmap <leader>Y "+Y
vmap <leader>p "+p
vmap <leader>P "+P
nmap <leader>p "+p
nmap <leader>P "+P

" quick save
nmap <leader>w :<C-u>nohlsearch<CR>:w<CR>

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" execute macro on every selected/visual lines

xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" zoom

function! Zoom ()
    " check if is the zoomed state (tabnumber > 1 && window == 1)
    if tabpagenr('$') > 1 && tabpagewinnr(tabpagenr(), '$') == 1
        let l:cur_winview = winsaveview()
        let l:cur_bufname = bufname('')
        tabclose

        " restore the view
        if l:cur_bufname == bufname('')
            call winrestview(cur_winview)
        endif
    else
        tab split
    endif
endfunction

nmap <leader>z :call Zoom()<CR>

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Interleave lines, do not support overlapping
" Usage: 90, 100call Interleave(1)

function! Interleave(where) range
    let l:where = a:where

    let l:pos = getpos(l:where)
    if l:where =~ "^'" && !empty(l:pos)
        let l:where = l:pos[1]
    endif

    let l:start = a:firstline
    let l:end = a:lastline

    if l:start < a:where
        for i in range(0, l:end - l:start)
            execute l:start . 'm' . (l:where + i)
        endfor
    else
        for i in range(l:end - l:start, 0, -1)
            execute l:end . 'm' . (l:where + i)
        endfor
    endif
endfunction

" Usage: 90,100Interleave 10
" or     '<,'>Interleave 'a  " will use mark 'a as the target
command! -nargs=1 -range Interleave <line1>,<line2>call Interleave("<args>")
vmap <leader>j :Interleave<space>

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Open all files in quickfix window
" Author: Tim Dahlin
function! QuickFixOpenAll()
    if empty(getqflist())
        return
    endif
    let s:prev_val = ""
    for d in getqflist()
        let s:curr_val = bufname(d.bufnr)
        if (s:curr_val != s:prev_val)
            exec "edit " . s:curr_val
        endif
        let s:prev_val = s:curr_val
    endfor
endfunction

command! QuickFixOpenAll call QuickFixOpenAll()

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" toggle wrap

function! ToggleWrap()
    if &wrap
        echo "Wrap Off"
        setlocal nowrap
        set virtualedit=all
    else
        echo "Wrap On"
        setlocal wrap
        set virtualedit=block
    endif
endfunction

command! ToggleWrap call ToggleWrap()

"===============================================================================
" Settings for Programming

"----------------------------------------------------------------------
" Extra Settings

set formatoptions=tclqronmM
if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j " Delete comment character when joining commented lines
endif

" Path extra will enable Up/downwards search in 'path' and 'tags'
if has('path_extra')
  setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

"----------------------------------------------------------------------
" Extra Mappings
" borrowed from vim-unimpaired

nmap <silent> [a :prev<CR>
nmap <silent> ]a :next<CR>
nmap <silent> [A :first<CR>
nmap <silent> ]A :last<CR>

" buffer related
nmap <silent> [b :bprev<CR>
nmap <silent> ]b :bnext<CR>
nmap <silent> [B :bfirst<CR>
nmap <silent> ]B :blast<CR>

" quickfix related
nmap <silent> [q :cprev<CR>
nmap <silent> ]q :cnext<CR>
nmap <silent> [Q :cfirst<CR>
nmap <silent> ]Q :clast<CR>
nmap <silent> [l :lprev<CR>
nmap <silent> ]l :lnext<CR>
nmap <silent> [L :lfirst<CR>
nmap <silent> ]L :llast<CR>

" tab related
nmap <silent> [t :tabprev<CR>
nmap <silent> ]t :tabnext<CR>
nmap <silent> [T :tabfirst<CR>
nmap <silent> ]T :tablast<CR>

" disable highlight search for current search
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" visual mode: star-search
xnoremap * :<C-u>call <SID>VSetSearch()<CR>/<C-R>=@/<CR><CR>
xnoremap # :<C-u>call <SID>VSetSearch()<CR>?<C-R>=@/<CR><CR>

function! s:VSetSearch()
    let tmp = @s
    norm! gv"sy
    let @/ = '\V' . substitute(escape(@s, '/\'), '\n', '\\n', 'g')
    let @s = tmp
endfunction

" Repeating last substitution
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" select last pasted texts
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

" Search in a Region
" vnoremap / <Esc>/\%><C-R>=line("'<")-1<CR>l\%<<C-R>=line("'>")+1<CR>l
" vnoremap ? <Esc>?\%><C-R>=line("'<")-1<CR>l\%<<C-R>=line("'>")+1<CR>l

" Scroll without moving cursor screen line
nnoremap <C-J> <C-E>j
vnoremap <C-J> <C-E>j
nnoremap <C-K> <C-Y>k
vnoremap <C-K> <C-Y>k

" https://dev.to/jovica/the-vim-trick-which-will-save-your-time-and-nerves-45pg
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" neovim makes Y = y$, why?
nmap Y yy

"===============================================================================
" settings for bundle plugins

"----------------------------------------------------------------------
" Colors and Fonts

" Enable syntax highlighting
syntax enable

" Colorscheme
" if has("gui_running")
"     set guifont=Fira\ Code:h16,Monaco:h16,Dejavu\ Sans\ Mono\ for\ Powerline:h16,Dejavu\ Sans\ Mono:h16
" endif

set background=dark
if &t_Co >= 256 || has("gui_running")
    colorscheme solarized8
elseif &t_Co >= 16
    let g:solarized_use16 = 1
    colorscheme solarized8
else
    colorscheme desert
    if &term == "linux"
        colorscheme desert
    endif
endif

" Set extra options when running in GUI mode
if has("gui_running")
    "set guioptions-=m " remove menubar
    set guioptions-=T " remove toolbar
    set guioptions-=r " remove right-hand scroll bar
    set guioptions-=l " remove left-hand scroll bar
    set guioptions+=e
    set guitablabel=%M\ %t
endif

"----------------------------------------------------------------------
" supertab

if exists('g:plugs["supertab"]')
    let g:SuperTabLongestEnhanced = 1
    let g:SuperTabDefaultCompletionType = "context"
    let g:SuperTabCrMapping = 1
endif

"----------------------------------------------------------------------
" vim-easy-align
if exists('g:plugs["vim-easy-align"]')
    xmap ga <Plug>(EasyAlign)
endif

"----------------------------------------------------------------------
" slimux

if exists('g:plugs["slimux"]')
    let g:slimux_select_from_current_window = 1
    map <Leader>s :SlimuxREPLSendLine<CR>
    vmap <Leader>s :SlimuxREPLSendSelection<CR>

    let g:slimux_scheme_keybindings=1
    let g:slimux_scheme_leader=';'
    let g:slimux_racket_keybindings=1
    let g:slimux_racket_leader=';'
    let g:slimux_racket_xrepl=1
    let g:slimux_clojure_keybindings=1
    let g:slimux_clojure_leader=';'
    let g:slimux_clojure_xrepl=1
    let g:slimux_python_use_ipython=1
endif

"----------------------------------------------------------------------
" NerdTree
if exists('g:plugs["nerdtree"]')
    nmap <silent> <leader>ne :NERDTreeToggle<cr>
    nmap <silent> <leader>nf :NERDTreeFind<cr>
endif

"----------------------------------------------------------------------
" Tagbar

if exists('g:plugs["tagbar"]')

    au FileType markdown let g:tagbar_sort = 0

    nmap <silent> <leader>tl :TagbarToggle<cr>
endif

"---------------------------------------------------------------------
" indent-blankline.nvim

if exists("g:plugs['indent-blankline.nvim']")
    let g:indent_blankline_show_first_indent_level = v:false
    let g:indent_blankline_show_trailing_blankline_indent = v:false

    if exists('g:plugs["nvim-treesitter"]')
        let g:indent_blankline_use_treesitter = v:true
        let g:indent_blankline_show_current_context = v:true
    endif
    highlight IndentBlanklineChar guifg=#063738 gui=nocombine
endif

"---------------------------------------------------------------------
" LargeFile

if exists("g:plugs['LargeFile']")
    let g:LargeFile = 10
endif

"---------------------------------------------------------------------
" fakeclip
if exists("g:plugs['vim-fakeclip']")
    let g:fakeclip_terminal_multiplexer_type = "tmux"
endif

"---------------------------------------------------------------------
" UltiSnips

if exists("g:plugs['ultisnips']")
    let g:UltiSnipsSnippetsDir = $VIMHOME . "plugged/vim-ultisnippet-private/UltiSnips"
    " load UltiSnips lazily
    augroup load_ultisnip
        autocmd!
        autocmd InsertEnter * call plug#load('ultisnips')
                    \| autocmd! load_ultisnip
    augroup END
endif

"---------------------------------------------------------------------
" fzf.vim

" change keybinding for AG
if executable('fzf') && exists('g:plugs["fzf.vim"]')
    let $FZF_DEFAULT_OPTS = '--bind ctrl-f:toggle'
    " replace Ctrl-p
    nmap <C-p> :Files<CR>

    " Customized binding for AG
    nnoremap <leader>/ :Ag<CR>

    " Replace Bufexplore
    nmap <C-e> :Buffers<CR>

    " select mapping
    nmap <leader><tab> <plug>(fzf-maps-n)
    xmap <leader><tab> <plug>(fzf-maps-x)
    omap <leader><tab> <plug>(fzf-maps-o)
endif

"---------------------------------------------------------------------
" skim.vim
if exists('g:plugs["skim.vim"]')
    let g:skim_history_dir = "~/.skim-history"

    let $SKIM_DEFAULT_COMMAND = '(fd --type f || git ls-files -c -o --exclude-standard || rg -l "" || ag -l -g "" || find . -type f)'
    let $SKIM_DEFAULT_OPTIONS = '--bind ctrl-f:toggle,ctrl-p:up,ctrl-n:down,up:previous-history,down:next-history,alt-h:scroll-left(5),alt-l:scroll-right(5),alt-p:toggle-preview'

    " replace Ctrl-p
    nmap <C-p> :Files<CR>

    " Customized binding for Rg
    nnoremap <leader>/ :Rg<CR>

    nnoremap <leader>i :BTags<CR>
    nnoremap <leader>I :Tags<CR>

    " Replace Bufexplore
    nmap <C-e> :Buffers<CR>

    if exists('*nvim_open_win')
        let $SKIM_DEFAULT_OPTIONS .= ' --layout=reverse'
        let g:fzf_preview_window = 'down:50%'
    endif
endif

"---------------------------------------------------------------------
" Gundo
if exists('g:plugs["gundo.vim"]')
    let g:gundo_prefer_python3 = 1
    nnoremap <F5> :GundoToggle<CR>
endif

"---------------------------------------------------------------------
" Personal wiki
let g:wiki_directory = $HOME . '/Nextcloud/Vault'

function! CreateMappingForPersonalWiki()
    execute("nmap <buffer> <C-p> :Files ".g:wiki_directory."<CR>")
    execute("nmap <buffer> <leader>/ :Rg ".g:wiki_directory."<CR>")
    execute("nmap <buffer> <C-n> :e ".g:wiki_directory."/")
endfunction

function! BindForWikiFiles()
    if expand('%:p') =~ '^'. g:wiki_directory
        call CreateMappingForPersonalWiki()
    endif
endfunction

au FileType markdown call BindForWikiFiles()
au FileType startify call CreateMappingForPersonalWiki()

command! OpenInBrowser :silent !open -a Google\ Chrome %

if exists('g:plugs["vimwiki"]')
    let g:vimwiki_use_calendar = 0
    let g:vimwiki_conceallevel = 0

    " turn off insert mode mappings
    let g:vimwiki_global_ext = 0
    let g:vimwiki_table_mappings = 0
    let g:vimwiki_ext2syntax = {'.md': 'markdown',
                \ '.mkd': 'markdown',
                \ '.wiki': 'media'}

    let wiki_1 = {}
    let wiki_1.path = g:wiki_directory
    let wiki_1.nested_syntaxes = {'python': 'python',
        \ 'js': 'javascript',
        \ 'bash': 'sh',
        \ 'sh': 'sh',
        \ 'c': 'c',
        \ 'java': 'java',
        \ 'sql': 'sql',
        \ 'rust': 'rust',
        \ 'scheme': 'scheme',
        \ 'racket': 'racket'}
    let wiki_1.syntax = 'markdown'
    let wiki_1.ext = '.md'

    let g:vimwiki_list = [wiki_1]

    " Disable vimwiki mappings (to remove bindings begins with <leader>w)
    nmap <Plug>NoVimwikiIndex <Plug>VimwikiIndex
    nmap <Plug>NoVimwikiTabIndex <Plug>VimwikiTabIndex
    nmap <Plug>NoVimwikiUISelect <Plug>VimwikiUISelect
    nmap <Plug>NoVimwikiDiaryIndex <Plug>VimwikiDiaryIndex
    nmap <Plug>NoVimwikiMakeDiaryNote <Plug>VimwikiMakeDiaryNote
    nmap <Plug>NoVimwikiTabMakeDiaryNote <Plug>VimwikiTabMakeDiaryNote
    nmap <Plug>NoVimwikiDiaryGenerateLinks <Plug>VimwikiDiaryGenerateLinks
    nmap <Plug>NoVimwikiMakeYesterdayDiaryNote <Plug>VimwikiMakeYesterdayDiaryNote
    nmap <Plug>NoVimwikiRenameLink <Plug>VimwikiRenameLink
    nmap <Plug>NoVimwikiDeleteLink <Plug>VimwikiDeleteLink
    nmap <Plug>NoVimwiki2HTMLBrowse <Plug>Vimwiki2HTMLBrowse
    nmap <Plug>NoVimwiki2HTML <Plug>Vimwiki2HTML
    nmap <Plug>NoVimwikiNormalizeLinkVisual <Plug>VimwikiNormalizeLinkVisual
    nmap <Plug>NoVimwikiNormalizeLink <Plug>VimwikiNormalizeLink
    nmap <Plug>NoVimwikiRenameFile <Plug>VimwikiRenameFile
    nmap <Plug>NoVimwikiDeleteFile <Plug>VimwikiDeleteFile
    nmap <Plug>NoVimwikiGoto <Plug>VimwikiGoto
    nmap <Plug>NoVimwikiMakeTomorrowDiaryNote <Plug>VimwikiMakeTomorrowDiaryNote

endif

"---------------------------------------------------------------------
" calendar-vim

if exists('g:plugs["calendar-vim"]')
    let g:calendar_diary_filename_pat = '%04d-%02d-%02d'
    let g:calendar_filetype = 'markdown'
    let g:calendar_diary= g:wiki_directory . '/Z91-diary'
endif

"---------------------------------------------------------------------
" vim-lightline
if exists('g:plugs["lightline.vim"]')
    set noshowmode
    let g:lightline = {
          \ 'colorscheme': 'solarized',
          \ 'active': {
          \   'left': [ [ 'mode', 'paste' ],
          \             [ 'gitbranch', 'readonly', 'relativepath', 'modified' ] ]
          \ },
          \ 'component_function': {
          \   'gitbranch': 'FugitiveHead',
          \   'mode': 'ModeWithKeymap',
          \ },
          \ }
endif

function! ModeWithKeymap()
  let l:keymap = &iminsert == 1 && exists("b:keymap_name") ? " <" . b:keymap_name . ">" : ""
  return lightline#mode() . l:keymap
endfunction


"---------------------------------------------------------------------
" vim-tmux-navigator
if exists('g:plugs["vim-tmux-navigator"]')
    let g:tmux_navigator_no_mappings = 1

    nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
    nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
    nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
    nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
    " originally binded to C-l, now change to C-d
    nnoremap <silent> <C-d> :<C-u>nohlsearch<CR><C-l>
    nnoremap <silent> <C-\> :TmuxNavigatePrevious<cr>
endif

"----------------------------------------------------------------------
" easymotion
if exists('g:plugs["vim-easymotion"]')
    nmap <Leader>l <Plug>(easymotion-bd-jk)
    vmap <Leader>l <Plug>(easymotion-bd-jk)
    nmap <Leader>L <Plug>(easymotion-overwin-line)
    vmap <Leader>L <Plug>(easymotion-overwin-line)
endif

"----------------------------------------------------------------------
" leap.nvim
if exists('g:plugs["leap.nvim"]')
    lua << EOF
    require('leap').add_default_mappings()
    vim.keymap.del({'x', 'o'}, 'x')
    vim.keymap.del({'x', 'o'}, 'X')
EOF
endif


"----------------------------------------------------------------------
" ale/syntastic
if exists('g:plugs["ale"]')
    nmap <silent> [e <Plug>(ale_previous_wrap)
    nmap <silent> ]e <Plug>(ale_next_wrap)

    let g:syntastic_mode_map = {
                \ "mode": "active",
                \ "passive_filetypes": ["java", "racket", "go"] }
endif


"----------------------------------------------------------------------
" neoformat

if exists('g:plugs["neoformat"]')
  nmap <silent> <leader>f :Neoformat<CR>
endif


"----------------------------------------------------------------------
" copilot.vim

if exists('g:plugs["copilot.vim"]')
    let g:copilot_no_tab_map = v:true
    imap <expr> <Plug>(vimrc:copilot-dummy-map) copilot#Accept("\<Tab>")
endif

"----------------------------------------------------------------------
" nvim-lspconfig

if exists('g:plugs["nvim-lspconfig"]')
lua << EOF
    local lspconfig = require('lspconfig')
    lspconfig.pyright.setup{}
EOF
endif

"----------------------------------------------------------------------
" coc.nvim

if exists('g:plugs["coc.nvim"]')

    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)

    " Use K to show documentation in preview window.
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    function! s:show_documentation()
        if (index(['vim','help'], &filetype) >= 0)
            execute 'h '.expand('<cword>')
        else
            call CocAction('doHover')
        endif
    endfunction
endif

"----------------------------------------------------------------------
" nvim-cmp

if exists('g:plugs["nvim-cmp"]')
lua <<EOF
    local cmp = require'cmp'
    cmp.setup({
        snippet = {
            -- REQUIRED - you must specify a snippet engine
            expand = function(args)
                vim.fn["UltiSnips#Anon"](args.body)
            end,
        },
        mapping = cmp.mapping.preset.insert({
            ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
            ['<Tab>'] = cmp.mapping(function(fallback)
                vim.api.nvim_feedkeys(vim.fn['copilot#Accept'](vim.api.nvim_replace_termcodes('<Tab>', true, true, true)), 'n', true)
            end),
        }),

        experimental = {
            ghost_text = false -- this feature conflict with copilot.vim's preview.
        },

        sources = cmp.config.sources({
            { name = 'nvim_lsp' },
            { name = 'ultisnips' },
        }, {
            { name = 'buffer' },
        }),
    })
EOF
endif

"----------------------------------------------------------------------
" vim-fugitive
if exists('g:plugs["fugitive-gitlab.vim"]')
    let g:fugitive_gitlab_domains = ['https://gitlab.4pd.io']
endif

"----------------------------------------------------------------------
" vim-gitgutter

if exists('g:plugs["vim-gitgutter"]')
    let g:gitgutter_map_keys = 0
    nmap <silent> ]g :GitGutterNextHunk<CR>
    nmap <silent> [g :GitGutterPrevHunk<CR>
endif

"----------------------------------------------------------------------
" ywvim: input method for chinese
if exists('g:plugs["ywvim"]')
    let g:ywvim_ims=[
                \['wb', '五笔', 'wubi86_jidian.ywvim'],
                \['py', '拼音', 'pinyin.ywvim'],
                \]

    let g:ywvim_py = { 'helpim':'wb', 'gb':0 }

    let g:ywvim_zhpunc = 1
    let g:ywvim_listmax = 5
    let g:ywvim_esc_autoff = 0
    let g:ywvim_autoinput = 0
    let g:ywvim_circlecandidates = 1
    let g:ywvim_helpim_on = 1          " 五笔反查
    let g:ywvim_matchexact = 0
    let g:ywvim_chinesecode = 1
    let g:ywvim_gb = 0
    let g:ywvim_preconv = 'g2b'
    let g:ywvim_conv = ''
    let g:ywvim_lockb = 1
    let g:ywvim_theme = 'dark'
    let g:ywvim_intelligent_punc = 1
endif

"----------------------------------------------------------------------
" treesitter
if exists('g:plugs["nvim-treesitter"]')
lua <<EOF
    require'nvim-treesitter.configs'.setup {
        ensure_installed = { "vim", "c", "cpp", "java", "python", "bash", "css", "go", "lua", "javascript", "yaml", "tsx", "json", "rust"}, -- one of "all", "maintained" (parsers with maintainers), or a list of languages
        ignore_install = {}, -- List of parsers to ignore installing
        highlight = {
            enable = true,
            additional_vim_regex_highlighting = { "markdown" },
        },
        textobjects = { enable = true },
        disable = function(lang, buf)
            local max_filesize = 100 * 1024 -- 100 KB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
                return true
            end
        end,
    }
EOF

endif

"----------------------------------------------------------------------
" treesitter-textobjects
if exists('g:plugs["nvim-treesitter-textobjects"]')

lua <<EOF
require'nvim-treesitter.configs'.setup {
  textobjects = {
    select = {
      enable = true,

      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,

      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
        ["ia"] = "@parameter.inner",
        ["aa"] = "@parameter.outer",
        ["is"] = "@statement.outer",
      },
    },
  },
}

require'nvim-treesitter.configs'.setup {
  textobjects = {
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]f"] = "@function.outer",
        ["]c"] = "@class.outer",
        ["]a"] = "@parameter.inner",
      },
      goto_next_end = {
        ["]F"] = "@function.outer",
        ["]C"] = "@class.outer",
        ["]A"] = "@parameter.inner",
      },
      goto_previous_start = {
        ["[f"] = "@function.outer",
        ["[c"] = "@class.outer",
        ["[a"] = "@parameter.inner",
      },
      goto_previous_end = {
        ["[F"] = "@function.outer",
        ["[C"] = "@class.outer",
        ["[A"] = "@parameter.inner",
      },
    },
  },
}
EOF

endif

"---------------------------------------------------------------------
" vim-translator
"
if exists('g:plugs["vim-translator"]')
    nmap <silent> <Leader>t <Plug>TranslateW
    vmap <silent> <Leader>t <Plug>TranslateWV
endif

"---------------------------------------------------------------------
" vim obsidian
"
if exists('g:plugs["obsidian.nvim"]')
lua << EOF
    require("obsidian").setup({
        workspaces = {
            {
                name = "Personal",
                path = "~/Nextcloud/Vault",
            }
        },

      -- Optional, set to true if you don't want obsidian.nvim to manage frontmatter.
      disable_frontmatter = true,

      templates = {
        subdir = "Z92-templates",
        date_format = "%Y-%m-%d",
        time_format = "%H:%M",
        -- A map for custom variables, the key should be the variable and the value a function
        substitutions = {}
      },
    })

    vim.keymap.set("n", "<C-]>", function()
      if require("obsidian").util.cursor_on_markdown_link() then
        return "<cmd>ObsidianFollowLink<CR>"
      else
        return "<C-]>"
      end
    end, { noremap = false, expr = true })
EOF
endif


"===============================================================================
" self-added plugins && settings

"---------------------------------------------------------------------
" i_emacs
let g:i_emacs_loaded = 1

"===============================================================================
" settings for filetypes

"----------------------------------------------------------------------
" C

autocmd FileType c,cpp setlocal colorcolumn=80

"----------------------------------------------------------------------
" python
let g:enable_my_python_config = 1

"----------------------------------------------------------------------
" markdown

if exists('g:plugs["vim-markdown"]')
    let g:vim_markdown_folding_disabled = 1
    let g:vim_markdown_frontmatter = 1
    let g:vim_markdown_follow_anchor = 1
    let g:vim_markdown_conceal = 1
    let g:tex_conceal = ""
    let g:vim_markdown_math = 1
    let g:vim_markdown_frontmatter = 1
    let g:vim_markdown_no_extensions_in_markdown = 1
    let g:vim_markdown_highlight_with_sed = 1
endif

"----------------------------------------------------------------------
" bullets
if exists('g:plugs["bullets.vim"]')
    let g:bullets_set_mappings = 0

    au FileType markdown,text,gitcommit nmap <silent> <C-Space> :ToggleCheckbox<CR>
    au FileType markdown,text,gitcommit nmap <silent> <C-@> :ToggleCheckbox<CR>
endif


"----------------------------------------------------------------------
" javascript

if exists('vim-jsx')
    let g:jsx_ext_required = 0 " Allow JSX in normal JS files
endif

if exists('javascript-libraries-syntax.vim')
    let g:used_javascript_libs = 'react'
endif

" Syntastic
let g:syntastic_javascript_checkers = ['eslint']

au BufNewFile,BufRead *.js,*.jsx,*.ts,*.tsx,*.html,*.css
    \ set tabstop=2 |
    \ set softtabstop=2 |
    \ set shiftwidth=2 |
    \ set smarttab

"===============================================================================
" client specified settings

if has('nvim')
    "--------------------------------------------------
    " neovim specified settings

    "- - - - - - - - - - - - - - - - - - - - - - - - -
    " mappings

    " Fast editing or sourcing vimrc file
    nmap <silent> <leader>ee :tabnew<cr>:edit ~/.config/nvim/init.vim<cr>

    tnoremap <C-h> <C-\><C-n>:TmuxNavigateLeft<cr>
    tnoremap <C-j> <C-\><C-n>:TmuxNavigateDown<cr>
    tnoremap <C-k> <C-\><C-n>:TmuxNavigateUp<cr>
    tnoremap <C-l> <C-\><C-n>:TmuxNavigateRight<cr>
    tnoremap <C-\> <C-\><C-n>:TmuxNavigatePrevious<cr>

    "- - - - - - - - - - - - - - - - - - - - - - - - -
    " Custom yank ring, utilize numbered registers for yank too.
    function! YankRing(event)
        if (len(a:event.regcontents) == 1 && len(a:event.regcontents[0]) <= 1)
                    \ || a:event.operator == 'd'
            return
        end
        if a:event.regname == ''
            " shfit the numbered registers
            for reg in range(9, 2, -1)
                call setreg(string(reg), getreg(string(reg-1)))
            endfor
            call setreg(1, a:event.regcontents)
        endif
    endfunction

    " Register the TextYankPost event
    au! TextYankPost * call YankRing(copy(v:event))
else
    "--------------------------------------------------
    " vim specified settings

    " use blowfish as default crypt method
    set cryptmethod=blowfish

    " Fast editing or sourcing vimrc file
    nmap <silent> <leader>ee :tabnew<cr>:edit ~/.vimrc<cr>
endif

if has("gui_macvim")
    set macligatures " Enable ligatures for font 'Fira Code'

    " Need this if python is installed via homebrew
    " check this https://github.com/macvim-dev/macvim/issues/562
    if has('python3')
        command! -nargs=1 Py py3 <args>
        set pythonthreedll=/usr/local/Frameworks/Python.framework/Versions/Current/Python
        set pythonthreehome=/usr/local/Frameworks/Python.framework/Versions/Current
    else
        command! -nargs=1 Py py <args>
        set pythondll=/usr/local/Frameworks/Python.framework/Versions/2.7/Python
        set pythonhome=/usr/local/Frameworks/Python.framework/Versions/2.7
    endif
endif
