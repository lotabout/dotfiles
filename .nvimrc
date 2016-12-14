"===============================================================================
" Plugins Manually Installed (needed anyway)
" 1. zenburn.vim       -- color theme
" 2. bufexplorer.vim    -- show buffer list
" 3. vimim_wubi.vim     -- wubi input method
" 4. TagBar             -- show tags of current file
" 5. NERD_tree.vim      -- plugins to show directories
" 6. a.vim              -- quick switch between .c file and .h file
" 7. linuxsty           -- settings for kernel coding style
" 8. EasyMotion	        -- quick navigating plugin
" 9. others => managed using vundle.

"===============================================================================
" General Settings

" Inform that the script is written in UTF-8 (including special characters)
set encoding=utf-8
scriptencoding utf-8

" redirect runtime path to .vim, so that vim and neovim can share directories.
if has('nvim')
    set rtp+=~/.vim/
endif

" use FZF(https://github.com/junegunn/fzf) if it exists
"if executable('fzf')
    "set rtp+=~/.fzf
"endif

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

" set relative number
set relativenumber

" don't treat numbers as octal when performing Ctrl-A and Ctrl-X
set nrformats-=octal

"----------------------------------------------------------------------
" VIM user interface

" Turn on the WiLd menu
set wildmenu
set wildmode=longest,list,full

" Ignore compiled files
set wildignore+=*.o,*~,*.pyc
"set wildignore+=*/.git/*,*/.hg/*,*/.svn/*

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

" set showtabline=2
set completeopt=longest,menuone,preview

"----------------------------------------------------------------------
" Colors and Fonts

" Enable syntax highlighting
syntax enable

" Colorscheme
if has("gui_running")
    colorscheme zenburn
    "colorscheme obsidian
    set guifont=Dejavu\ Sans\ Mono\ for\ Powerline\ 11,Dejavu\ Sans\ Mono\ 11
elseif &t_Co == 256
    "colorscheme zenburn
    colorscheme obsidian
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
    set t_Co=256
    set guitablabel=%M\ %t
endif

"----------------------------------------------------------------------
" Files, backups and undo

" Set possible file encodings
"set fileencodings=ucs-bom,utf-8,cp936,gb18030,utf-16,big5,euc-jp,euc-kr,latin1
set fileencodings=ucs-bom,utf-8,cp932,cp936,gb18030,big5,euc-jp,euc-kr,latin1

" Use Unix as the standard file type
set ffs=unix,dos,mac

" Turn backup off.
" set nobackup

" Change backup directory to a less annoying place under linux.
if has("unix")
    if isdirectory($HOME.'/.vim-backup') == 0
        :silent !mkdir -p ~/.vim-backup > /dev/null 2>&1
    endif
    set backupdir-=.
    set backupdir+=.
    set backupdir-=~/
    set backupdir^=~/.vim-backup
    set backupdir^=./.vim-backup

    if isdirectory($HOME.'/.vim-swap') == 0
        :silent !mkdir -p ~/.vim-swap > /dev/null 2>&1
    endif
    set directory=./.vim-swap//
    set directory+=~/.vim-swap//
    set directory+=~/tmp//
    set directory+=.

    if exists('+undofile')
        if isdirectory($HOME.'/.vim-undo') == 0
            :silent !mkdir -p ~/.vim-undo > /dev/null 2>&1
        endif

        set undodir=./.vim-undo//
        set undodir+=~/.vim-undo//
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
set cinoptions=l1,t0,g0,(0,Ws

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

function! MyBufferDelete ()
    let s:totalnumber = len(filter(range(1,bufnr('$')),'buflisted(v:val)'))
    if s:totalnumber > 1
        bdelete
    else
        quit
    end
endfunction

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

"----------------------------------------------------------------------
" Misc Maps & Functions(for convenience)

" Remove the Windows ^M - when the encodings gets messed up
nnoremap <Leader>mm mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" strip the spaces at the end of line
nnoremap <leader><Space><Space> :%s/\s\+$//<cr>:<C-u>nohlsearch<CR>

" merge multiple continuous lines into one.
nmap <leader><cr> :%s/\(^[[:blank:]]*\n\)\{2,}/\r/<cr>

" quick access to system's clipboard
vmap <leader>y "+y
vmap <leader>Y "+Y
vmap <leader>d "+d
vmap <leader>p "+p
vmap <leader>P "+P
nmap <leader>p "+p
nmap <leader>P "+P

" quick save
nmap <leader>w :w<CR>

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


" execute macro on every selected/visual lines
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

"===============================================================================
" Settings for Programming

"----------------------------------------------------------------------
" Extra Settings

set formatoptions=tclqron
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

" grep, ignore some directories.
set grepprg=grep\ -nrI\ --exclude-dir=target\ --exclude-dir=tmp\ --exclude=\"*.min.js\"\ --exclude=\"tags\"\ $*\ /dev/null

" The Silver Searcher
if executable('ag')
    " Use ag over grep
    set grepprg=ag\ --nogroup\ --nocolor\ --ignore-dir\ target\ --ignore-dir\ tmp\ --ignore\ \"*.min.js\"\ --ignore\ \"tags\"
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

" tag related
nmap <silent> [t :tprev<CR>
nmap <silent> ]t :tnext<CR>
nmap <silent> [T :tfirst<CR>
nmap <silent> ]T :tlast<CR>

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
vnoremap / <Esc>/\%><C-R>=line("'<")-1<CR>l\%<<C-R>=line("'>")+1<CR>l
vnoremap ? <Esc>?\%><C-R>=line("'<")-1<CR>l\%<<C-R>=line("'>")+1<CR>l

" Scroll without moving cursor screen line
nnoremap <C-J> <C-E>j
vnoremap <C-J> <C-E>j
nnoremap <C-K> <C-Y>k
vnoremap <C-K> <C-Y>k

"----------------------------------------------------------------------
" Show tabs (indent lines)

" | ¦ ┆ ┊ │
let show_tabs=0
if show_tabs == 1
    if &encoding ==? "utf-8"
        set list
        set listchars=tab:\│\ ,trail:~
    else
        set list
        set listchars=tab:>-,trail:~
    endif
endif

" Configuration for plugin: indentLine
let g:indentLine_loaded = 1
let g:indentLine_char = "│"

"===============================================================================
" settings for manually installed plugins

"----------------------------------------------------------------------
" BufExplorer

" disable default keybinding
noremap <Plug>NoBufExplorerHorizontalSplit :BufExplorerHorizontalSplit<CR>
noremap <Plug>NoBufExplorerVerticalSplit :BufExplorerVerticalSplit<CR>

noremap <silent> <C-e> :BufExplorer<CR>

"----------------------------------------------------------------------
" vimim

"let fileencodings=ucs-bom,utf8,chinese,taiwan,ansi
if has ("win32")
    set guifont=Courier_New:h12:w7
    set guifontwide=NSimSun-18030,NSimSun
endif
"set ambiwidth=double
" let g:vimim_map='c-bslash'



"===============================================================================
" package manager settings
let package_manager = "vim-plug"

if package_manager == "vim-plug"
    call plug#begin('~/.vim/plugged')

    "------------------------------------------------------------------
    " Enhance Basic functionality
    "------------------------------------------------------------------
    Plug 'altercation/vim-colors-solarized'

    Plug 'moll/vim-bbye'
    Plug 'surround.vim'
    Plug 'repeat.vim'
    Plug 'junegunn/vim-easy-align'
    Plug 'ervandew/supertab'    " you'll need it
    "Plug 'osyo-manga/vim-over'  " for substitution preview
    Plug 'easymotion/vim-easymotion'
    "Plug 'haya14busa/incsearch.vim' " highlight all instances when search

    Plug 'Tagbar' " actually not used frequently
    Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

    " for word wraps for japanese and chinese
    Plug 'autofmt'

    " powerline alternative; for better status line
    Plug 'bling/vim-airline'
    Plug 'vim-airline/vim-airline-themes'

    Plug 'terryma/vim-expand-region'

    Plug 'Chiel92/vim-autoformat' " enhance the format function (press '=' key)

    "Plug 'MattesGroeger/vim-bookmarks'

    Plug 't9md/vim-choosewin'

    Plug 'sjl/gundo.vim'

    Plug 'Raimondi/delimitMate' " insert closing quotes, parenthesis, etc. automatically

    Plug 'takac/vim-hardtime'
    "------------------------------------------------------------------
    " Integration with Linux environment
    "------------------------------------------------------------------
    Plug 'lotabout/slimux'
    Plug 'fakeclip'

    Plug 'lotabout/skim', { 'dir': '~/.skim', 'do': './install' }
    Plug 'lotabout/skim.vim'

    " Async support for vim
    " used by vim-monster
    Plug 'Shougo/vimproc.vim', {'do' : 'make'}

    " work with git
    Plug 'fugitive.vim'

    Plug 'DirDiff.vim'

    " Completion
    if has('nvim')
        Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
        Plug 'zchee/deoplete-jedi', {'for': 'python'}

        Plug 'sebastianmarkow/deoplete-rust', {'for': 'rust'}
    else
        Plug 'https://github.com/davidhalter/jedi-vim.git', {'for': 'python'}

        Plug 'racer-rust/vim-racer', {'for': 'rust'}
    endif

    "------------------------------------------------------------------
    " Support more filetype specific feature
    "------------------------------------------------------------------
    Plug 'The-NERD-Commenter'
    Plug 'UltiSnips'
    Plug 'honza/vim-snippets'

    " private snippets
    Plug 'lotabout/vim-ultisnippet-private'

    "Plug 'Syntastic' " syntax checker
    "Plug 'scrooloose/syntastic'
    Plug 'w0rp/ale' " async version of Syntastic

    " Plug 'LaTeX-Suite-aka-Vim-LaTeX'
    " Plug 'jcf/vim-latex.git'
    " Plug 'rosenfeld/conque-term'
    "Plug 'slimv.vim'
    " Plug 'paredit.vim'

    " in replace of paredit.vim
    Plug 'vim-sexp', {'for': ['clojure', 'scheme']}
    Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': ['clojure', 'scheme']}

    Plug 'https://github.com/wlangstroth/vim-racket', {'for': 'racket'}

    Plug 'osyo-manga/vim-monster', {'do': 'gem install rcodetools --user-install', 'for': 'ruby'}

    Plug 'Rip-Rip/clang_complete', {'for': ['c', 'cpp']}

    Plug 'mattn/emmet-vim', {'for': ['html', 'xml', 'css', 'nhtml', 'javascript', 'javascript-jsx']}

    " for javascript
    "Plug 'ternjs/tern_for_vim', {'for': 'javascript', 'do' : 'npm install'}
    Plug 'ternjs/tern_for_vim', {'do' : 'npm install'}
    Plug 'pangloss/vim-javascript', {'for': 'javascript'}
    Plug 'mxw/vim-jsx' " for react.js
    Plug 'othree/javascript-libraries-syntax.vim'

    Plug 'tpope/vim-fireplace', {'for': 'clojure'}
    Plug 'rust-lang/rust.vim', {'for': 'rust'}

    Plug 'vim-ruby/vim-ruby'

    " for markdown
    Plug 'plasticboy/vim-markdown'

    "------------------------------------------------------------------
    " Others
    "------------------------------------------------------------------
    Plug 'christoomey/vim-tmux-navigator'
    Plug 'mattn/calendar-vim'
    Plug 'lotabout/vimwiki', {'branch': 'dev'}

    call plug#end()
elseif package_manager == "pathogen"
    execute pathogen#infect()
endif

"===============================================================================
" settings for bundle plugins

"----------------------------------------------------------------------
" color scheme
if has('gui_running')
    colorscheme solarized
elseif &t_Co == 256
    set background=dark
    colorscheme solarized
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
xmap ga <Plug>(EasyAlign)

"----------------------------------------------------------------------
" easymotion

if exists('g:plugs["vim-easymotion"]')
    let g:EasyMotion_do_mapping = 0 " Disable default mappings
    let g:EasyMotion_enter_jump_first = 1

    " Turn on case insensitive feature
    "let g:EasyMotion_smartcase = 1

    nmap f <Plug>(easymotion-s)
    vmap f <Plug>(easymotion-s)
    nmap F <Plug>(easymotion-F)
    vmap F <Plug>(easymotion-F)
    nmap ; <Plug>(easymotion-next)
    vmap ; <Plug>(easymotion-next)
    nmap , <Plug>(easymotion-prev)
    vmap , <Plug>(easymotion-prev)
    nmap <Leader>l <Plug>(easymotion-bd-jk)
    vmap <Leader>l <Plug>(easymotion-bd-jk)
    nmap <Space>. <Plug>(easymotion-repeat)
endif

"----------------------------------------------------------------------
" incsearch.vim
if exists('g:plugs["incsearch.vim"]')
    map /  <Plug>(incsearch-forward)
    map ?  <Plug>(incsearch-backward)
    map g/ <Plug>(incsearch-stay)
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
endif

"----------------------------------------------------------------------
" Tagbar

if exists('g:plugs["Tagbar"]')

    " nmap <silent> <leader>tl :TlistToggle<cr>
    nmap <silent> <leader>tl :TagbarToggle<cr>
    nmap <silent> <leader>ne :NERDTreeToggle<cr>
    nmap <silent> <leader>nf :NERDTreeFind<cr>
    nmap <silent> <F8> :call  ToggleNERDTreeAndTagbar()<cr>

    function! ToggleNERDTreeAndTagbar()
        let w:jumpbacktohere = 1

        " Detect which plugins are open
        if exists('t:NERDTreeBufName')
            let nerdtree_open = bufwinnr(t:NERDTreeBufName) != -1
        else
            let nerdtree_open = 0
        endif
        let tagbar_open = bufwinnr('__Tagbar__') != -1

        " Perform the appropriate action
        if nerdtree_open && tagbar_open
            NERDTreeClose
            TagbarClose
        elseif nerdtree_open
            TagbarOpen
        elseif tagbar_open
            NERDTree
        else
            NERDTree
            TagbarOpen
        endif

        " Jump back to the original window
        for window in range(1, winnr('$'))
            execute window . 'wincmd w'
            if exists('w:jumpbacktohere')
                unlet w:jumpbacktohere
                break
            endif
        endfor
    endfunction
endif

"---------------------------------------------------------------------
" fakeclip
let g:fakeclip_terminal_multiplexer_type = "tmux"

"---------------------------------------------------------------------
" UltiSnips

let g:UltiSnipsSnippetsDir = "~/.vim/plugged/vim-ultisnippet-private/UltiSnips"

"---------------------------------------------------------------------
" ctrlp
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files']

" use ctrlp-cmatcher for mathing items.
" if you do not use it, disable the following line.
let g:ctrlp_match_func = {'match' : 'matcher#cmatch' }

"---------------------------------------------------------------------
" fzf.vim

" change keybinding for AG
if executable('fzf')
    let $FZF_DEFAULT_OPTS = '--bind ctrl-f:toggle'
    " replace Ctrl-p
    nmap <C-p> :Files<CR>

    " Customized binding for AG
    nnoremap <leader>/ :Ag<space>

    " Replace Bufexplore
    nmap <C-e> :Buffers<CR>

    " select mapping
    nmap <leader><tab> <plug>(fzf-maps-n)
    xmap <leader><tab> <plug>(fzf-maps-x)
    omap <leader><tab> <plug>(fzf-maps-o)
endif

"---------------------------------------------------------------------
" skim.vim
if executable('sk')
    let $SKIM_DEFAULT_OPTS = '--bind ctrl-f:toggle'
    " replace Ctrl-p
    nmap <C-p> :Files<CR>

    " Customized binding for AG
    nnoremap <leader>/ :Ag<space>

    " Replace Bufexplore
    nmap <C-e> :Buffers<CR>

    " select mapping
    nmap <leader><tab> <plug>(fzf-maps-n)
    xmap <leader><tab> <plug>(fzf-maps-x)
    omap <leader><tab> <plug>(fzf-maps-o)
endif

"---------------------------------------------------------------------
" vim-bookmarks
if exists('g:plugs["vim-bookmarks"]')
    "nmap <leader>i <Plug>BookmarkAnnotate
    nmap mj <Plug>BookmarkNext
    nmap mk <Plug>BookmarkPrev
    nmap ml <Plug>BookmarkShowAll
    nmap <F2> <Plug>BookmarkNext

    " unmap for mkk/mjj
    nmap <Plug>NoBookmarkMoveUp <Plug>BookmarkMoveUp
    nmap <Plug>NoBookmarkMoveDown <Plug>BookmarkMoveDown

    let g:bookmark_auto_save = 1
endif

"---------------------------------------------------------------------
" autofmt
if exists('g:plugs["autofmt"]')
    nmap <leader>f :setlocal formatexpr=autofmt#japanese#formatexpr()<CR>
endif

"---------------------------------------------------------------------
" Gundo
if exists('g:plugs["gundo.vim"]')
    nnoremap <F5> :GundoToggle<CR>
endif

if exists('g:plugs["delimitMate"]')
    " not used
    au FileType racket let b:delimitMate_quotes = "\""
endif


"---------------------------------------------------------------------
" vimwiki

" set norelativenumber manually.
let g:calendar_options = "fdc=0 nonu nornu"

if exists('g:plugs["vimwiki"]')
    " turn off insert mode mappings
    let g:vimwiki_global_ext = 0
    let g:vimwiki_table_mappings = 0
    let g:vimwiki_ext2syntax = {'.md': 'markdown',
                    \ '.mkd': 'markdown',
                    \ '.wiki': 'media'}
    let g:vimwiki_use_calendar = 1

    let wiki_1 = {}
    let wiki_1.path = '~/Dropbox/wiki/vimwiki'
    "let wiki_1.path_html = '~/repos/vimwiki_html'
    "let wiki_1.template_path= wiki_1.path_html . '/template'
    "let wiki_1.template_default = 'default'
    "let wiki_1.template_ext = '.htm'
    let wiki_1.nested_syntaxes = {'python': 'python',
        \ 'js': 'javascript',
        \ 'c': 'c',
        \ 'java': 'java',
        \ 'sql': 'sql',
        \ 'rust': 'rust',
        \ 'scheme': 'scheme',
        \ 'racket': 'racket'}
    let wiki_1.syntax = 'markdown'
    let wiki_1.ext = '.md'
    let wiki_1.diary_rel_path = '../diary/'

    let wiki_2 = {}
    let wiki_2.path = '~/Dropbox/wiki/vimwiki-private'
    let wiki_2.nested_syntaxes = wiki_1.nested_syntaxes
    let wiki_2.syntax = 'markdown'
    let wiki_2.ext = '.md'
    let g:vimwiki_list = [wiki_1, wiki_2]

    map <F4> :exec '!cd '.VimwikiGet('path').'; ./sync.sh'<cr>

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

    nmap <leader>` <Plug>VimwikiIndex
endif

"---------------------------------------------------------------------
" vim-airline

if exists('g:plugs["vim-airline"]')
    if has("gui_running")
        let g:airline_theme='solarized'
    else
        "let g:airline_theme='wombat'
        let g:airline_theme='solarized'
    endif

    " let g:airline#extensions#tabline#enabled = 1
    let g:airline_left_sep = ''
    let g:airline_left_alt_sep = ''
    let g:airline_right_sep = ''
    let g:airline_right_alt_sep = ''
    let g:airline_symbols = {}
    let g:airline_symbols.branch = ''
    let g:airline_symbols.readonly = ''
    let g:airline_symbols.linenr = ''

    "" Enable the list of buffers
    "let g:airline#extensions#tabline#enabled = 1

    "" Show just the filename
    "let g:airline#extensions#tabline#fnamemod = ':t'
endif

"---------------------------------------------------------------------
" vim-argumentative
if exists('g:plugs["vim-argumentative"]')
    nmap >. <Plug>Argumentative_MoveRight
endif

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

"---------------------------------------------------------------------
" vim-choosewin
if exists('g:plugs["vim-choosewin"]')
    nmap - <Plug>(choosewin)
    let g:choosewin_blink_on_land  = 0 " don't blink at land
    "let g:choosewin_overlay_enable = 1
endif

if exists('g:plugs["vim-hardtime"]')
    nnoremap <silent> <leader>h :HardTimeToggle<CR>
endif
"----------------------------------------------------------------------

"===============================================================================
" self-added plugins && settigns

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
" rust

" racer : rust auto completion
let $RUST_SRC_PATH = "/usr/local/src/rust/src/"
let g:deoplete#sources#rust#racer_binary = expand("~/.cargo/bin/racer")

"----------------------------------------------------------------------
" markdown
let g:vim_markdown_folding_disabled = 1

"----------------------------------------------------------------------
" javascript

" javascript-libraries-syntax.vim
let g:used_javascript_libs = 'underscore,jquery,angularjs,angularui,react'
let g:jsx_ext_required = 0 " Allow JSX in normal JS files

" Syntastic
let g:syntastic_javascript_checkers = ['eslint']

let g:syntastic_mode_map = {
            \ "mode": "active",
            \ "passive_filetypes": ["java", "racket"] }

"===============================================================================
" client specified settings

if has('nvim')
    "--------------------------------------------------
    " neovim specified settings

    "- - - - - - - - - - - - - - - - - - - - - - - - -
    " mappings

    " Fast editing or sourcing vimrc file
    nmap <silent> <leader>ee :tabnew<cr>:edit ~/.nvimrc<cr>

    tnoremap <C-h> <C-\><C-n>:TmuxNavigateLeft<cr>
    tnoremap <C-j> <C-\><C-n>:TmuxNavigateDown<cr>
    tnoremap <C-k> <C-\><C-n>:TmuxNavigateUp<cr>
    tnoremap <C-l> <C-\><C-n>:TmuxNavigateRight<cr>
    tnoremap <C-\> <C-\><C-n>:TmuxNavigatePrevious<cr>

    if exists('g:plugs["deoplete.nvim"]')
        let g:deoplete#enable_at_startup = 1

        " With deoplete.nvim
        let g:monster#completion#rcodetools#backend = "async_rct_complete"
        let g:deoplete#sources#omni#input_patterns = {
                    \   "ruby" : '[^. *\t]\.\w*\|\h\w*::',
                    \}
    endif

else
    "--------------------------------------------------
    " vim specified settings

    "- - - - - - - - - - - - - - - - - - - - - - - - -
    " default settings

    " use blowfish as default crypt method
    set cryptmethod=blowfish

    " enable mouse support
    set mouse=a

    "- - - - - - - - - - - - - - - - - - - - - - - - -
    " mappings

    " Fast editing or sourcing vimrc file
    nmap <silent> <leader>ee :tabnew<cr>:edit ~/.vimrc<cr>

    "if !has("gui_running")
        "" fix Alt key problem
        "let c='a'
        "while c <= 'z'
            "exec "set <A-".c.">=\e".c
            "exec "imap \e".c." <A-".c.">"
            "let c = nr2char(1+char2nr(c))
        "endw
        ""set timeout ttimeoutlen=50
        "set ttimeoutlen=20
    "endif
endif

