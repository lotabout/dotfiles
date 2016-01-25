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
set history=700

" Enable filetype plugins
filetype plugin indent on

" Set to auto read when a file is changed from the outside
set autoread

" Set map leader
"let mapleader = ","
"let g:mapleader = ","
let mapleader = "\<Space>"
let g:mapleader = "\<Space>"

" Show line number
set nu

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
    "colorscheme zenburn
    "colorscheme obsidian
    colorscheme molokai
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
noremap 0 g0
noremap g0 0
noremap $ g$
noremap g$ $

" swap these too because ' is easier to type and ` is what I want
noremap ' `
noremap ` '


" Close current buffer
" map <leader>q :bdelete<cr>
" nmap <leader>k :call MyBufferDelete()<cr>
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

" Specify the behavior when switching between buffers
" try
"   set switchbuf=usetab,newtab
"   set stal=2
" catch
" endtry

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

"----------------------------------------------------------------------
" Misc Maps & Functions(for convenience)

" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>mm mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Toggle paste mode on and off
" nmap <leader>p :setlocal paste!<cr>

" strip the spaces at the end of line
nmap <leader><Space><Space> :%s/\s\+$//<cr>

" merge multiple continuous lines into one.
nmap <leader><cr> :%s/\(^[[:blank:]]*\n\)\{2,}/\r/<cr>

" quick access to system's clipboard
vmap <leader>y "+y
vmap <leader>Y "+Y
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

"===============================================================================
" Settings for Programming

"----------------------------------------------------------------------
" Extra Settings

set formatoptions=tclqron
runtime macros/matchit.vim

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

" tab related
nmap <silent> <leader>tn :tabnew<cr>
nmap <silent> <leader>tc :tabclose<cr>

" disable highlight search for current search
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>


" visual mode: star-search
xnoremap * :<C-u>call <SID>VSetSearch()<CR>/<C-R>=@/<CR><CR>
xnoremap * :<C-u>call <SID>VSetSearch()<CR>?<C-R>=@/<CR><CR>

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

"nnoremap <A-h> <C-w>h
"nnoremap <A-j> <C-w>j
"nnoremap <A-k> <C-w>k
"nnoremap <A-l> <C-w>l

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
    Plug 'moll/vim-bbye'
    Plug 'surround.vim'
    Plug 'repeat.vim'
    Plug 'yueyoum/vim-alignment'
    Plug 'ervandew/supertab'
    Plug 'EasyMotion'
    Plug 'Tagbar'
    Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

    " for word wraps for japanese and chinese
    Plug 'autofmt'

    " powerline alternative; for better status line
    Plug 'bling/vim-airline'

    Plug 'terryma/vim-expand-region'

    " enhance the format function (press '=' key)
    Plug 'Chiel92/vim-autoformat'

    Plug 'MattesGroeger/vim-bookmarks'

    "------------------------------------------------------------------
    " Integration with Linux environment
    "------------------------------------------------------------------
    Plug 'lotabout/slimux'
    Plug 'fakeclip'

    "Plug 'rking/ag.vim'
    "Plug 'lotabout/ag.nvim'

    "Plug 'Shougo/Vimproc.vim', {'do': 'make'}
    "Plug 'Shougo/Unite.vim'

    " search files/MRUs easily (by press 'Ctrl-p' in normal mode)
    " use ctrlp only when FZF do not exist
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    if !executable('fzf')
        Plug 'ctrlp.vim'
        Plug 'JazzCore/ctrlp-cmatcher'
    endif

    " work with git
    Plug 'fugitive.vim'

    Plug 'DirDiff.vim'

    "------------------------------------------------------------------
    " Support more filetype specific feature
    "------------------------------------------------------------------
    Plug 'The-NERD-Commenter'
    Plug 'UltiSnips'
    Plug 'honza/vim-snippets'

    " private snippets
    Plug 'lotabout/vim-ultisnippet-private'

    Plug 'Syntastic' " syntax checker

    " Plug 'LaTeX-Suite-aka-Vim-LaTeX'
    " Plug 'jcf/vim-latex.git'
    " Plug 'rosenfeld/conque-term'
    "Plug 'slimv.vim'
    " Plug 'paredit.vim'

    " in replace of paredit.vim
    Plug 'vim-sexp', {'for': ['clojure', 'scheme']}
    Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': ['clojure', 'scheme']}

    " Plug 'ivanov/vim-ipython.git'
    "Plug 'Yggdroot/indentLine'
    Plug 'https://github.com/davidhalter/jedi-vim.git', {'for': 'python'}
    Plug 'https://github.com/wlangstroth/vim-racket', {'for': 'racket'}

    Plug 'mattn/emmet-vim', {'for': ['html', 'xml', 'css', 'nhtml']}

    "Plug 'Rip-Rip/clang_complete'
    "Plug 'Valloric/YouCompleteMe'

    " for javascript
    "Plug 'ternjs/tern_for_vim', {'for': 'javascript', 'do' : 'npm install'}
    Plug 'ternjs/tern_for_vim', {'do' : 'npm install'}

    Plug 'tpope/vim-fireplace', {'for': 'clojure'}
    Plug 'rust-lang/rust.vim'
    Plug 'racer-rust/vim-racer'

    " for markdown
    Plug 'godlygeek/tabular'
    Plug 'plasticboy/vim-markdown'

    "------------------------------------------------------------------
    " Others
    "------------------------------------------------------------------
    Plug 'christoomey/vim-tmux-navigator'
    Plug 'https://github.com/mattn/calendar-vim'
    "Plug 'vimwiki/vimwiki'
    Plug 'https://github.com/xolox/vim-misc.git'
    Plug 'xolox/vim-notes'

    call plug#end()
elseif package_manager == 'vundle'
    "------------------------------------------------------------------
    " Vundle Settings
    set nocompatible               " be iMproved
    filetype off                   " required!

    if has("win32")
        set rtp+=~/vimfiles/vundle.git
    else
        set rtp+=~/.vim/Vundle.vim
    endif

    call vundle#begin()

    " let Vundle manage Vundle, required
    "Plugin 'gmarik/Vundle.vim'

    " My Bundles here:

    "------------------------------------------------------------------
    " Enhance Basic functionality
    "------------------------------------------------------------------
    "Plugin 'justinmk/vim-sneak'
    "Bundle 'https://github.com/xolox/vim-misc.git'
    "Bundle 'https://github.com/xolox/vim-session.git'
    "Bundle 'https://github.com/kshenoy/vim-signature'
    Bundle 'moll/vim-bbye'
    Bundle 'surround.vim'
    Bundle 'repeat.vim'
    Bundle 'Gundo'
    Bundle 'yueyoum/vim-alignment'
    " Bundle "Align"
    " Bundle 'matchit.zip'
    Bundle "ervandew/supertab"

    " for word wraps for japanese and chinese
    Bundle 'autofmt'

    "Bundle 'mru.vim'

    " bettern substitution support
    "Bundle 'abolish.vim'

    " for kill ring like in emacs
    " Bundle 'yankstack'  " not work well with other part of my configuration

    " powerline alternative; for better status line
    Plugin 'bling/vim-airline'

    Plugin 'terryma/vim-expand-region'

    " enhance the format function (press '=' key)
    Plugin 'Chiel92/vim-autoformat'

    Plugin 'MattesGroeger/vim-bookmarks'

    "------------------------------------------------------------------
    " Integration with Linux environment
    "------------------------------------------------------------------
    "Bundle 'epeli/slimux'
    Bundle 'lotabout/slimux'
    Bundle 'fakeclip'

    Bundle 'rking/ag.vim'

    " search files/MRUs easily (by press 'Ctrl-p' in normal mode)
    " use ctrlp only when FZF do not exist
    if !executable('fzf')
        Bundle 'ctrlp.vim'
        Plugin 'JazzCore/ctrlp-cmatcher'
    endif

    " work with git
    Bundle 'fugitive.vim'

    " Bundle 'https://github.com/oplatek/Conque-Shell'

    Plugin 'DirDiff.vim'
    "Plugin 'szw/vim-tags'

    "------------------------------------------------------------------
    " Support more filetype specific feature
    "------------------------------------------------------------------
    Bundle 'The-NERD-Commenter'
    Plugin 'UltiSnips'
    Bundle "honza/vim-snippets"

    Plugin 'Syntastic' " syntax checker

    " Bundle 'LaTeX-Suite-aka-Vim-LaTeX'
    " Bundle 'jcf/vim-latex.git'
    " Bundle 'rosenfeld/conque-term'
    "Bundle 'slimv.vim'
    " Bundle 'paredit.vim'

    " in replace of paredit.vim
    Bundle 'vim-sexp'
    Bundle 'tpope/vim-sexp-mappings-for-regular-people.git'

    " Bundle 'ivanov/vim-ipython.git'
    "Bundle 'Yggdroot/indentLine'
    Bundle 'https://github.com/davidhalter/jedi-vim.git'

    Bundle 'https://github.com/wlangstroth/vim-racket'
    Bundle "mattn/emmet-vim"

    "Bundle 'Rip-Rip/clang_complete'
    "Bundle 'Valloric/YouCompleteMe'

    " for javascript
    "Plugin 'marijnh/tern_for_vim'


    Plugin 'tpope/vim-fireplace'

    " for rust
    "Plugin 'rust-lang/rust.vim'
    "Plugin 'racer-rust/vim-racer'

    "------------------------------------------------------------------
    " Others
    "------------------------------------------------------------------
    Bundle 'christoomey/vim-tmux-navigator'
    Bundle 'https://github.com/mattn/calendar-vim'
    "Bundle 'vimwiki'
    "Plugin 'vim-orgmode'

    " non github repos
    " Bundle 'git://git.wincent.com/command-t.git'
    " ...

    " All of your Plugins must be added before the following line
    call vundle#end()            " required
    filetype plugin indent on    " required
    " To ignore plugin indent changes, instead use:
    "filetype plugin on
    "
    " Brief help
    " :PluginList       - lists configured plugins
    " :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
    " :PluginSearch foo - searches for foo; append `!` to refresh local cache
    " :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
    "
    " see :h vundle for more details or wiki for FAQ
    " Put your non-Plugin stuff after this line
elseif package_manager == "pathogen"
    execute pathogen#infect()
endif

"===============================================================================
" settings for bundle plugins

"----------------------------------------------------------------------
" savesession
let g:session_directory="~/.tmp/vim/sessions"
let g:session_autosave = 'no'
let g:session_autoload = 'no'

"----------------------------------------------------------------------
" supertab
let g:SuperTabLongestEnhanced = 1
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabCrMapping = 1

"----------------------------------------------------------------------
" slimux
let g:slimux_select_from_current_window = 1
map <Leader>s :SlimuxREPLSendLine<CR>
vmap <Leader>s :SlimuxREPLSendSelection<CR>
"map <Leader>sl :SlimuxShellLast<CR>
"map <leader>bl :SlimuxREPLSendBuffer<CR>

let g:slimux_scheme_keybindings=1
let g:slimux_scheme_leader=';'
let g:slimux_racket_keybindings=1
let g:slimux_racket_leader=';'
let g:slimux_racket_xrepl=1

"----------------------------------------------------------------------
" Tagbar

" nmap <silent> <leader>tl :TlistToggle<cr>
nmap <silent> <leader>tl :TagbarToggle<cr>
nmap <silent> <leader>ne :NERDTreeToggle<cr>
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

"---------------------------------------------------------------------
" fakeclip
let g:fakeclip_terminal_multiplexer_type = "tmux"

"---------------------------------------------------------------------
" UltiSnips

let g:UltiSnipsSnippetsDir = "~/.vim/plugged/vim-ultisnippet-private/UltiSnips"

"---------------------------------------------------------------------
" Unite.vim
let g:unite_prompt='» '
"let g:unite_enable_start_insert=1
let g:unite_data_directory='~/.vim/.cache/unite'

if executable('ag')
    let g:unite_source_grep_command = 'ag'
    let g:unite_source_grep_default_opts =
          \ '--line-numbers --nocolor --nogroup --hidden --ignore ' .
          \  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
    let g:unite_source_grep_recursive_opt = ''
endif

"nnoremap <C-p> :Unite -start-insert file_rec/async:!<cr>
"nnoremap <leader>b :Unite buffer<cr>
"nnoremap <leader>. :UniteResume<cr>

"---------------------------------------------------------------------
" ctrlp
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files']

" use ctrlp-cmatcher for mathing items.
" if you do not use it, disable the following line.
let g:ctrlp_match_func = {'match' : 'matcher#cmatch' }

if executable('fzf')
    nmap <C-p> :FZF --multi --bind ctrl-f:toggle<CR>
endif

"---------------------------------------------------------------------
" FZF utilities

" https://github.com/junegunn/fzf/wiki/Examples-%28vim%29
command! FZFMru call fzf#run({
\ 'source':  reverse(s:all_files()),
\ 'sink':    'edit',
\ 'options': '-m -x +s',
\ 'down':    '40%' })

function! s:all_files()
  return extend(
  \ filter(copy(v:oldfiles),
  \        "v:val !~ 'fugitive:\\|NERD_tree\\|^/tmp/\\|.git/'"),
  \ map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), 'bufname(v:val)'))
endfunction

" FZF Ag integration
function! s:ag_to_qf(line)
  let parts = split(a:line, ':')
  return {'filename': parts[0], 'lnum': parts[1], 'col': parts[2],
        \ 'text': join(parts[3:], ':')}
endfunction

function! s:ag_handler(lines)
  if len(a:lines) < 2 | return | endif

  let cmd = get({'ctrl-x': 'split',
               \ 'ctrl-v': 'vertical split',
               \ 'ctrl-t': 'tabe'}, a:lines[0], 'e')
  let list = map(a:lines[1:], 's:ag_to_qf(v:val)')

  let first = list[0]
  execute cmd escape(first.filename, ' %#\')
  execute first.lnum
  execute 'normal!' first.col.'|zz'

  if len(list) > 1
    call setqflist(list)
    copen
    wincmd p
  endif
endfunction

command! -nargs=* Ag call fzf#run({
\ 'source':  printf('ag --nogroup --column --color "%s"',
\                   escape(empty(<q-args>) ? '^(?=.)' : <q-args>, '"\')),
\ 'sink*':    function('<sid>ag_handler'),
\ 'options': '--ansi --expect=ctrl-t,ctrl-v,ctrl-x --delimiter : --nth 4.. '.
\            '--multi --bind ctrl-a:select-all,ctrl-d:deselect-all,ctrl-f:toggle '.
\            '--color hl:68,hl+:110',
\ 'down':    '50%'
\ })

nnoremap <leader>/ :Ag<space>

"---------------------------------------------------------------------
" vim-bookmarks
"nmap <leader>i <Plug>BookmarkAnnotate
nmap <leader>j <Plug>BookmarkNext
nmap <leader>k <Plug>BookmarkPrev
nmap <leader>l <Plug>BookmarkShowAll
nmap <F2> <Plug>BookmarkNext

let g:bookmark_auto_save = 1

"---------------------------------------------------------------------
" autofmt
nmap <leader>f :setlocal formatexpr=autofmt#japanese#formatexpr()<CR>

"---------------------------------------------------------------------
" Gundo
nnoremap <F5> :GundoToggle<CR>


"---------------------------------------------------------------------
" vimwiki

" turn off insert mode mappings
let g:vimwiki_table_mappings = 0
" let g:vimwiki_ext2syntax = {'.md': 'markdown',
"                 \ '.mkd': 'markdown',
"                 \ '.wiki': 'media'}

let wiki_1 = {}
let wiki_1.path = '~/Dropbox/wiki/vimwiki'
"let wiki_1.path_html = '~/repos/vimwiki_html'
"let wiki_1.template_path= wiki_1.path_html . '/template'
"let wiki_1.template_default = 'default'
"let wiki_1.template_ext = '.htm'
let wiki_1.nested_syntaxes = {'python': 'python',
    \ 'javascript': 'javascript',
    \ 'c': 'c',
    \ 'sql': 'sql',
    \ 'rust': 'rust',
    \ 'scheme': 'scheme',
    \ 'racket': 'racket'}
"let wiki_1.syntax = 'markdown'
"let wiki_1.ext = '.md'

let wiki_2 = {}
let wiki_2.path = '~/Dropbox/wiki/vimwiki-private'
let wiki_2.nested_syntaxes = wiki_1.nested_syntaxes
"let wiki_2.syntax = 'markdown'
"let wiki_2.ext = '.md'
let g:vimwiki_list = [wiki_1, wiki_2]
"map <F4> :VimwikiAll2HTML<cr> :exec '!cd '.VimwikiGet('path_html').'; ./sync.sh'<cr>

" Disable vimwiki mappings (to remove bindings begins with <leader>w)
nmap <Plug>NoVimwikiIndex <Plug>VimwikiIndex
nmap <Plug>NoVimwikiTabIndex <Plug>VimwikiTabIndex
nmap <Plug>NoVimwikiUISelect <Plug>VimwikiUISelect
nmap <Plug>NoVimwikiDiaryIndex <Plug>VimwikiDiaryIndex
nmap <Plug>NoVimwikiMakeDiaryNote <Plug>VimwikiMakeDiaryNote
nmap <Plug>NoVimwikiTabMakeDiaryNote <Plug>VimwikiTabMakeDiaryNote
nmap <Plug>NoVimwikiDiaryGenerateLinks <Plug>VimwikiDiaryGenerateLinks

"nmap <leader>` <Plug>VimwikiIndex

"---------------------------------------------------------------------
" Calendar.vim
let g:calendar_diary=$HOME.'/Dropbox/wiki/diary'

"---------------------------------------------------------------------
" vim-notes
let g:notes_directories = ['~/Dropbox/wiki/vimnotes']
let g:notes_suffix = '.md'

autocmd FileType notes vmap <buffer> <CR> :NoteFromSelectedText<CR>
autocmd FileType notes nmap <buffer> <CR> gf

" disable smart quotes
let g:notes_smart_quotes = 0

"---------------------------------------------------------------------
" vim-airline
let g:airline_theme='wombat'
" let g:airline#extensions#tabline#enabled = 1
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols = {}
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

"---------------------------------------------------------------------
" vim-tmux-navigator
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
" originally binded to C-l, now change to C-d
nnoremap <silent> <C-d> :<C-u>nohlsearch<CR><C-l>
nnoremap <silent> <C-\> :TmuxNavigatePrevious<cr>



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
" javascript

" Simple re-format for minified Javascript
command! UnMinify call UnMinify()
function! UnMinify()
    %s/{\ze[^\r\n]/{\r/g
    %s/){/) {/g
    %s/};\?\ze[^\r\n]/\0\r/g
    %s/;\ze[^\r\n]/;\r/g
    %s/[^\s]\zs[=&|]\+\ze[^\s]/ \0 /g
    normal ggVG=
endfunction

"----------------------------------------------------------------------
" rust

" racer : rust auto completion
let g:racer_cmd = "~/bin/racer/target/release/racer"
let $RUST_SRC_PATH = "/usr/local/src/rust/src/"

"----------------------------------------------------------------------
" rust
let g:vim_markdown_folding_disabled = 1

"===============================================================================
" client specified settings

if has('nvim')
    "--------------------------------------------------
    " neovim specified settings

    "- - - - - - - - - - - - - - - - - - - - - - - - -
    " mappings

    " Fast editing or sourcing vimrc file
    nmap <silent> <leader>ee :tabnew<cr>:edit ~/.nvimrc<cr>

    tnoremap <A-h> <C-\><C-n>:TmuxNavigateLeft<cr>
    tnoremap <A-j> <C-\><C-n>:TmuxNavigateDown<cr>
    tnoremap <A-k> <C-\><C-n>:TmuxNavigateUp<cr>
    tnoremap <A-l> <C-\><C-n>:TmuxNavigateRight<cr>
    tnoremap <A-\> <C-\><C-n>:TmuxNavigatePrevious<cr>

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
