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

" redirect runtime path to .vim, so that vim and neovim can share directories.
if has('nvim')
    set rtp+=~/.vim/
endif

" Inform that the script is written in UTF-8 (including special characters)
scriptencoding utf-8

" Use Vim settings instead of vi settings.
set nocompatible

" Set how many lines of history VIM has to remember
set history=700

" Enable filetype plugins
filetype plugin indent on

" Set to auto read when a file is changed from the outside
set autoread

" Set map leader
let mapleader = ","
let g:mapleader = ","

" Show line number
set nu

"----------------------------------------------------------------------
" VIM user interface

" Turn on the WiLd menu
set wildmenu
set wildmode=longest,list,full

" Ignore compiled files
set wildignore+=*.o,*~,*.pyc
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*

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
    colorscheme obsidian
    set guifont=Dejavu\ Sans\ Mono\ 11
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
set fileencodings=ucs-bom,utf-8,cp936,gb18030,big5,euc-jp,euc-kr,latin1

" Use Unix as the standard file type
set ffs=unix,dos,mac

" Turn backup off.
" set nobackup

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
noremap k gk
noremap gj j
noremap gk k

" Close current buffer
" map <leader>q :bdelete<cr>
map <leader>k :call MyBufferDelete()<cr>
" need vim-bbye
map <leader>q :Bdelete<cr>

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
" Misc Maps (for convenience)

" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>mm mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Toggle paste mode on and off
nmap <leader>p :setlocal paste!<cr>

" strip the spaces at the end of line
nmap <leader><Space> :%s/\s\+$//<cr>

" merge multiple continuous lines into one.
nmap <leader><cr> :%s/\(^[[:blank:]]*\n\)\{2,}/\r/<cr>

"===============================================================================
" Settings for Programming

"----------------------------------------------------------------------
" Extra Settings

set formatoptions=tclqron
runtime macros/matchit.vim

" grep, ignore some directories.
set grepprg=grep\ -nrI\ --exclude-dir=target\ --exclude-dir=tmp\ --exclude=\"*.min.js\"\ --exclude=\"tags\"\ $*\ /dev/null

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

nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

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


"----------------------------------------------------------------------
" vimim

set encoding=utf-8
"let fileencodings=ucs-bom,utf8,chinese,taiwan,ansi
if has ("win32")
    set guifont=Courier_New:h12:w7
    set guifontwide=NSimSun-18030,NSimSun
endif
"set ambiwidth=double
" let g:vimim_map='c-bslash'



"===============================================================================
" vundle settings
let use_vundle = 1 " 0 = not use vundle

if use_vundle == 1
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
    Bundle 'https://github.com/xolox/vim-misc.git'
    Bundle 'https://github.com/xolox/vim-session.git'
    Bundle 'https://github.com/kshenoy/vim-signature'
    Bundle 'moll/vim-bbye'
    Bundle 'surround.vim'
    Bundle 'repeat.vim'
    Bundle 'Gundo'
    Bundle 'yueyoum/vim-alignment'
    Bundle "Align"
    " Bundle 'matchit.zip'
    Bundle "ervandew/supertab"
    Bundle 'autofmt'

    "Bundle 'mru.vim'
    Bundle 'abolish.vim'

    "Bundle 'Valloric/YouCompleteMe'
    Plugin 'bling/vim-airline'

    "------------------------------------------------------------------
    " Integration with Linux environment
    "------------------------------------------------------------------
    "Bundle 'epeli/slimux'
    Bundle 'lotabout/slimux'
    Bundle 'fakeclip'
    Bundle 'ctrlp.vim'

    " work with git
    Bundle 'fugitive.vim'
    " Bundle 'https://github.com/oplatek/Conque-Shell'

    "------------------------------------------------------------------
    " Support more filetype specific feature
    "------------------------------------------------------------------
    Bundle 'The-NERD-Commenter'
    "Plugin 'UltiSnips'
    Bundle "honza/vim-snippets"

    Plugin 'Syntastic' " syntax checker

    " Bundle 'LaTeX-Suite-aka-Vim-LaTeX'
    " Bundle 'jcf/vim-latex.git'
    " Bundle 'rosenfeld/conque-term'
    "Bundle 'slimv.vim'
    Bundle 'paredit.vim'
    " Bundle 'ivanov/vim-ipython.git'
    "Bundle 'Yggdroot/indentLine'
    Bundle 'https://github.com/davidhalter/jedi-vim.git'

    "Bundle 'https://github.com/wlangstroth/vim-racket'
    Bundle "mattn/emmet-vim"

    "Bundle 'Rip-Rip/clang_complete'

    " for javascript
    Plugin 'marijnh/tern_for_vim'


    Plugin 'tpope/vim-fireplace'

    "------------------------------------------------------------------
    " Others
    "------------------------------------------------------------------
    "Bundle 'christoomey/vim-tmux-navigator'
    "Bundle 'https://github.com/mattn/calendar-vim'
    Bundle 'vimwiki'
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
else
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
map <Leader>ss :SlimuxREPLSendLine<CR>
vmap <Leader>ss :SlimuxREPLSendSelection<CR>
map <Leader>sl :SlimuxShellLast<CR>
map <leader>bl :SlimuxREPLSendBuffer<CR>

let g:slimux_scheme_keybindings=1
let g:slimux_scheme_leader=';'
let g:slimux_racket_keybindings=1
let g:slimux_racket_leader=';'
let g:slimux_racket_xrepl=1


"---------------------------------------------------------------------
" fakeclip
let g:fakeclip_terminal_multiplexer_type = "tmux"

"---------------------------------------------------------------------
" autofmt
nmap <leader>f :setlocal formatexpr=autofmt#japanese#formatexpr()<CR>

"---------------------------------------------------------------------
" Gundo
nnoremap <F5> :GundoToggle<CR>


"---------------------------------------------------------------------
" vimwiki
let wiki_1 = {}
let wiki_1.path = '~/Dropbox/vimwiki'
"let wiki_1.path_html = '~/Dropbox/vimwiki_html'
let wiki_1.path_html = '~/repos/vimwiki_html'
let wiki_1.template_path= wiki_1.path_html . '/template'
let wiki_1.template_default = 'default'
let wiki_1.template_ext = '.htm'
let wiki_1.syntax = 'markdown'
let wiki_1.ext = '.md'

let g:vimwiki_list = [wiki_1]
map <F4> :VimwikiAll2HTML<cr> :exec '!cd '.VimwikiGet('path_html').'; ./sync.sh'<cr>

"---------------------------------------------------------------------
" vim-airline
let g:airline#extensions#tabline#enabled = 1

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

"===============================================================================
" client specified settings

if has('nvim')
    "--------------------------------------------------
    " neovim specified settings

    "- - - - - - - - - - - - - - - - - - - - - - - - -
    " mappings

    " Fast editing or sourcing vimrc file
    nmap <silent> <leader>ee :tabnew<cr>:edit ~/.nvimrc<cr>

    " integrate with nvim terminal
    tnoremap <A-h> <C-\><C-n><C-w>h
    tnoremap <A-j> <C-\><C-n><C-w>j
    tnoremap <A-k> <C-\><C-n><C-w>k
    tnoremap <A-l> <C-\><C-n><C-w>l

else
    "--------------------------------------------------
    " vim specified settings

    "- - - - - - - - - - - - - - - - - - - - - - - - -
    " default settings

    " use blowfish as default crypt method
    set cryptmethod=blowfish

    "- - - - - - - - - - - - - - - - - - - - - - - - -
    " mappings

    " Fast editing or sourcing vimrc file
    nmap <silent> <leader>ee :tabnew<cr>:edit ~/.vimrc<cr>

endif
