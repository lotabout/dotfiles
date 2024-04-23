--------------------------------------------------------------------------------
-- Global imports
local HOME = os.getenv("HOME")
local wiki_directory = HOME .. '/Nextcloud/Vault'

--------------------------------------------------------------------------------
-- install lazy.vim
local lazypath = vim.fn.stdpath("config") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

--------------------------------------------------------------------------------
-- Global Settings

vim.opt.encoding = "utf-8"
vim.opt.termguicolors = true

if vim.o.history < 1000 then
  vim.o.history = 1000
end

if vim.o.tabpagemax < 50 then
  vim.o.tabpagemax = 50
end

-- Set to auto read when a file is changed from the outside
vim.opt.autoread = true

-- Set map leader to Space
vim.mapleader = " "
vim.g.mapleader = " "

-- Show line number
vim.opt.number = true

----------------------------------------------------
-- vim user interface

-- set wildmode
vim.opt.wildmode = { "longest", "list", "full" }

-- ignore certain files and directories when globing
vim.opt.wildignore = {
    '*~',
    '*.o', '*.obj', '*.bin', '*.dll', '*.exe',
    '*.pyc', '*.pyo',
    '*/.git/*', '*/.svn/*', '*/__pycache__/*', '*/build/**',
    '*.DS_Store',
    '*.class', '*/target/*',
    '*/node_modules/*',
    '*.aux', '*.bbl', '*.blg', '*.brf', '*.fls', '*.fdb_latexmk', '*.synctex.gz', '*.pdf',
}

-- always show current position
vim.opt.ruler = true

-- smart about case
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- highlight search
vim.opt.hlsearch = true

-- don't redraw while executing macros (good performance config)
vim.opt.lazyredraw = true

-- show matching brackets when text indicator is over them
vim.opt.showmatch = true

-- how many tenths of a second to blink when matching brackets
vim.opt.mat = 2

-- show the cursor position
vim.opt.cursorline = true

-- always show the status line
vim.opt.laststatus = 2

vim.opt.completeopt = { "longest", "menuone", 'preview' }

-- virtual edit is useful for visual block edit
vim.opt.virtualedit = "block"

----------------------------------------------------
-- files, backups, and undo

-- set possible file encodings
vim.opt.fileencodings = {"ucs-bom", "utf-8", "cp932", "cp936", "gb18030", "big5", "euc-jp", "euc-kr", "latin1"}

-- use unix as the standard file type
vim.opt.fileformats = {"unix", "dos", "mac"}

----------------------------------------------------
-- text, tab, and indent related

-- use spaces instead of tabs on save
vim.opt.expandtab = true

-- be smart when using tabs
vim.opt.smarttab = true

-- 1 tab == 8 spaces
vim.opt.tabstop = 8
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4

-- text width to 78
vim.opt.textwidth = 78

-- smart indent
vim.opt.smartindent = true
vim.opt.cinoptions = { 'l1', 't0', 'g0', 'Ws' }

-- dictionary
vim.opt.dictionary = "/usr/share/dict/words"

----------------------------------------------------
-- moving around, tabs, windows and buffers

-- treat long lines as break lines(like emacs)
vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'gj', 'j')
vim.keymap.set('n', 'k', 'gk')
vim.keymap.set('n', 'gk', 'k')

-- swap these two because ' is easier to type than `
vim.keymap.set('n', "'", '`')
vim.keymap.set('n', '`', "'")

-- return to last edit position when opening files
vim.api.nvim_create_autocmd({'BufReadPost'}, {
    pattern = '*',
    callback = function(ev)
        -- if last editing line exists, then jump to it
        if vim.fn.line("'\"") > 0 and vim.fn.line("'\"") <= vim.fn.line("$") then
            vim.cmd [[normal! g`"zz]]
        end
    end
})

-- enable backupcopy for crontab
vim.api.nvim_create_autocmd({'FileType'}, {
    pattern = 'crontab',
    callback = function(ev)
        vim.local_opts.backupcopy = 'yes'
    end
})

----------------------------------------------------
-- settings for programming

vim.o.formatoptions = 'tclqronmMj'

----------------------------------------------------
-- settings for self-added plugins & settings

--------------------------------------------------------------------------------
-- mappings

----------------------------------------------------
-- handy keys for copy & paste
vim.keymap.set({'v', 'n'}, '<Leader>y', '"+y')
vim.keymap.set({'v', 'n'}, '<Leader>Y', '"+Y')
vim.keymap.set({'v', 'n'}, '<Leader>p', '"+p')
vim.keymap.set({'v', 'n'}, '<Leader>P', '"+P')

----------------------------------------------------
-- borrowed from vim-unimpaired

vim.keymap.set('n', '[a', ':prev<CR>', {silent = true})
vim.keymap.set('n', ']a', ':next<CR>', {silent = true})
vim.keymap.set('n', '[A', ':first<CR>', {silent = true})
vim.keymap.set('n', ']A', ':last<CR>', {silent = true})

-- buffer related
vim.keymap.set('n', '[b', ':bprev<CR>', {silent = true})
vim.keymap.set('n', ']b', ':bnext<CR>', {silent = true})
vim.keymap.set('n', '[B', ':bfirst<CR>', {silent = true})
vim.keymap.set('n', ']B', ':blast<CR>', {silent = true})

-- quickfix related
vim.keymap.set('n', '[q', ':cprev<CR>', {silent = true})
vim.keymap.set('n', ']q', ':cnext<CR>', {silent = true})
vim.keymap.set('n', '[Q', ':cfirst<CR>', {silent = true})
vim.keymap.set('n', ']Q', ':clast<CR>', {silent = true})
vim.keymap.set('n', '[l', ':lprev<CR>', {silent = true})
vim.keymap.set('n', ']l', ':lnext<CR>', {silent = true})
vim.keymap.set('n', '[L', ':lfirst<CR>', {silent = true})
vim.keymap.set('n', ']L', ':llast<CR>', {silent = true})

-- tab related
vim.keymap.set('n', '[t', ':tabprev<CR>', {silent = true})
vim.keymap.set('n', ']t', ':tabnext<CR>', {silent = true})
vim.keymap.set('n', '[T', ':tabfirst<CR>', {silent = true})
vim.keymap.set('n', ']T', ':tablast<CR>', {silent = true})

-- disable highlight search for current search
vim.keymap.set('n', '<C-l>', ':<C-u>nohlsearch<CR><C-l>')


-- Repeating last substitution
vim.keymap.set({'n', 'x'}, '&', ':&&<CR>')

-- select last pasted texts
vim.keymap.set('n', 'gp', function()
    return '`[' .. vim.fn.strpart(vim.fn.getregtype(), 0, 1) .. '`]'
end, {expr = true})

-- Scroll without moving cursor screen line
vim.keymap.set('n', '<C-J>',  '<C-E>j')
vim.keymap.set('v', '<C-J>',  '<C-E>j')
vim.keymap.set('n', '<C-K>',  '<C-Y>k')
vim.keymap.set('v', '<C-K>',  '<C-Y>k')

-- neovim makes Y = y$
vim.keymap.set('n', 'Y', 'yy')

----------------------------------------------------
-- keys for wiki markdown files

function create_mapping_for_personal_wiki()
    vim.keymap.set('n', '<C-p>', ':Files ' .. wiki_directory .. '<CR>', {buffer=true})
    vim.keymap.set('n', '<Leader>/', ':Rg ' .. wiki_directory .. '<CR>', {buffer=true})
    vim.keymap.set('n', '<C-n>', ':ObsidianNew ', {buffer=true})
end

function bind_for_wiki_files()
    if vim.startswith(vim.fn.expand('%:p'), wiki_directory) then
        create_mapping_for_personal_wiki()
    end
end

vim.api.nvim_create_autocmd({'FileType'}, {
    pattern = 'markdown',
    callback = bind_for_wiki_files,
})

----------------------------------------------------
-- Shortcuts

-- Remove the Windows ^M - when the encodings gets messed up
vim.keymap.set('n', '<Leader>mm', "mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm")

-- strip the spaces at the end of line
vim.keymap.set('n', '<leader><Space><Space>', ':%s/\\s\\+$//<cr>:<C-u>nohlsearch<CR>')

-- merge multiple continuous lines into one.
vim.keymap.set('n', '<leader><cr>', ':%s/\\(^[[:blank:]]*\\n\\)\\{2,}/\\r/<cr>')

-- quick save
vim.keymap.set('n', '<Leader>w', ':<C-u>nohlsearch<CR>:w<CR>')

-- shortcuts for editing init
vim.keymap.set('n', '<Leader>ee', ':tabnew<cr>:edit ~/.config/nvim/init.lua<cr>', {silent = true})

----------------------------------------------------
-- zoom current buffer

function zoom_win()
    -- check if is the zoomed state (tabnumber > 1 && window == 1)
    if vim.fn.tabpagenr('$') > 1 and vim.fn.tabpagewinnr(vim.fn.tabpagenr(), '$') == 1 then
        local cur_winview = vim.fn.winsaveview()
        local cur_bufname = vim.fn.bufname()
        vim.cmd.tabclose()

        -- restore the view
        if cur_bufname == vim.fn.bufname() then
            vim.fn.winrestview(cur_winview)
        end
    else
        vim.cmd [[ tab split ]]
    end
end

vim.keymap.set('n', '<Leader>z', ':lua zoom_win()<CR>', {silent = true})

----------------------------------------------------
-- toggle wrap and set virtualedit

function toggle_wrap()
    if vim.wo.wrap then
        print("wrap off")
        vim.wo.wrap = false
        vim.wo.virtualedit = "all"
    else
        print("wrap on")
        vim.wo.wrap = true
        vim.wo.virtualedit = "block"
    end
end

vim.api.nvim_create_user_command('ToggleWrap', toggle_wrap, { nargs = 0 })

----------------------------------------------------
-- Quick open all files in quickfix window
function quickfix_open_all()
    if next(vim.fn.getqflist()) == nil then
        return
    end
    local prev_val = ""
    for _, val in ipairs(vim.fn.getqflist()) do
        local cur_val = vim.fn.bufname(val.bufnr)
        if prev_val ~= cur_val then
            vim.cmd('e ' .. cur_val)
        end
        prev_val = cur_val
    end
end

vim.api.nvim_create_user_command('QuickfixOpenAll', quickfix_open_all, { nargs = 0 })

----------------------------------------------------
-- Iterleave lines two blocks
-- Usage:
-- - 90,100 Interleave 10
-- - '<,'> Interleave 'a

function interleave(args)
    local tgt_start = args.fargs[1]
    if vim.startswith(tgt_start, "'") then
        tgt_start = vim.api.nvim_buf_get_mark(0, tgt_start:sub(2, 2))[1]
    end
    tgt_start = tonumber(tgt_start)

    local src_start = math.min(args.line1, args.line2)
    local src_end = math.max(args.line1, args.line2)
    if tgt_start >= src_start and tgt_start <= src_end then
        return
    end

    local source_lines = vim.api.nvim_buf_get_lines(0, src_start - 1, src_end, false)
    local tgt_end = tgt_start + #source_lines - 1
    local target_lines = vim.api.nvim_buf_get_lines(0, tgt_start - 1, tgt_end, false)

    local new_lines = {}
    for i = 1,#source_lines do
        new_lines[#new_lines + 1] = target_lines[i]
        new_lines[#new_lines + 1] = source_lines[i]
    end

    vim.api.nvim_buf_set_lines(0, src_start - 1, src_end, false, {}) -- delete src
    vim.api.nvim_buf_set_lines(0, tgt_start - 1, tgt_end, false, new_lines)
end

vim.api.nvim_create_user_command('Interleave', interleave, { nargs = 1, range = true })

vim.keymap.set('v', '<Leader>j', ':Interleave ')

--------------------------------------------------------------------------------
-- Package Manager

require('lazy').setup({

    checker = {enabled = false},

    --------------------------------------------------
    -- UI enhancement

    -- solarized color theme
    {
        'lifepillar/vim-solarized8',
        branch = 'neovim',
        config = function()
            vim.g.solarized_extra_hi_groups = true
            vim.cmd [[ colorscheme solarized8 ]]
        end
    },

    -- status line enhancement
    {
        'itchyny/lightline.vim',
        config = function()
            vim.g.lightline = {
                colorscheme = 'solarized',
                active = {
                    left = {
                        { 'mode', 'paste' },
                        { 'gitbranch', 'readonly', 'relativepath', 'modified' },
                    },
                },
                component_function = {
                    gitbranch = 'FugitiveHead',
                },
            }
        end
    },

    -- tagbar
    {
        'majutsushi/tagbar',
        dependencies = {
            {'lvht/tagbar-markdown', ft='markdown'},
        },
        cmd = {'Tagbar'},
        keys = {{'<Leader>tl', ':TagbarToggle<CR>', silent=true}},
        config = function()
            vim.api.nvim_create_autocmd({'FileType'}, {
                pattern = 'markdown',
                callback = function() vim.g.tagbar_sort = 0 end,
            })
        end
    },

    -- file browser
    {
        'preservim/nerdtree',
        lazy = true,
        dependencies = {"Xuyuanp/nerdtree-git-plugin"},
        keys = {
            {"<Leader>ne", ":NERDTreeToggle<CR>", desc="toggle nerdtree"},
            {"<Leader>nf", ":NERDTreeFind<CR>", desc="locate current file in nerdtree"},
        },
    },

    -- start screen
    {
        'mhinz/vim-startify',
        config = function()
            vim.api.nvim_create_autocmd({'FileType'}, {
                pattern = 'startify',
                callback = create_mapping_for_personal_wiki,
            })
        end
    },

    --------------------------------------------------
    -- Basic feature enhancement
    
    {
        'moll/vim-bbye',
        keys = {
            {"<Leader>q", ":Bdelete<CR>"},
        },
    },

    -- enhance s command
    {
        'tpope/vim-abolish',
        cmd = {'Subvert', 'S', 'Abolish'},
    },

    -- form vim's sudo tee trick
    {
        'tpope/vim-eunuch',
        cmd = {'Delete', 'Move', 'Rename', 'Chmod', 'Mkdir', 'SudoWrite', 'SudoEdit'},
    },

    -- text objects
    {
        'kana/vim-textobj-indent',
        dependencies = {'kana/vim-textobj-user'},
    },
    {
        'wellle/targets.vim',
    },

    -- auto pair
    {
        'cohama/lexima.vim',
    },

    --------------------------------------------------
    -- completion framework

    {
        'github/copilot.vim',
        config = function()
            vim.g.copilot_no_tab_map = true
            vim.keymap.set('i', '<Plug>(vimrc:copilot-dummy-map)', 'copilot#Accept("<Tab>")', {expr = true})
        end
    },

    -- LSP config
    {
        'neovim/nvim-lspconfig',
        config = function()
            require('lspconfig').bashls.setup{}
            require('lspconfig').clangd.setup{}
            require('lspconfig').cmake.setup{}
            require('lspconfig').gopls.setup{}
            require('lspconfig').pyright.setup{}
            require('lspconfig').rust_analyzer.setup{}
            require('lspconfig').texlab.setup{}
            require('lspconfig').vimls.setup{}
            require('lspconfig').yamlls.setup{}
        end
    },

    {
        "williamboman/mason.nvim",
        cmd = {'Mason', 'MasonInstall', 'MasonUninstall', 'MasonUpdate', 'MasonList', 'MasonLog'},
        opts = {},
    },

    -- nvim-cmp
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'hrsh7th/cmp-nvim-lsp',
            'hrsh7th/cmp-buffer',
            'hrsh7th/cmp-path',
            'hrsh7th/cmp-cmdline',
            'saadparwaiz1/cmp_luasnip',
            {"L3MON4D3/LuaSnip", version = "v2.*"},
        },
        config = function()
            local cmp = require('cmp')
            local has_words_before = function()
                unpack = unpack or table.unpack
                local line, col = unpack(vim.api.nvim_win_get_cursor(0))
                return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
            end
            cmp.setup {
                snippet = {
                    -- REQUIRED - you must specify a snippet engine
                    expand = function(args)
                        require('luasnip').lsp_expand(args.body)
                    end,
                },
                mapping = cmp.mapping.preset.insert({
                    ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
                    -- supertab like
                    ["<Tab>"] = cmp.mapping(function(fallback)
                        luasnip = require('luasnip')
                        if vim.fn.exists('*copilot#GetDisplayedSuggestion') ~= 0 and vim.fn['copilot#GetDisplayedSuggestion']()['text'] ~= '' then
                            vim.api.nvim_feedkeys(vim.fn['copilot#Accept'](vim.api.nvim_replace_termcodes('<Tab>', true, true, true)), 'n', true)
                        elseif cmp.visible() then
                            cmp.select_next_item()
                            -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable() 
                            -- that way you will only jump inside the snippet region
                        elseif luasnip.expand_or_jumpable() then
                            luasnip.expand_or_jump()
                        elseif has_words_before() then
                            cmp.complete()
                        else
                            fallback()
                        end
                    end, { "i", "s" }),
                    ["<S-Tab>"] = cmp.mapping(function(fallback)
                        luasnip = require('luasnip')
                        if cmp.visible() then
                            cmp.select_prev_item()
                        elseif luasnip.jumpable(-1) then
                            luasnip.jump(-1)
                        else
                            fallback()
                        end
                    end, { "i", "s" }),
                }),

                experimental = {
                    ghost_text = false -- this feature conflict with copilot.vim's preview.
                },

                sources = cmp.config.sources({
                    { name = 'nvim_lsp' },
                    { name = 'luasnip' },
                }, {
                    { name = 'buffer' },
                }),
            }
        end
    },

    --------------------------------------------------
    -- Handy commans

    -- alignment
    {
        'junegunn/vim-easy-align',
        keys = {{"ga", "<Plug>(EasyAlign)", mode='x'}},
    },

    -- format
    {
        'sbdchd/neoformat',
        keys = {{'<Leader><Leader>f', ':Neoformat<CR>', silent=true}},
    },

    -- gundo
    {
        'sjl/gundo.vim',
        cmd = {'GundoToggle'},
        keys = {{'<F5>', ':GundoToggle<CR>'}},
        config = function()
            vim.g.gundo_prefer_python3 = true
        end
    },

    -- close all bufs but current
    {
        'schickling/vim-bufonly',
        cmd = {'BufOnly', 'BOnly'},
    },


    -- git integration
    {
        'tpope/vim-fugitive',
        dependencies = {
            'tpope/vim-rhubarb',
            'shumphrey/fugitive-gitlab.vim'
        },
        config = function()
            vim.g.fugitive_gitlab_domains = { 'https://gitlab.4pd.io' }
        end
    },

    -- gitgutter
    {
        'airblade/vim-gitgutter',
        lazy = false,
        keys = {
            {"]g", ":GitGutterNextHunk<CR>"},
            {"[g", ":GitGutterPrevHunk<CR>"},
        },
        config = function()
            vim.g.gitgutter_map_keys = 0
        end
    },

    -- tool: dirdiff
    {
        'will133/vim-dirdiff',
        cmd = {'DirDiff'},
    },

    -- hop (easymotion like)
    {
        'phaazon/hop.nvim',
        keys = {
            {"<Leader>l", ":HopLine<CR>"},
            {"<Leader>f", ":HopWord<CR>"},
        },
        config = function()
            require('hop').setup()
        end
    },

    --------------------------------------------------
    -- Better Defaults

    -- tool: disable some features for faster opening large file
    'vim-scripts/LargeFile',

    --------------------------------------------------
    -- Integration with Linux

    -- send text to tmux's pane
    {
        'lotabout/slimux',
        keys = {
            {"<Leader>s", ":SlimuxREPLSendLine<CR>"},
            {"<Leader>s", ":SlimuxREPLSendLine<CR>", mode='v'},
        },
        config = function()
            vim.g.slimux_select_from_current_window = true

            vim.g.slimux_scheme_keybindings = true
            vim.g.slimux_scheme_leader = ';'
            vim.g.slimux_racket_keybindings = true
            vim.g.slimux_racket_leader = ';'
            vim.g.slimux_racket_xrepl = true
            vim.g.slimux_clojure_keybindings = true
            vim.g.slimux_clojure_leader = ';'
            vim.g.slimux_clojure_xrepl = true
            vim.g.slimux_python_use_ipython = true
        end
    },

    -- Find file & Search
    {
        'lotabout/skim.vim',
        dependencies = {
            {
                'lotabout/skim',
                dir = '~/.skim',
                run = './install'
            }
        },
        keys = {
            {"<C-p>", ":Files<CR>"},
            {"<Leader>/", ":Rg<CR>", {remap=false}},
            {"<Leader>i", ":BTags<CR>", {remap=false}},
            {"<Leader>I", ":Tags<CR>", {remap=false}},
            {"<C-e>", ":Buffers<CR>", {remap=false}},
        },
        cmd = {'Files', 'Rg', 'BTags', 'Tags', 'Buffers'},
        config = function()
            vim.g.skim_history_dir = "~/.skim-history"
            vim.g.fzf_preview_window = 'down:50%'
            vim.fn.setenv('SKIM_DEFAULT_COMMAND', '(fd --type f || git ls-files -c -o --exclude-standard || rg -l "" || ag -l -g "" || find . -type f)')
            vim.fn.setenv('SKIM_DEFAULT_OPTIONS', '--bind ctrl-f:toggle,ctrl-p:up,ctrl-n:down,up:previous-history,down:next-history,alt-h:scroll-left(5),alt-l:scroll-right(5),alt-p:toggle-preview --layout=reverse')
        end
    },

    --------------------------------------------------
    -- support more filetypes

    -- highlight
    {
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            'nvim-treesitter/nvim-treesitter-textobjects',
            'andymass/vim-matchup',
        },
        run = ':TSUpdate',
        config = function()
            require('nvim-treesitter.configs').setup({
                ensure_installed = { "vim", "c", "cpp", "java", "python", "bash", "css", "go", "lua", "javascript", "yaml", "tsx", "json", "rust", 'markdown', 'markdown_inline'}, -- one of "all", "maintained" (parsers with maintainers), or a list of languages
                ignore_install = {}, -- List of parsers to ignore installing
                highlight = {
                    enable = true,
                },
                textobjects = { enable = true },
                disable = function(lang, buf)
                    local max_filesize = 100 * 1024 -- 100 KB
                    local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
                    if ok and stats and stats.size > max_filesize then
                        return true
                    end
                end,
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
                matchup = {
                    enable = true,
                },
            });
        end
    },

    -- comment
    {
        'tpope/vim-commentary',
        keys = {
            {"gcc", ":Commentary<CR>", silent=true},
            {"gcc", ":Commentary<CR>", mode='v', silent=true},
        },
    },

    -- markdown
    {
        'lotabout/vim-markdown',
        ft = {'markdown'},
        config = function()
            vim.g.vim_markdown_folding_disabled = 1
            vim.g.vim_markdown_frontmatter = 1
            vim.g.vim_markdown_follow_anchor = 1
            vim.g.vim_markdown_conceal = 1
            vim.g.tex_conceal = ""
            vim.g.vim_markdown_math = 1
            vim.g.vim_markdown_no_extensions_in_markdown = 1
            vim.g.vim_markdown_highlight_with_sed = 1
        end
    },

    {
        'dkarter/bullets.vim',
        keys = {
            {"<C-Space>", ":ToggleCheckbox<CR>", silent=true, ft={'markdown', 'text', 'gitcommit'}},
            {"<C-@>", ":ToggleCheckbox<CR>", silent=true, ft={'markdown', 'text', 'gitcommit'}},
        },
        config = function()
            vim.g.bullets_set_mappings = 0
        end
    },

    'lotabout/orgmark.vim',
    
    --------------------------------------------------
    -- Others

    {
        'christoomey/vim-tmux-navigator',
        keys = {
            {'<C-h>', ':TmuxNavigateLeft<CR>', silent=true},
            {'<C-j>', ':TmuxNavigateDown<CR>', silent=true},
            {'<C-k>', ':TmuxNavigateUp<CR>', silent=true},
            {'<C-l>', ':TmuxNavigateRight<CR>', silent=true},
            {'<C-d>', ':<C-u>nohlsearch<CR><C-l>', silent=true},
            {'<C-\\>', ':TmuxNavigatePrevious<CR>', silent=true},
        },
    },

    {
        'lotabout/calendar-vim',
        keys = {
            {'<Leader>cal', '<Plug>CalendarV', silent=true},
            {'<Leader>caL', '<Plug>CalendarH', silent=true},
        },
        config = function()
            vim.g.calendar_no_mappings = 1
            vim.g.calendar_diary_filename_pat = '%04d-%02d-%02d'
            vim.g.calendar_filetype = 'markdown'
            vim.g.calendar_diary= wiki_directory .. '/Z91-diary'
        end
    },

    'wakatime/vim-wakatime',

    {
        'epwalsh/obsidian.nvim',
        dependencies = {'nvim-lua/plenary.nvim'},
        event = {
            "BufReadPre " .. wiki_directory .. "/**.md",
            "BufNewFile " .. wiki_directory .. "/**.md",
        },
        cmd = {'ObsidianNew', 'ObsidianToday'},
        opts = {
            workspaces = {
                {
                    name = "Personal",
                    path = wiki_directory,
                }
            },

            notes_subdir = 'Z94-slipbox',

            -- Optional, set to true if you don't want obsidian.nvim to manage frontmatter.
            disable_frontmatter = true,

            templates = {
                subdir = "Z92-templates",
                date_format = "%Y-%m-%d",
                time_format = "%H:%M",
                -- A map for custom variables, the key should be the variable and the value a function
                substitutions = {}
            },
            daily_notes = {
                folder = "Z91-diary",
                date_format = "%Y-%m-%d",
                template = 'Z92.01-Diary.md',
            },
            note_id_func = function(title)
                -- Create note IDs in a Zettelkasten format with a timestamp and a suffix.
                -- In this case a note with the title 'My new note' will given an ID that looks
                -- like '202311141112-my-new-note', and therefore the file name '202311141112-my-new-note.md'
                local suffix = ""
                if title ~= nil then
                    -- If title is given, transform it into valid file name.
                    suffix = title:gsub("[ 　]", "-")
                else
                    -- If title is nil, just add 4 random uppercase letters to the suffix.
                    for _ = 1, 4 do
                        suffix = suffix .. string.char(math.random(65, 90))
                    end
                end
                return os.date('%Y%m%d%H%M', os.time()) .. "-" .. suffix
            end,
            mappings = {
                -- Overrides the 'gf' mapping to work on markdown/wiki links within your vault.
                ["gf"] = {
                    action = function()
                        return require("obsidian").util.gf_passthrough()
                    end,
                    opts = { noremap = false, expr = true, buffer = true },
                },
            },
            completion = {
                new_notes_location = "Z94-slipbox",
            },
        },
        config = function(plugin, opts)
            local client = require('obsidian').setup(opts);

            -- integrate with calendar.vim
            open_daily_note = function(day, month, year, week, dir)
                local date = os.time{year=year, month=month, day=day}
                local note = client:_daily(date)
                vim.api.nvim_command('wincmd w')
                vim.api.nvim_command('edit ' .. tostring(note.path))
            end

            -- vim do not support functionref, implement a wrapper, note that
            -- v:lua's functions should be global
            vim.api.nvim_exec([[
                function! ObsidianOpenDate(day, month, year, week, dir)
                    call v:lua.open_daily_note(a:day, a:month, a:year, a:week, a:dir)
                endfunc
            ]], false)
            vim.g.calendar_action = 'ObsidianOpenDate'
        end
    }
})
