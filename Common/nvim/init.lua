-- global options
vim.cmd("set nocompatible")          -- disable compatibility to old-time vi
vim.cmd("set showmatch")             -- show matching
vim.cmd("set ignorecase")            -- case insensitive
vim.cmd("set hlsearch")              -- highlight search
vim.cmd("set incsearch")             -- incremental search
vim.cmd("set expandtab")             -- converts tabs to white space
vim.cmd("set tabstop=4")             -- number of columns occupied by a tab
vim.cmd("set softtabstop=4")         -- see multiple spaces as tabstops so <BS> does the right thing
vim.cmd("set shiftwidth=4")          -- width for autoindents
vim.cmd("set autoindent")            -- indent a new line the same amount as the line just typed
vim.cmd("set number")                -- add line numbers
vim.cmd("set wildmode=longest,list") -- get bash-like tab completions
vim.cmd("set mouse=v")               -- middle-click paste with
vim.cmd("set mouse=a")               -- enable mouse click
vim.cmd("set clipboard=unnamedplus") -- using system clipboard
vim.cmd("set cursorline")            -- highlight current cursorline
vim.cmd("set ttyfast")               -- speed up scrolling in Vim

-- open new split panes to right and below
vim.cmd("set splitright")
vim.cmd("set splitbelow")

-- Leader
vim.g.mapleader = " "

-- Navigate vim panes better
vim.keymap.set('n', '<c-k>', ':wincmd k<CR>')
vim.keymap.set('n', '<c-j>', ':wincmd j<CR>')
vim.keymap.set('n', '<c-h>', ':wincmd h<CR>')
vim.keymap.set('n', '<c-l>', ':wincmd l<CR>')

-- search off
vim.keymap.set('n', '<leader>h', ':nohlsearch<CR>')
