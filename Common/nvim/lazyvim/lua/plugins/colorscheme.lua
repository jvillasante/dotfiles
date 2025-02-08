return {
    -- Theme inspired modus-themes from Emacs
    {
        "miikanissi/modus-themes.nvim",
        lazy = true,
        config = function()
            vim.opt.background = "light"
            -- vim.cmd([[colorscheme modus]]) -- modus_operandi, modus_vivendi

            -- configure highlights by doing something like
            vim.cmd.hi("Comment gui=none")
        end,
    },

    -- Configure LazyVim to load modus-themes
    {
        "LazyVim/LazyVim",
        opts = {
            colorscheme = "modus_operandi",
        },
    },
}
