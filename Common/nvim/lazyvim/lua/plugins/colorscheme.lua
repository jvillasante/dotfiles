return {
    { -- Theme inspired modus-themes from Emacs
        "miikanissi/modus-themes.nvim",
        priority = 1000, -- Make sure to load this before all the other start plugins.
        config = function()
            vim.opt.background = "light"
            -- vim.cmd([[colorscheme modus]]) -- modus_operandi, modus_vivendi
        end,
    },

    -- Configure LazyVim to load gruvbox
    {
        "LazyVim/LazyVim",
        opts = {
            colorscheme = "modus",
        },
    },
}
