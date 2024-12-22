return {
    {
        -- Theme inspired modus-themes from Emacs
        "miikanissi/modus-themes.nvim",
        priority = 1000, -- Make sure to load this before all the other start plugins.
        config = function()
            vim.opt.background = "light"
            vim.cmd([[colorscheme modus]]) -- modus_operandi, modus_vivendi

            -- configure highlights by doing something like
            vim.cmd.hi("Comment gui=none")
        end,
    },

    {
        -- Set lualine as statusline
        "nvim-lualine/lualine.nvim",
        priority = 1000, -- Make sure to load this before all the other start plugins.
        opts = {
            options = {
                icons_enabled = false,
                theme = "auto",
                component_separators = "|",
                section_separators = "",
            },
        },
    },
}
