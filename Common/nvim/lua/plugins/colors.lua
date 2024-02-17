return {
    {
        -- Theme inspired modus-themes from Emacs
        "miikanissi/modus-themes.nvim",
        priority = 1000,
        lazy = false,
        config = function()
            vim.o.background = "light"
            vim.cmd([[colorscheme modus]]) -- modus_operandi, modus_vivendi
        end
    },

    {
        -- Set lualine as statusline
        'nvim-lualine/lualine.nvim',
        opts = {
            options = {
                icons_enabled = false,
                theme = 'auto',
                component_separators = '|',
                section_separators = '',
            },
        },
    }
}
