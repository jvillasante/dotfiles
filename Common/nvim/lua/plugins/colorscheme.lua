return {
    {
        "miikanissi/modus-themes.nvim",
        priority = 1000,
        lazy = false,
        config = function()
            vim.o.background = "light"
            vim.cmd([[colorscheme modus]]) -- modus_operandi, modus_vivendi
        end
    }
}
