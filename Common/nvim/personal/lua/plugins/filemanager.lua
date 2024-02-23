return {
    {
        -- A vim-vinegar like file explorer that lets you edit your filesystem like a normal Neovim buffer.
        'stevearc/oil.nvim',
        priority = 1000,
        opts = {},
        config = function()
            vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
            require("oil").setup()
        end
        -- Optional dependencies
        -- dependencies = { "nvim-tree/nvim-web-devicons" },
    }
}
