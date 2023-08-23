local Plug = vim.fn["plug#"]

vim.call("plug#begin")

Plug 'andweeb/presence.nvim'
Plug 'ellisonleao/gruvbox.nvim'
Plug ('nvim-treesitter/nvim-treesitter', {['do'] = 'TSUpdate'})
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/plenary.nvim'
Plug ('nvim-telescope/telescope.nvim', {['tag'] = '0.1.2'})
Plug ('akinsho/toggleterm.nvim', {['tag'] = '*'})
--Plug ("berylllium/session-manager", {['tag'] = 'v1.0.0'})
Plug "berylllium/session-manager"
Plug "vim-scripts/DoxygenToolkit.vim"
Plug "mfussenegger/nvim-dap"

vim.call("plug#end")
