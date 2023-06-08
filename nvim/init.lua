-- Berrys init.lua

-- Initialize vim-plug plugins.

local Plug = vim.fn["plug#"]

vim.call("plug#begin")

Plug 'andweeb/presence.nvim'
--Plug 'preservim/nerdtree'
--Plug 'gruvbox-community/gruvbox'
Plug 'ellisonleao/gruvbox.nvim'
Plug ('nvim-treesitter/nvim-treesitter', {['do'] = 'TSUpdate'})
--Plug 'tpope/vim-fugitive'

vim.call("plug#end")

-- General settings

vim.o.background = "dark"
vim.cmd([[colorscheme gruvbox]])

vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

vim.opt.colorcolumn = "120"

vim.keymap.set("i", "jk", "<ESC>")

vim.keymap.set("n", "<C-k>", ":b#<Cr>")

--vim.keymap.set("n", "<F7>", ":tabp<CR>")
--vim.keymap.set("n", "<F8>", ":tabn<CR>")

-- Discord Presence Settings

vim.g.presence_show_time = 0
vim.g.presence_main_image = "file"

-- NERDTree Settings

--vim.keymap.set("n", "<C-n>", ":NERDTreeToggle<CR>")

-- TreeSitter Settings

local status_ok, configs = pcall(require, "nvim-treesitter.configs")

if not status_ok then
	return
end

configs.setup(
{
	ensure_installed = {"cpp", "lua", "c", "cmake", "make", "markdown_inline"},
	sync_install = false,
	ignore_install = {""},
	
	highlight = {
		enable = true,
		disable = {""},
		additional_vim_regex_highlighting = true
	}
}
);

