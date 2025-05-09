local util = require("util")

vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.signcolumn = "number"

vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

vim.opt.colorcolumn = "120"

vim.keymap.set("i", "jk", "<ESC>")

vim.keymap.set("n", "<C-k>", ":b#<Cr>")

if util.os() == "win" then
	vim.api.nvim_set_option_value("fsync", false, { scope = "global" })
end

--vim.keymap.del("n", "<C-q>")
