local util = require("util")

return {
	{
		"andweeb/presence.nvim",
		enabled = false,
		opts = {
			main_image = "file",
			show_time = false
		},
	},
	{
		"akinsho/toggleterm.nvim",
		version = "*",
		keys = {
			{"<C-\\>", "<cmd>ToggleTerm<cr>"},
		},
		opts = {},
		config = function(_, opts)
			if util.os() == "win" then
				local powershell_options = {
				  shell = "powershell",
				  shellcmdflag = "-NoLogo -NoProfile -ExecutionPolicy RemoteSigned -Command [Console]::InputEncoding=[Console]::OutputEncoding=[System.Text.Encoding]::UTF8;",
				  shellredir = "-RedirectStandardOutput %s -NoNewWindow -Wait",
				  shellpipe = "2>&1 | Out-File -Encoding UTF8 %s; exit $LastExitCode",
				  shellquote = "",
				  shellxquote = "",
				}

				for option, value in pairs(powershell_options) do
				  vim.opt[option] = value
				end
			end

			require("toggleterm").setup(opts)
		end
	},
	{
		"berylllium/session-manager",
		version = "*",
		opts = {},
	},
	{
		"vim-scripts/DoxygenToolkit.vim"
	},
}
