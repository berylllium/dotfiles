-- All telescope related plugins.

return {
	{
		"nvim-telescope/telescope.nvim",
		version = "*",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = function()
			local actions = require("telescope.actions")

			return {
				defaults = {
					file_ignore_patterns = {
						"^deps/",
						"^target/",
						"^.git/",
					}
				},
				extensions = {
					file_browser = {
						auto_depth = true,
						mappings = {
							["i"] = {
								["<C-j>"] = actions.move_selection_next,
								["<C-k>"] = actions.move_selection_previous,
								["<C-Space>"] = actions.select_default,
							}
						},
					}
				}
			}
		end,
		config = function(_, opts)
			local telescope = require("telescope")
			telescope.setup(opts)

			telescope.load_extension("file_browser")

			-- local builtin = require("telescope.builtin")

			-- vim.keymap.set("n", "<leader>ff", telescope.extensions.file_browser.file_browser)
			-- vim.keymap.set("n", "<leader>fb", builtin.buffers)
		end,
		keys = {
			{ "<leader>ff", "<cmd>Telescope file_browser<cr>" },
			{ "<leader>fb", "<cmd>Telescope buffers<cr> "}
		}
	},
	{
		"nvim-telescope/telescope-file-browser.nvim",
		dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
	},
}
