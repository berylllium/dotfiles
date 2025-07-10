return {
	-- For commenting code using keybinds.
	{
		"numToStr/Comment.nvim",
		opts = {}
	},
	-- Main formatting plugin.
	{
		"stevearc/conform.nvim",
		opts = {
			formatters_by_ft = {
				c = { "clang_format" },
				cpp = { "clang_format" },
				rust = { "rustfmt" },
			},
			formatters = {
				clang_format = {
					prepend_args = { "--style=file", "--fallback-style=LLVM" }
				}
			}
		},
		config = function(_, opts)
			require("conform").setup(opts)

			-- Run format just before saving a file to disk.
			vim.api.nvim_create_autocmd("BufWritePre", {
				pattern = { "*.c", "*.cpp", "*.cc", "*.h", "*.hpp", "*.rs" },
				callback = function(args)
					require("conform").format({ bufnr = args.buf })
				end,
			})
		end
	},
	-- Tabbing out of quotes, braces, etc.
	{
		"abecodes/tabout.nvim",
		lazy = false,
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"hrsh7th/nvim-cmp"
		},
		opts = {}
	},
	-- Automatically close quotes, braces, etc.
	{
		"m4xshen/autoclose.nvim",
		opts = {},
	}
}
