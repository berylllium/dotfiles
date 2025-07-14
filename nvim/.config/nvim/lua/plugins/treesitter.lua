return {
	-- Syntax highlighting, basically.
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		opts = {
			ensure_installed = {
				"cpp",
				"lua",
				"c",
				"c_sharp",
				"java",
				"make",
				"markdown_inline",
				"json",
				"bash",
				"rust",
				"latex",
				"ocaml",
				"prolog",
				"typst",
				"glsl"
			},
			sync_install = false,
			ignore_install = {"org"},
			highlight = {
				enable = true,
				disable = {""},
				additional_vim_regex_highlighting = true
			}
		},
		config = function(_, opts)
			vim.filetype.add({
				extension = {
					vert = "glsl",
					frag = "glsl"
				}
			})

			require("nvim-treesitter.configs").setup(opts)
		end
	},
}
