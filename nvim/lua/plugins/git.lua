return {
	{
		"NeogitOrg/neogit",
		dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
		keys = {
			{ "<leader>gg", function() require("neogit").open({ kind = "auto" }) end },
		},
		opts = {},
	},
}
