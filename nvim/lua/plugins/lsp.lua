return {
	{
		"neovim/nvim-lspconfig",
		dependencies = { "hrsh7th/cmp-nvim-lsp", "williamboman/mason.nvim" },
		keys = {
			{ "g?", vim.diagnostic.open_float, { noremap=true, silent=true } },
			{ "gd", vim.lsp.buf.definition, { noremap=true, silent=true } },
			{ "gD", vim.lsp.buf.declaration, { noremap=true, silent=true } },
			{ "gi", vim.lsp.buf.implementation, { noremap=true, silent=true } },
			{ "K", vim.lsp.buf.hover, { noremap=true, silent=true } },
			{ "<C-.>", vim.lsp.buf.code_action, { noremap=true, silent=true } },
		},
		config = function()
			-- Enable manual completion.
			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("UserLspConfig", {}),
				callback = function(ev)
					vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
				end
			})

			-- Disable scratch buffer creation.
			vim.cmd("set completeopt-=preview")

			local cmp_caps = require('cmp_nvim_lsp').default_capabilities()

			-- C/C++
			vim.lsp.config("ccls", {
				init_options = {
					compilationDatabaseDirectory = "build/";
					index = {
						threads = 0;
					};
					clang = {
				  		excludeArgs = { "-frounding-math" };
					};
				},
				capabilities = cmp_caps,
			})

			-- C#
			vim.lsp.config("omnisharp", {
				capabilities = cmp_caps,
			})

			-- Rust
			vim.lsp.config("rust_analyzer", {
				settings = {
					["rust-analyzer"] = {
						diagnostics = {
							enable = true
						}
					}
				},
				capabilities = cmp_caps,
			})

			-- Lua
			vim.lsp.config("lua_ls", {
				settings = {
					Lua = {
						runtime = {
							version = "LuaJIT"
						},
						diagnostics = {
							globals = {}
						},
						workspace = {
							library = vim.api.nvim_get_runtime_file("", true),
							checkThirdParty = false
						},
						telemetry = {
							enable = false
						}
					}
				},
				capabilities = cmp_caps,
			})
		end
	},
	{
    	"mason-org/mason-lspconfig.nvim",
		dependencies = { "mason-org/mason.nvim", "neovim/nvim-lspconfig" },
		opts = {
			ensure_installed = { "omnisharp", "lua_ls" },
			automatic_enable = true,
		},
	},
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/vim-vsnip",
			"hrsh7th/cmp-vsnip",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
		},
		opts = function()
			local cmp = require('cmp')

			return {
				snipped = {
					expand = function(args)
						vim.fn["vsnip#anonymous"](args.body)
					end,
				},
				window = {
					completion = cmp.config.window.bordered({
						border = "none",
					}),
					documentation = cmp.config.window.bordered(),
				},
				mapping = cmp.mapping.preset.insert({
					['<C-k>'] = cmp.mapping.select_prev_item(),
					['<C-j>'] = cmp.mapping.select_next_item(),
					['<C-S-k>'] = cmp.mapping.scroll_docs(-4),
					['<C-S-j>'] = cmp.mapping.scroll_docs(4),
					['<C-Space>'] = cmp.mapping.complete(),
					['<C-q>'] = cmp.mapping.abort(),
					['<Tab>'] = cmp.mapping.confirm({ select = true }),
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp" },
					{ name = "vsnip" },
				}, {
					{ name = "buffer" },
				}),
				completion = {
					autocomplete = false,
				},
				formatting = {
					format = function(_, vim_item)
						-- Completion window max width of 40.
						vim_item.abbr = string.sub(vim_item.abbr, 1, 40)

						return vim_item
					end,
				},
			}
		end,
	},
}
