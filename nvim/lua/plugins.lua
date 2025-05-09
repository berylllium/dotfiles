local plugins = {
	{
		"andweeb/presence.nvim",
		enabled = false,
		config = function()
			require("presence").setup {
				main_image = "file",
				show_time = false
			}
		end
	},
	{
		"ellisonleao/gruvbox.nvim",
		config = function()
			vim.o.background = "dark"
			vim.cmd([[colorscheme gruvbox]])
		end
	},
	{
		"brooth/far.vim"
	},
	{
		"stevearc/conform.nvim",
		config = function()
			local cf = require("conform")

			cf.setup {
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
			}

			vim.api.nvim_create_autocmd("BufWritePre", {
				pattern = { "*.c", "*.cpp", "*.cc", "*.h", "*.hpp", "*.rs" },
				callback = function(args)
					require("conform").format({ bufnr = args.buf })
				end,
			})
		end
	},
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			local configs = require("nvim-treesitter.configs")

			vim.filetype.add({
				extension = {
					vert = "glsl",
					frag = "glsl"
				}
			})

			configs.setup {
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
				ignore_install = {""},
				highlight = {
					enable = true,
					disable = {""},
					additional_vim_regex_highlighting = true
				}
			}
		end
	},
	{
		"chomosuke/typst-preview.nvim",
		version = "1.*",
		config = function()
			require("typst-preview").setup {}
		end
	},
	{
		"williamboman/mason.nvim",
		config = function()
			require("mason").setup()
		end
	},
	{
		"williamboman/mason-lspconfig.nvim",
		dependencies = { "williamboman/mason.nvim" },
		config = function()
			require("mason-lspconfig").setup {
				ensure_installed = { "jdtls", "omnisharp" }
			}

			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local client = vim.lsp.get_client_by_id(args.data.client_id)
					if client ~= nil and client.name == "jdtls" then
						client.server_capabilities.semanticTokensProvider = nil
					end
				end
			})
		end
	},
	{
		"mfussenegger/nvim-jdtls",
		dependencies = { "williamboman/mason-lspconfig.nvim" },
	},
	{
		"neovim/nvim-lspconfig",
		dependencies = { "hrsh7th/cmp-nvim-lsp", "williamboman/mason-lspconfig.nvim" },
		config = function()
			local lspconfig = require("lspconfig")

			vim.keymap.set("n", "g?", vim.diagnostic.open_float, { noremap=true, silent=true })
			vim.keymap.set("n", "gd", vim.lsp.buf.definition, { noremap=true, silent=true })
			vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { noremap=true, silent=true })
			vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { noremap=true, silent=true })
			vim.keymap.set("n", "K", vim.lsp.buf.hover, { noremap=true, silent=true })
			vim.keymap.set("n", "<C-.>", vim.lsp.buf.code_action, { noremap=true, silent=true })

			vim.keymap.set("n", "[d", vim.diagnostic.goto_prev)
			vim.keymap.set("n", "]d", vim.diagnostic.goto_next)

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
			lspconfig.ccls.setup {
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
			}

			-- C#
			lspconfig.omnisharp.setup {
				capabilities = cmp_caps,
			}

			-- Rust
			lspconfig.rust_analyzer.setup {
				settings = {
					["rust-analyzer"] = {
						diagnostics = {
							enable = true
						}
					}
				},
				capabilities = cmp_caps,
			}

			-- Java
--			lspconfig.jdtls.setup {
--				on_attach = function(client)
--					if client.name == "jdtls" then
--						client.server_capabilities.semanticTokensProvider = nil
--					end
--				end,
--				capabilities = cmp_caps,
--			}

			-- Lua
			lspconfig.lua_ls.setup {
				settings = {
					Lua = {
						runtime = {
							version = "LuaJIT"
						},
						diagnostics = {
							globals = {"vim", "game", "script", "commands", "helpers", "prototypes", "rcon", "remote", "rendering", "settings", "storage", "serpent", "defines", "log"}
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
			}
		end
	},
	{
		"nvim-telescope/telescope.nvim",
		version = "*",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = function()
			require("telescope").setup {
				defaults = {
					file_ignore_patterns = { "^deps/" }
				}
			}

			local builtin = require("telescope.builtin")

			vim.keymap.set("n", "<leader>ff", builtin.find_files)
			vim.keymap.set("n", "<leader>fb", builtin.buffers)
		end
	},
	{
		"akinsho/toggleterm.nvim",
		version = "*",
		config = function()
			require("toggleterm").setup()

			vim.keymap.set("n", "<C-\\>", ":ToggleTerm<CR>")
		end
	},
	{
		"berylllium/session-manager",
		version = "*",
		config = function()
			require("session-manager").setup({})
		end
	},
	{
		"vim-scripts/DoxygenToolkit.vim"
	},
	{
		"mfussenegger/nvim-dap",
		config = function()
			local dap = require("dap")

			-- Adapters.
			dap.adapters.lldb = {
				type = "executable",
				command = "/usr/bin/lldb-vscode",
				name = "lldb"
			}

			-- Configurations.
			dap.configurations.cpp = {
				{
					name = "Launch",
					type = "lldb",
					request = "launch",
					program = function()
						return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
					end,
					cwd = "${workspaceFolder}",
					stopOnEntry = false,
					args = {}
				}
			}

			dap.defaults.fallback.external_terminal = {
				command = "usr/bin/alacritty";
				args = { "-e" };
			}

			-- Keybinds.
			local dwidgets = require("dap.ui.widgets")

			vim.keymap.set("n", "<Leader>db", function() dap.toggle_breakpoint() end)
			vim.keymap.set("n", "<Leader>ddb", function() dap.clear_breakpoints() end)

			vim.keymap.set("n", "<Leader>dc", function() dap.continue() end)
			vim.keymap.set("n", "<Leader>ds", function() dap.step_into() end)
			vim.keymap.set("n", "<Leader>dn", function() dap.step_over() end)
			vim.keymap.set("n", "<Leader>du", function() dap.step_out() end)

			vim.keymap.set("n", "<Leader>dk", function() dap.terminate() end)
			vim.keymap.set("n", "<Leader>dK", function() dwidgets.hover() end)
			vim.keymap.set("n", "<Leader>dr", function() dap.run_to_cursor() end)

			-- Events.
			dap.listeners.after["event_initialized"]["me"] = function()
				dap.repl.open({ height = 10 })
			end

			dap.listeners.after["event_terminated"]["me"] = function()
				dap.repl.close()
			end
		end
	},
	{
		"theHamsta/nvim-dap-virtual-text",
		dependencies = { "mfussenegger/nvim-dap" },
		config = function()
			require("nvim-dap-virtual-text").setup({})
		end
	},
--	{
--		"kevinhwang91/nvim-ufo",
--		dependencies = { "kevinhwang91/promise-async", "nvim-treesitter/nvim-treesitter" },
--		config = function()
--			vim.o.foldcolumn = "1" -- "0" is not bad
--			vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
--			vim.o.foldlevelstart = 99
--			vim.o.foldenable = true
--
--			vim.keymap.set("n", "zR", require("ufo").openAllFolds)
--			vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
--
----			local capabilities = vim.lsp.protocol.make_client_capabilities()
----			capabilities.textDocument.foldingRange = {
----				dynamicRegistration = false,
----				lineFoldingOnly = true
----			}
----			local language_servers = require("lspconfig").util.available_servers() -- or list servers manually like {"gopls", "clangd"}
----			for _, ls in ipairs(language_servers) do
----				require("lspconfig")[ls].setup({
----					capabilities = capabilities
----					-- you can add other fields for setting up lsp server in this table
----				})
----			end
--			require("ufo").setup()
--		end
--	},
	{
		"nvim-neorg/neorg",
		build = ":Neorg sync-parsers",
		dependencies = { "nvim-treesitter/nvim-treesitter" },
		config = function()
			require("neorg").setup {
				load = {
					["core.defaults"] = {}, -- Loads default behaviour
					["core.concealer"] = {}, -- Adds pretty icons to your documents
					["core.dirman"] = { -- Manages Neorg workspaces
						config = {
							workspaces = {
								notes = "~/documents/notes",
							},
						},
					},
				}
			}
		end
	},
	{
		"NeogitOrg/neogit",
		dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
		config = function()
			local neogit = require("neogit")

			neogit.setup {}

			vim.keymap.set("n", "<Leader>gg", function() neogit.open({ kind = "auto" }) end)
		end,
	},
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/vim-vsnip",
			"hrsh7th/cmp-vsnip",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
		},
		config = function()
			local cmp = require('cmp')

			cmp.setup {
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
					['<CR>'] = cmp.mapping.confirm({ select = true }),
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp" },
					{ name = "vsnip" },
				}, {
					{ name = "buffer" },
				}),
				completion = {
					autocomplete = false,
				}
			}
		end,
	},
--	{
--		'mrcjkb/rustaceanvim',
--		version = '^4', -- Recommended
--		lazy = false, -- This plugin is already lazy
--	}
}

local opts = {}

require("lazy").setup(plugins, opts)
