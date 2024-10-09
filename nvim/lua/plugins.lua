local util = require("util")

local plugins = {
	{
		"andweeb/presence.nvim",
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
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			local configs = require("nvim-treesitter.configs")

			configs.setup {
				ensure_installed = {
					"cpp",
					"lua",
					"c",
					"make",
					"markdown_inline",
					"json",
					"bash",
					"rust",
					"latex"
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
		"neovim/nvim-lspconfig",
		dependencies = { "hrsh7th/cmp-nvim-lsp" },
		config = function()
			local lspconfig = require("lspconfig")

			vim.keymap.set("n", "g?", vim.diagnostic.open_float, { noremap=true, silent=true })
			vim.keymap.set("n", "gd", vim.lsp.buf.definition, { noremap=true, silent=true })
			vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { noremap=true, silent=true })
			vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { noremap=true, silent=true })
			vim.keymap.set("n", "K", vim.lsp.buf.hover, { noremap=true, silent=true })

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

			-- Rust
			--lspconfig.rust_analyzer.setup {
			--	settings = {
			--		["rust-analyzer"] = {
			--			diagnostics = {
			--				enable = true
			--			}
			--		}
			--	},
			--	capabilities = cmp_caps,
			--}

			-- Lua
			lspconfig.lua_ls.setup {
				settings = {
					Lua = {
						runtime = {
							version = "LuaJIT"
						},
						diagnostics = {
							globals = {"vim"}
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
					file_ignore_patterns = {
						"^deps/",
						"^target/",
						"^.git/",
					}
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
	{
		'mrcjkb/rustaceanvim',
		version = '^4', -- Recommended
		lazy = false, -- This plugin is already lazy
	}
}

local opts = {}

require("lazy").setup(plugins, opts)
