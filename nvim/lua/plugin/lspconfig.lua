local lspconfig = require 'lspconfig'

vim.keymap.set('n', 'g?', vim.diagnostic.open_float, { noremap=true, silent=true })
vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { noremap=true, silent=true })
vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { noremap=true, silent=true })
vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, { noremap=true, silent=true })
vim.keymap.set('n', 'K', vim.lsp.buf.hover, { noremap=true, silent=true })

-- C/C++
lspconfig.ccls.setup {
	init_options = {
		compilationDatabaseDirectory = "build";
		index = {
			threads = 0;
		};
		clang = {
      		excludeArgs = { "-frounding-math" } ;
		};
	}
}

-- Lua
lspconfig.lua_ls.setup {
	settings = {
		Lua = {
			runtime = {
				version = 'LuaJIT'
			},
			diagnostics = {
				globals = {'vim'}
			},
			workspace = {
				library = vim.api.nvim_get_runtime_file("", true),
				checkThirdParty = false
			},
			telemetry = {
				enable = false
			}
		}
	}
}
