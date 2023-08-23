local lspconfig = require 'lspconfig'

vim.keymap.set('n', 'g?', vim.diagnostic.open_float, { noremap=true, silent=true })
vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { noremap=true, silent=true })
vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { noremap=true, silent=true })
vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, { noremap=true, silent=true })
vim.keymap.set('n', 'K', vim.lsp.buf.hover, { noremap=true, silent=true })

vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)

-- Enable manual completion.
vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
	end
})

-- Disable scratch buffer creation.
vim.cmd("set completeopt-=preview")

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
	}
}

-- Rust
lspconfig.rust_analyzer.setup {
	settings = {
		['rust-analyzer'] = {
			diagnostics = {
				enable = true
			}
		}
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
