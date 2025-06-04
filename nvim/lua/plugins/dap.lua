return {
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
		opts = {},
	},
}
