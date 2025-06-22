-- cfg by James Scholz

return {
  {
	'nvim-telescope/telescope.nvim',
	dependencies = {
		{ 'nvim-telescope/telescope-fzf-native.nvim','BurntSushi/ripgrep', build = 'make'  }
	},--'nvim-telescope/telescope-fzf-native.nvim', 
	config = function(lazy, opts) 
		local telescope = require('telescope')
		require('telescope').load_extension('fzf')
		telescope.setup({
			defaults = {
				wrap_result = true,
				mappings = {
					i = {
						["<esc>"] = require("telescope.actions").close,
						-- search history
						["<C-Down>"] = require('telescope.actions').cycle_history_next,
						["<C-Up>"] = require('telescope.actions').cycle_history_prev,
					},
				},
				layout_strategy = "vertical",
				layout_config = {
					vertical = {
						width = 0.9,
						preview_cutoff = 10,
					}
				}
			},
			pickers = {
				-- note: remove the 'builtin.' prefix.
				["lsp_references"] = { wrap_results = true, },
				["lsp_definitions"] = { wrap_results = true, },
				["diagnostics"] = { wrap_results = true, },
				["find_files"] = { wrap_results = true, },
				["buffers"] = { sort_mru = true, ignore_current_buffer = true },
			}
		})
	end,

	keys = {
		-- See :help telescope.builtin
		{ '<leader>fo', function()
			require("telescope.builtin").oldfiles {
				prompt_title = 'Recent files',
				sort_mru= true
			} end,
			desc = "Old (recent) files"},
			{'<leader><space>', '<cmd>Telescope buffers<cr>', desc = "Buffers"},

			{'<leader>ff', '<cmd>Telescope find_files<cr>', desc = "Find filenames"},
			{'<leader>fm', '<cmd>Telescope marks<cr>', desc = "Marks"},
		    {'<leader>fw', '<cmd>Telescope live_grep<cr>', desc = "Grep files"},
			}
		}
}
