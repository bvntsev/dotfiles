--make leader space
vim.g.mapleader = " "

--copy to system clipboard
-- vim.keymap.set("n", "<leader>y", '"+y')
-- vim.keymap.set("v", "<leader>y", '"+y')
-- vim.keymap.set("n", "<leader>y", '"+Y')

--when using these binds you can move a chunk of text and it will automatically indent
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

--tabs in visual mode
vim.keymap.set("v", "<S-Tab>", "<<")
vim.keymap.set("v", "<Tab>", ">>")

-- nvim-tree binds 
vim.api.nvim_set_keymap('n', '<leader>e', ':NvimTreeToggle<CR>', { noremap = true, silent = true })

vim.api.nvim_set_keymap('n', '<C-h>', '<C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>k', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-l>', '<C-w>l', { noremap = true, silent = true })


--inverse tabs
vim.keymap.set("i", "<S-Tab>", "<C-d>")

--change buffer
vim.keymap.set("n", "<leader>t", ":enew<CR>")
vim.keymap.set("n", "<leader>d", ":bd<CR>")
vim.keymap.set("n", "<leader><Tab>", ":bnext<CR>")
vim.keymap.set("n", "<leader><S-Tab>", ":bprev<CR>")

--use d without copy
-- vim.keymap.set("n", "d", '"_d')
-- vim.keymap.set("v", "d", '"_d')

--allows ci without removing current paste buffer
-- vim.keymap.set("n", "ci(", '"_ci(')
-- vim.keymap.set("n", "ci)", '"_ci)')
-- vim.keymap.set("n", "ci{", '"_ci{')
-- vim.keymap.set("n", "ci}", '"_ci}')
-- vim.keymap.set("n", "ci'", "\"_ci'")
-- vim.keymap.set("n", "ci[", '"_ci[')
-- vim.keymap.set("n", "ci]", '"_ci]')
-- vim.keymap.set("n", 'ci"', '"_ci"')

