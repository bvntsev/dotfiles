-- Minimal config based on doomguy69
vim.g.mapleader = " "
vim.g.termguicolors = true

vim.opt.clipboard = 'unnamedplus' -- buffer

vim.o.nu = true
vim.o.relativenumber = true
vim.o.cursorline = true
vim.o.expandtab = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.hlsearch = false
vim.o.incsearch = true
vim.o.swapfile = false
vim.o.backup = false
vim.o.wrap = false
vim.o.undofile = true
vim.o.undodir = os.getenv("HOME") .. "/.cache/nvim/undodir"

vim.cmd("set colorcolumn=80")
vim.cmd("hi ColorColumn guibg='Gray'")

vim.o.list = true
-- vim.o.listchars = "tabs:> "
vim.o.path = "**"

-- vim.cmd(":colorscheme retrobox")
-- vim.cmd(":colorscheme habamax")
vim.cmd(":colorscheme industry")

vim.cmd(":command! -nargs=+ Grep execute 'silent grep! <args>' | copen")
vim.o.grepprg = "grep -RInH --exclude-dir=.git --color=never"

vim.treesitter.language.register("cpp", "c", "python")
vim.api.nvim_create_autocmd("FileType", {
    callback = function()
        pcall(vim.treesitter.start)
    end,
    -- pattern = {
    --     "c", "h", "cpp", "cc", "hpp",
    --     "lua"
    -- },
    -- callback = function() vim.treesitter.start() end,
})

local function pick_buffer()
    -- Get all listed buffers
    local buffers = vim.fn.getbufinfo({buflisted = 1})
    if #buffers == 0 then
        print("No open buffers")
        return
    end

    -- Build display list: "1: filename"
    local names = {}
    for _, buf in ipairs(buffers) do
        local name = buf.name ~= "" and vim.fn.fnamemodify(buf.name, ":t") or "[No Name]"
        table.insert(names, string.format("%d: %s", buf.bufnr, name))
    end

    -- Show the select menu
    vim.ui.select(names, {prompt = "Select buffer:"}, function(choice)
        if not choice then return end

        -- Extract buffer number from selection
        local bufnr = tonumber(choice:match("^(%d+):"))
        if bufnr and vim.api.nvim_buf_is_valid(bufnr) then
            vim.api.nvim_set_current_buf(bufnr)
        end
    end)
end

local map = vim.keymap.set
-- map("n", "<C-h>", "<C-w><C-h>")
-- map("n", "<C-j>", "<C-w><C-j>")
-- map("n", "<C-k>", "<C-w><C-k>")
-- map("n", "<C-l>", "<C-w><C-l>")

-- map("n", "<C-d>", "<C-d>zz")
-- map("n", "<C-u>", "<C-u>zz")

map("i", "<C-n>", "<C-x><C-]>")
map("i", "<C-Space>", "<C-x><C-o>")

map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")
map("n", ";", "q:")
map("t", "<ESC>", "<C-\\><C-n>")
-- map("n", "<C-h>", ":below term<CR>i")
map("n", "<C-\\>", ":rightbelow vsplit | terminal<CR>i")

-- map("n", "<leader>w", ":write<CR>")
-- map("n", "<leader>q", ":quit<CR>")
-- map("n", "<leader>Q", ":quit!<CR>")
map("n", "<leader>y", "\"+y")
map("n", "<leader>e", ":Ex<CR>")
map("n", "<leader>fc", ":edit $MYVIMRC<CR>")
map("n", "<leader>ff", ":find ")
map("n", "<leader>fb", pick_buffer, { desc = "Pick buffer" })
map("n", "<leader>fg", ":Grep ")
-- map("n", "<leader>r", ":make!<CR>")
vim.keymap.set("n", "<leader>r", ":botright split | terminal make -k<CR>")
map("n", "<leader>R", ":set makeprg=")
-- map("n", "<leader>x", ":copen<CR>")
