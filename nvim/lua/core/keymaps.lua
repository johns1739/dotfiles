vim.g.mapleader = ' '

vim.keymap.set('n', '<C-d>', '<C-d>zz', { desc = 'Page down' })
vim.keymap.set('n', '<C-j>', '<cmd>cprev<CR>zz', { desc = 'Prev compilation error' })
vim.keymap.set('n', '<C-k>', '<cmd>cnext<CR>zz', { desc = 'Next compilation error' })
vim.keymap.set('n', '<C-u>', '<C-u>zz', { desc = 'Page up' })
vim.keymap.set('n', '<leader>`', '<cmd>:b#<CR>', { desc = 'Switch to alt buffer' })
vim.keymap.set('n', '<leader>bd', '<cmd>b#<CR><cmd>bd#<CR>', { desc = 'Delete buffer' })
vim.keymap.set('n', '<leader>bt', [[:%s/\s\+$//e<cr>]], { desc = 'Trim trailing white space' })
vim.keymap.set('n', '<leader>cf', vim.lsp.buf.format, { desc = 'format buffer' })
vim.keymap.set('n', '<leader>j', '<cmd>lprev<CR>zz')
vim.keymap.set('n', '<leader>k', '<cmd>lnext<CR>zz')
vim.keymap.set({ 'i', 'c' }, '<C-a>', '<Home>', { desc = 'Beginning of line' })
vim.keymap.set({ 'i', 'c' }, '<C-e>', '<End>', { desc = 'End of line' })
