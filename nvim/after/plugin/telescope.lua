local builtin = require('telescope.builtin')

vim.keymap.set('n', '<C-p>', builtin.git_files, { desc = 'Find files in git' })
vim.keymap.set('n', '<leader><leader>', builtin.find_files, { desc = 'Find files' })
vim.keymap.set('n', '<leader>bb', builtin.buffers, { desc = 'Find buffers' })
vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Find files' })
vim.keymap.set('n', '<leader>sb', builtin.buffers, { desc = 'Find buffers' })
vim.keymap.set('n', '<leader>sf', builtin.find_files, { desc = 'Find files' })
vim.keymap.set('n', '<leader>ss', builtin.live_grep, { desc = 'Live search' })
vim.keymap.set('n', '<leader>pv', '<cmd>Telescope file_browser path=%:p:h select_buffer=true<CR>',
    { desc = 'Directory listing' })
vim.keymap.set('n', '<leader>pp', '<cmd>Telescope project<CR>', { desc = 'Projects' })
