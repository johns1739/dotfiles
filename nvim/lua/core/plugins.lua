-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim' -- Packer can manage itself

    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        config = function()
            require 'nvim-treesitter.configs'.setup {
                ensure_installed = {
                    "c",
                    "lua",
                    "vim",
                    "vimdoc",
                    "query",
                },
                sync_install = false,
                auto_install = true,
                highlight = {
                    enable = true,
                    additional_vim_regex_highlighting = false,
                },
            }
        end
    }

    use {
        'tpope/vim-fugitive',
        config = function()
            vim.keymap.set('n', '<leader>gg', vim.cmd.Git, { desc = 'Git status' })
        end
    }

    use {
        'mbbill/undotree',
        config = function()
            vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
        end
    }

    use {
        'nvim-tree/nvim-tree.lua',
        config = function()
            vim.g.loaded_netrw = 1
            vim.g.loaded_netrwPlugin = 1
            vim.opt.termguicolors = true
            require("nvim-tree").setup({
                sort_by = "case_sensitive",
                renderer = {
                    group_empty = true,
                },
                filters = {
                    dotfiles = true,
                },
            })
            vim.keymap.set('n', '<leader>op', '<cmd>NvimTreeFindFileToggle<CR>', { desc = 'Toggle File Explorer' })
        end
    }

    use {
        'lewis6991/gitsigns.nvim',
        config = function()
            require('gitsigns').setup {
                on_attach = function(bufnr)
                    local gs = package.loaded.gitsigns
                    vim.keymap.set('n', '<leader>gB', function() gs.blame_line { full = true } end)
                end
            }
        end,
    }

    use {
        "kylechui/nvim-surround",
        tag = "*",
        config = function()
            require("nvim-surround").setup({})
        end
    }

    use { "catppuccin/nvim",
        disable = true,
        as = "catppuccin",
        config = function()
            vim.cmd.colorscheme "catppuccin"
        end
    }

    use({
        'rose-pine/neovim',
        as = 'rose-pine',
        config = function()
            vim.cmd('colorscheme rose-pine')
        end
    })

    use {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v2.x',
        requires = {
            -- LSP Support
            { 'neovim/nvim-lspconfig' }, -- Required
            {
                -- Optional
                'williamboman/mason.nvim',
                run = function()
                    pcall(vim.cmd, 'MasonUpdate')
                end,
            },
            { 'williamboman/mason-lspconfig.nvim' }, -- Optional

            -- Autocompletion
            { 'hrsh7th/nvim-cmp' },     -- Required
            { 'hrsh7th/cmp-nvim-lsp' }, -- Required
            { 'L3MON4D3/LuaSnip' },     -- Required
        },
        config = function()
            local lsp = require('lsp-zero').preset({})
            lsp.on_attach(function(client, bufnr)
                lsp.default_keymaps({ buffer = bufnr })
            end)
            require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())
            lsp.setup()
            vim.diagnostic.disable() -- TODO: Toggle
        end
    }

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.x',
        requires = { { 'nvim-lua/plenary.nvim' } },
        config = function()
            local builtin = require('telescope.builtin')
            vim.keymap.set('n', '<C-p>', builtin.git_files, { desc = 'Find files in git' })
            vim.keymap.set('n', '<leader><leader>', builtin.find_files, { desc = 'Find files' })
            vim.keymap.set('n', '<leader>bb', builtin.buffers, { desc = 'Find buffers' })
            vim.keymap.set('n', '<leader>ss', builtin.live_grep, { desc = 'Live search' })
            vim.keymap.set('n', '<leader>ff', '<cmd>Telescope file_browser path=%:p:h select_buffer=true<CR>',
                { desc = 'Directory listing' })
            vim.keymap.set('n', '<leader>pp', '<cmd>Telescope project<CR>', { desc = 'Projects' })
        end
    }

    use {
        "nvim-telescope/telescope-file-browser.nvim",
        requires = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
        config = function()
            require("telescope").load_extension "file_browser"
        end
    }

    use {
        "nvim-telescope/telescope-project.nvim",
        requires = { "nvim-telescope/telescope.nvim" },
        config = function()
            require 'telescope'.load_extension('project')
        end
    }
end)
