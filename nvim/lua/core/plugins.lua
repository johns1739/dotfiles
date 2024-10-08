-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim' -- Packer can manage itself
    use 'mbbill/undotree'

    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }

    use {
        'nvim-lualine/lualine.nvim',
        requires = { 'nvim-tree/nvim-web-devicons', opt = true },
        config = function()
            require('lualine').setup {
                sections = {
                    lualine_c = {
                        require('auto-session.lib').current_session_name,
                        {
                            'filename',
                            path = 1
                        },
                    }
                }

            }
        end
    }

    use 'tpope/vim-fugitive'

    use {
        'ruifm/gitlinker.nvim',
        requires = 'nvim-lua/plenary.nvim',
        config = function()
            require 'gitlinker'.setup()
        end
    }

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
        'nvim-tree/nvim-tree.lua',
        config = function()
            -- vim.g.loaded_netrw = 1
            -- vim.g.loaded_netrwPlugin = 1

            vim.opt.termguicolors = true
            require("nvim-tree").setup {
                disable_netrw = false,
                hijack_netrw = true,
                reload_on_bufenter = true,
                sync_root_with_cwd = true,
                view = {
                    width = 40
                },
                renderer = {
                    group_empty = true,
                },
                filters = {
                    dotfiles = true,
                },
            }
        end
    }

    use {
        'lewis6991/gitsigns.nvim',
        config = function()
            require('gitsigns').setup {
                current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
                current_line_blame_opts = {
                    delay = 2000,
                },
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
                    pcall(vim.api.nvim_command, 'MasonUpdate')
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
            lsp.on_attach(function(_, bufnr)
                lsp.default_keymaps({ buffer = bufnr })
            end)
            require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())
            lsp.setup()
        end
    }

    use {
        'nvim-telescope/telescope.nvim',
        branch = '0.1.x',
        requires = { { 'nvim-lua/plenary.nvim' } },
        config = function()
            local actions = require('telescope.actions')
            require('telescope').setup({
                defaults = {
                    layout_strategy = "vertical",
                    vimgrep_arguments = {
                        "rg",
                        "--color=never",
                        "--no-heading",
                        "--with-filename",
                        "--line-number",
                        "--column",
                        "--smart-case",
                        -- "--hidden"
                    },
                    mappings = {
                        i = {
                            ['<C-j>'] = actions.cycle_history_next,
                            ['<C-k>'] = actions.cycle_history_prev
                        }
                    }
                },
                extensions = {
                    project = {
                        order_by = "recent", -- already the default
                        sync_with_nvim_tree = true,
                    }
                }
            })
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

    use {
        'rmagatti/auto-session',
        requires = { 'nvim-telescope/telescope.nvim' },
        config = function()
            require("auto-session").setup {
                log_level = "error",
                auto_save_enabled = true,
                auto_session_suppress_dirs = { "~/", "~/workspace", "~/Downloads", "/" },
                session_lens = {
                    load_on_setup = true,
                    path_display = { 'shorten' },
                }
            }
            require("telescope").load_extension "session-lens"
        end
    }

    use {
        "folke/which-key.nvim",
        config = function()
            vim.o.timeout = true
            vim.o.timeoutlen = 500
            local wk = require("which-key")
            wk.register(
                {
                    c = { name = 'Code' },
                    f = {
                        name = 'Find',
                        g = { name = 'Git' },
                    },
                    g = {
                        name = 'Git',
                        y = 'Git link',
                    },
                    o = { name = 'Open' },
                    p = { name = 'Project' },
                    t = { name = 'Toggle' },
                },
                { prefix = '<leader>' }
            )
        end
    }
end)
