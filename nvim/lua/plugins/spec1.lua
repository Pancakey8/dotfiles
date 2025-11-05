return {
    {
        "rebelot/kanagawa.nvim",
        config = function()
            require('kanagawa').setup({
                colors = {
                    palette = {
                        dragonBlack3 = "#212121",
                        dragonBlack4 = "#2A2A2A"
                    },
                    theme = {
                        dragon = {
                            ui = {
                                bg_m3 = "#1C1C1C"
                            }
                        }
                    }
                }
            })
            vim.cmd("colorscheme kanagawa-dragon")
        end
    },
    {
        "nvim-treesitter/nvim-treesitter", 
        lazy = false,
        build = ":TSUpdate",
        opts = {
            ensure_installed = "all",
            auto_install = true,
            highlight = { enable = true },
            indent = { enable = true },
        },
    },
    {
        "neovim/nvim-lspconfig",
        -- event = { "BufReadPost", "BufWritePost", "BufNewFile" },
        config = function()
            local capabilities = require("cmp_nvim_lsp").default_capabilities()
            vim.lsp.enable("clangd", { capabilities = capabilities })
            -- vim.lsp.config("clangd", { cmd = { "clangd", "--tweaks=-fno-modules-ts", "--tweaks=-fno-module-mapper", "--tweaks=-fno-deps-format=p1689r5" } })
            vim.lsp.enable("cmake", { capabilities = capabilities })
            vim.lsp.enable("zls", { capabilities = capabilities })
            vim.lsp.enable("serve_d", { capabilities = capabilities })
            vim.lsp.config("serve_d", {
                cmd = { "serve-d", "--loglevel", "all",  },
                settings = {
                    d = {
                        scanAllFolders = true,
                    }
                }
            })

            vim.api.nvim_create_autocmd('LspAttach', {
                group = vim.api.nvim_create_augroup('UserLspConfig', {}),
                callback = function(ev)
                    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

                    local opts = { buffer = ev.buf }
                    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
                    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
                    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
                    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
                    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
                    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
                    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
                    vim.keymap.set('n', '<space>wl', function()
                        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
                    end, opts)
                    -- vim.keymap.set('i', '<C-Space>', '<C-x><C-o>', { buffer = ev.buf, noremap = true, silent = true })
                    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
                    vim.keymap.set('n', '<space>d', vim.diagnostic.open_float, opts)
                    vim.keymap.set('n', '<space>nd', vim.diagnostic.goto_next, opts)
                    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
                    vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
                    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
                    vim.keymap.set('n', '<space>f', function()
                        vim.lsp.buf.format { async = true }
                    end, opts)
                end,
            })
        end,
    },
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
        },
        config = function()
            local cmp = require("cmp")
            cmp.setup({
                sources = {
                    { name = "nvim_lsp" }
                },
                mapping = cmp.mapping.preset.insert({
                    -- Navigate between completion items
                    ['<C-p>'] = cmp.mapping.select_prev_item({behavior = 'select'}),
                    ['<C-n>'] = cmp.mapping.select_next_item({behavior = 'select'}),

                    -- `Enter` key to confirm completion
                    ['<CR>'] = cmp.mapping.confirm({select = false}),

                    -- Ctrl+Space to trigger completion menu
                    ['<C-Space>'] = cmp.mapping.complete(),

                    -- Scroll up and down in the completion documentation
                    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
                    ['<C-d>'] = cmp.mapping.scroll_docs(4),
                }),
                snippet = {
                    expand = function(args)
                        vim.snippet.expand(args.body)
                    end,
                }
            })
        end,
    },
    {
        "iamcco/markdown-preview.nvim",
        cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
        ft = { "markdown" },
        build = function() vim.fn["mkdp#util#install"]() end,
    },
    {
        {
            "nvim-lualine/lualine.nvim",
            dependencies = { "nvim-tree/nvim-web-devicons" },
            opts = {},
            config = function()
                require("lualine").setup({
                    options = {
                        icons_enabled = true,
                        theme = "auto",
                        always_last_status = false,
                    },
                    sections = {
                        lualine_a = { "mode" },
                        lualine_b = { "branch", "filename" },
                        lualine_c = { lsp_clients_status },
                        lualine_z = { "location" },
                    },
                })
            end
        }
    }
}
