vim.opt.tabstop = 4
vim.opt.shiftwidth=4
vim.opt.expandtab = true
vim.opt.relativenumber = true

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.o.foldmethod = "syntax"
vim.o.foldenable = false

require("config.lazy")
