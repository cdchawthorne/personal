--------------
-- Settings --
--------------

vim.opt.backup = true
vim.opt.backupdir = vim.fn.stdpath("data") .. "/backup"
vim.opt.breakat = " "
vim.opt.colorcolumn = "+1"
-- vim.opt.completeopt:append { "longest" }
vim.opt.diffopt:append { "iwhite", "foldcolumn:0", "context:1000000", "vertical" }
vim.opt.expandtab = true
vim.opt.foldenable = false
vim.opt.guicursor = { "n-v-c-sm:block", "i-ci-ve-r-cr-o:hor20", "a:blinkon500-blinkoff500" }
vim.opt.hidden = true
vim.opt.ignorecase = true
vim.opt.inccommand = "split"
vim.opt.laststatus = 3
vim.opt.matchtime = 1
vim.opt.mouse = ""
vim.opt.modeline = false
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrollback = 100000
vim.opt.shiftwidth = 2
vim.opt.showcmd = true
vim.opt.showmatch = true
vim.opt.smartcase = true
vim.opt.softtabstop = 2
vim.opt.spelllang = "en_ca"
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.tabstop = 2
vim.opt.tags = vim.fn.stdpath("data") .. "/tags"
vim.opt.tildeop = true
vim.opt.timeoutlen = 1000
vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath("data") .. "/undo"
vim.opt.wildmode = { "longest", "list", "full" }

vim.cmd.language("en_US.utf8")

--------------
-- autocmds --
--------------

local toolbar_colours = {
  -- insert = { fg = '#000000', bg = '#73472d' },
  -- insert = { fg = '#000000', bg = '#d45440' },
  insert = { fg = '#000000', bg = '#ac6abf' },
  terminal = { fg = '#000000', bg = '#1a9c4a' },
  normal = { fg = '#9e9e9e', bg = '#262626' },
}
vim.api.nvim_create_autocmd({ "InsertEnter", "InsertLeave", "TermEnter", "TermLeave" }, {
  group = vim.api.nvim_create_augroup("insert_toolbar", {}),
  callback = function(ev)
    local colours
    if ev.event == "InsertEnter" then
      colours = toolbar_colours.insert
    elseif ev.event == "TermEnter" then
      colours = toolbar_colours.terminal
    else
      colours = toolbar_colours.normal
    end
    vim.api.nvim_set_hl(0, 'Tbufferline', { fg = colours.fg, bg = colours.bg, bold = true })
    vim.api.nvim_set_hl(0, 'TbufferlineNC', { fg = colours.fg, bg = colours.bg })
  end
})

local terminal_augroup = vim.api.nvim_create_augroup('terminal', {})
vim.api.nvim_create_autocmd("TermOpen", {
  group = terminal_augroup,
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
})
-- Complement the builtin automcd in $VIMRUNTIME/lua/vim/_defaults.lua
vim.api.nvim_create_autocmd('TermClose' , {
  group = terminal_augroup,
  nested = true,
  desc = 'Automatically close terminal buffers when started with no arguments and exiting with an error',
  callback = function(args)
    if vim.v.event.status == 0 then
      return
    end
    local info = vim.api.nvim_get_chan_info(vim.bo[args.buf].channel)
    local argv = info.argv or {}
    if table.concat(argv, ' ') == vim.o.shell then
      vim.api.nvim_buf_delete(args.buf, { force = true })
    end
  end,
})

vim.api.nvim_exec2([[
  augroup restore_cursor_pos
    autocmd!

    autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$")
        \|  execute "normal! g`\""
        \|endif
  augroup END
]], {})

--------------
-- Commands --
--------------

vim.api.nvim_create_user_command(
  'DiffOrig',
  'vertical new | set buftype=nofile | read # | 0delete_ | diffthis | wincmd p | diffthis',
  {}
)

----------------
-- Submodules --
----------------

require("config.keymaps")
require("config.lsp")
require("config.lazy")
