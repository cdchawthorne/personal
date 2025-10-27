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

vim.diagnostic.config {
  severity_sort = true,
  virtual_text = { severity = { min = vim.diagnostic.severity.INFO }},
  signs = { severity = { min = vim.diagnostic.severity.INFO }},
}

--------------
-- autocmds --
--------------

local skeleton_au_group = vim.api.nvim_create_augroup("skeleton_files", {})
vim.api.nvim_create_autocmd("BufNewFile", {
  pattern = "*.tex",
  group = skeleton_au_group,
  command = "0read " .. vim.fn.stdpath("config") .. "/skeleton.tex | normal! Gdd6k$",
})
vim.api.nvim_create_autocmd("BufNewFile", {
  pattern = "*.js",
  group = skeleton_au_group,
  command = "0read " .. vim.fn.stdpath("config") .. "/skeleton.js | normal! G",
})


vim.api.nvim_create_autocmd("TermOpen", {
  group = vim.api.nvim_create_augroup("terminal", {}),
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
})
-- Complement the builtin automcd in $VIMRUNTIME/lua/vim/_defaults.lua
vim.api.nvim_create_autocmd({ 'TermClose' }, {
  group = nvim_terminal_augroup,
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

-- disable lsp-based syntax highlighting; see
-- https://www.reddit.com/r/neovim/comments/109vgtl/comment/j40jzjd/ and
-- https://github.com/simrat39/rust-tools.nvim/issues/365#issuecomment-1506286437
-- (and :h lspconfig-setup and :h LspAttach)
vim.api.nvim_create_autocmd("CompleteDone", { command = "pclose" })

-----------------------
-- Utility functions --
-----------------------

local function get_buf_cwd()
  if vim.b.terminal_job_pid == nil then
    return vim.fn.expand("%:p:h")
  else
    -- Doing this using nvim's system() since apparently lua only lets you execute
    -- external processes as strings and I really don't want to worry about escaping.
    local dir = vim.fn.system({ "realpath", "/proc/" .. vim.b.terminal_job_pid .. "/cwd" })
    return dir:sub(1, -2)
  end
end

local function spawn_shell(layout_callback)
  local cwd = get_buf_cwd()
  layout_callback()
  vim.fn.termopen({ os.getenv("SHELL") }, { cwd = cwd })
  vim.cmd.startinsert()
end

--------------
-- Mappings --
--------------

local map = vim.keymap.set

local basic_modes = {'n', 'v', 's', 'o'}
local silent = { silent = true }

map(basic_modes, '/', [[/\v]])
map(basic_modes, '?', [[?\v]])
map(basic_modes, '<Tab>', '%')
map(basic_modes, 'v', '<C-b>')
map(basic_modes, 'm', '<C-f>')

map('n', 'S', 'm')
map('n', 'x', ':write<CR>', silent)
map('n', '<Up>', '<Cmd>.m-2<CR>', silent)
map('n', '<Down>', '<Cmd>.m+1<CR>', silent)

map('n', '}f', '<Cmd>cn<CR>', silent)
map('n', '{f', '<Cmd>cp<CR>', silent)

map({'v', 's'}, '<', '<gv')
map({'v', 's'}, '>', '>gv')

local swaps = {
  ['0'] = ')',
  ['9'] = '(',
  ['8'] = '*',
  ['7'] = '&',
  ['6'] = '^',
  ['['] = '{',
  [']'] = '}',
}
for i, v in pairs(swaps) do
  map(basic_modes, i, v)
  map(basic_modes, v, i)
end

map('t', ',.', [[<C-\><C-n>]])

for _, fst in ipairs({ 'f', 'F' }) do
  for _, snd in ipairs({ 'j', 'J' }) do
    map('i', fst .. snd, '<C-]><Esc>')
  end
end

map('i', '<C-u>', '<C-g>u<C-u>')
map('i', 'fdn', '<C-n>')
map('i', 'fdp', '<C-p>')
map('i', 'fdh', '<C-]><Cmd>nohlsearch<CR>')
map('i', 'fdd', '<C-g>u<C-R>=strftime("%Y-%m-%d")<CR>')
map('i', 'j', function() return vim.fn.pumvisible() == 1 and "<C-n>" or "j" end, { expr = true })
map('i', 'k', function() return vim.fn.pumvisible() == 1 and "<C-p>" or "k" end, { expr = true })
map('i', '<Tab>', function() return vim.fn.pumvisible() == 1 and "<C-x><C-o>" or "<Tab>" end, { expr = true })

map(basic_modes, '{h', function() vim.diagnostic.goto_prev { severity = { max = vim.diagnostic.severity.INFO } } end)
map(basic_modes, '}h', function() vim.diagnostic.goto_next { severity = { max = vim.diagnostic.severity.INFO } } end)
map(basic_modes, '{d', function() vim.diagnostic.goto_prev { severity = { min = vim.diagnostic.severity.WARN } } end)
map(basic_modes, '}d', function() vim.diagnostic.goto_next { severity = { min = vim.diagnostic.severity.WARN } } end)
map('i', 'fdk', '<C-x><C-o>')
map('n', '<C-k>', function() vim.lsp.buf.type_definition() end)

-- Filetype-specific mappings
vim.api.nvim_create_autocmd({ 'FileType' }, {
  pattern = "qf",
  group = vim.api.nvim_create_augroup("mappings_quickfix", {}),
  callback = function()
    map('n', 'o', '<CR><Cmd>copen<CR>', { silent = true, buffer = true })
    map('n', '<CR>', '<CR><Cmd>cclose<CR>', { silent = true, buffer = true })
  end,
})

-- Leader mappings
vim.g.mapleader = 's'
vim.g.maplocalleader = vim.g.mapleader .. 'l'

map(basic_modes, '<Leader>', '<NOP>')
map(basic_modes, '<LocalLeader>', '<NOP>')

-- Config files
map(basic_modes, '<Leader>r', '<NOP>')
map('n', '<Leader>re', '<Cmd>edit $MYVIMRC<CR>', silent)
map('n', '<Leader>rk', '<Cmd>edit stdpath("config") . "/skeleton.%:e"<CR>', silent)
function edit_ftplugin()
  cmd.edit(stdpath("config") .. "/after/ftplugin/" .. opt.filetype:get() .. ".vim")
end
map('n', '<Leader>ra', edit_ftplugin, silent)

-- Settings
map(basic_modes, '<Leader>s', '<NOP>')
map('n', '<Leader>sh', '<Cmd>nohlsearch<CR>', silent)
map('n', '<Leader>sd', '<Cmd>diffoff!<CR>:bdelete<CR>:bdelete<CR>', silent)
map('n', '<Leader>so', '<Cmd>windo diffthis<CR>', silent)
map('n', '<Leader>sl', '<Cmd>diffoff!<CR><C-w>h:bdelete<CR>', silent)
map('n', '<Leader>sn', [[<Cmd>set number! | set relativenumber!<CR>]], silent)
map('n', '<Leader>ss', '<Cmd>syntax sync fromstart<CR>', silent)

-- Buffers
map(basic_modes, '<Leader>k', '<NOP>')
map(basic_modes, '<Leader>ks', '<NOP>')
map(basic_modes, '<Leader>kv', '<NOP>')

map('n', '<Leader>kf', '<C-^>')
map('n', '<Leader>kc', function() spawn_shell(vim.cmd.enew) end, { silent = true })
map('n', '<Leader>ksc', function() spawn_shell(vim.cmd.new) end, silent)
map('n', '<Leader>ksf', '<Cmd>sbuffer #<CR>', silent)
map('n', '<Leader>kvc', function() spawn_shell(cmd.vnew) end, silent)
map('n', '<Leader>kvf', '<Cmd>vertical sbuffer #<CR>', silent)
map('n', '<Leader>kd', '<Cmd>bdelete<CR>', silent)
map('n', '<Leader>ko', function() return 'q:aedit ' .. get_buf_cwd() .. "/" end, { expr = true })
map('n', '<Leader>kl', [[i<C-l><C-\><C-n><Cmd>set scrollback=1 | set scrollback=100000<CR>]], silent)

-- Windows
map(basic_modes, '<Leader>d', '<C-w>')
map(basic_modes, '<Leader>dd', '<C-w><C-w>')

-- LSP
map('n', '<Leader>jr', function() vim.lsp.buf.rename() end)
-- map('n', '<Leader>jc', function() vim.lsp.buf.incoming_calls() end)
map('n', '<Leader>jc', function() vim.lsp.buf.references() end)
map('n', '<Leader>jf', function() vim.lsp.buf.code_action({ apply = true }) end)
map('n', '<Leader>jj', function() vim.lsp.buf.format() end)
map('n', '<Leader>jt', function() vim.diagnostic.enable(not vim.diagnostic.is_enabled()) end)
map('n', '<Leader>jo', function() vim.diagnostic.open_float() end)
map('n', '<Leader>jR',
  function()
    vim.lsp.stop_client(vim.lsp.get_clients())
    vim.cmd.edit()
  end
)
map('n', '<Leader>jl', '<Cmd>cclose<CR>')
map('n', '<Leader>jh', function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end)

-- Leader miscellany
map({ 'n', 'v', 's' }, '<Leader>;', 'q:i')
map({ 'n', 'v', 's' }, '<Leader>/', ':tag ')
map({ 'n', 'v' }, '<Leader>.', 'G')
map({ 'n', 'v' }, '<Leader>,', 'gg')
map(basic_modes, '<Leader>v', 'v')
map('n', '<Leader>c', '0D')
map('n', '<Leader>a', '<Cmd>qa<CR>')
map('n', '<Leader>o', 'o<C-u>')
map('n', '<Leader>O', 'O<C-u>')

--------------
-- Commands --
--------------

vim.api.nvim_create_user_command(
  'DiffOrig',
  'vertical new | set buftype=nofile | read # | 0delete_ | diffthis | wincmd p | diffthis',
  {}
)

----------------
-- LSP config --
----------------

vim.lsp.config('rust_analyzer', {
  settings = {
    ['rust-analyzer'] = {
      -- cargo = { features = { 'veea' }, },
      check = {
        -- features = { 'veea' },
        command = 'clippy',
        ignore = {
          'clippy::too_many_arguments',
          'clippy::large_enum_variant',
          'clippy::new_without_default',
          'clippy::len_without_is_empty',
          'clippy::needless_range_loop',
        },
      },
      diagnostics = { disabled = { 'inactive-code' } },
    },
  },
})

vim.lsp.enable('rust_analyzer')

-------------
-- Plugins --
-------------

require("config.lazy")
