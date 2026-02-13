local util = require('config.util')

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

map('n', ']f', '<Cmd>cn<CR>', silent)
map('n', '[f', '<Cmd>cp<CR>', silent)

map({'v', 's'}, '<', '<gv')
map({'v', 's'}, '>', '>gv')

map('t', '<C-l>', [[<C-l><C-\><C-n><Cmd>set scrollback=1 | set scrollback=100000<CR>a]], silent)
map('t', '<C-b>', '<CR><CR><CR><CR><CR><CR><CR><CR><CR><CR><CR><CR><CR><CR><CR><CR><CR><CR><C-l>')

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

map(basic_modes, '[h', function() vim.diagnostic.jump { count = -1, float = true, severity = { max = vim.diagnostic.severity.INFO } } end)
map(basic_modes, ']h', function() vim.diagnostic.jump { count = 1, float = true, severity = { max = vim.diagnostic.severity.INFO } } end)
map(basic_modes, '[d', function() vim.diagnostic.jump { count = -1, float = true, severity = { min = vim.diagnostic.severity.WARN } } end)
map(basic_modes, ']d', function() vim.diagnostic.jump { count = 1, float = true, severity = { min = vim.diagnostic.severity.WARN } } end)
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
map('n', '<Leader>kc', function() util.spawn_shell(vim.cmd.enew) end, { silent = true })
map('n', '<Leader>ksc', function() util.spawn_shell(vim.cmd.new) end, silent)
map('n', '<Leader>ksf', '<Cmd>sbuffer #<CR>', silent)
map('n', '<Leader>kvc', function() util.spawn_shell(vim.cmd.vnew) end, silent)
map('n', '<Leader>kvf', '<Cmd>vertical sbuffer #<CR>', silent)
map('n', '<Leader>kd', '<Cmd>bdelete<CR>', silent)
map('n', '<Leader>ko', function() return 'q:aedit ' .. util.get_buf_cwd() .. "/" end, { expr = true })

-- Windows
map(basic_modes, '<Leader>d', '<C-w>')
map(basic_modes, '<Leader>dd', '<C-w><C-w>')

-- LSP
map('n', '<Leader>jr', function() vim.lsp.buf.rename() end)
map('n', '<Leader>jc', function() vim.lsp.buf.references() end)
map('n', '<Leader>jf', function() vim.lsp.buf.code_action({ apply = true }) end)
map('n', '<Leader>jj', function() vim.lsp.buf.format() end)
map('n', '<Leader>jt', function() vim.diagnostic.enable(not vim.diagnostic.is_enabled()) end)
map('n', '<Leader>jo', function() vim.diagnostic.open_float() end)
map('n', '<Leader>jR',
  function()
    local cur_buf = vim.api.nvim_get_current_buf()
    vim.lsp.stop_client(vim.lsp.get_clients())
    vim.cmd.bufdo("e")
    vim.api.nvim_set_current_buf(cur_buf)
  end
)
map('n', '<Leader>jl', '<Cmd>cclose<CR>')
map('n', '<Leader>jh', function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end)
map('n', '<Leader>jv', function() util.toggle_rust_analyzer_feature('veea') end)

-- Leader miscellany
map({ 'n', 'v', 's' }, '<Leader>;', 'q:i')
map({ 'n', 'v', 's' }, '<Leader>/', ':tag ')
map({ 'n', 'v' }, '<Leader>.', 'G')
map({ 'n', 'v' }, '<Leader>,', 'gg')
map(basic_modes, '<Leader>v', 'v')
map('n', '<Leader>c', '0D')
map('n', '<Leader>o', 'o<C-u>')
map('n', '<Leader>O', 'O<C-u>')
