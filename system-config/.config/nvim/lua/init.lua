local opt = vim.opt
local cmd = vim.cmd
local fn = vim.fn
local api = vim.api
local g = vim.g
local b = vim.b
local map = vim.keymap.set

-- builtin options
opt.backup = true
opt.backupdir = fn.stdpath("data") .. "/backup"
opt.breakat = " "
opt.colorcolumn = "+1"
opt.diffopt:append { "iwhite", "foldcolumn:0", "context:1000000" }
opt.expandtab = true
opt.foldenable = false
opt.guicursor = { "n-v-c-sm:block", "i-ci-ve-r-cr-o:hor20", "a:blinkon500-blinkoff500" }
opt.hidden = true
opt.ignorecase = true
opt.inccommand = "split"
opt.laststatus = 3
opt.matchtime = 1
opt.mouse = ""
opt.modeline = false
opt.number = true
opt.relativenumber = true
opt.scrollback = 100000
opt.shiftwidth = 2
opt.showcmd = true
opt.showmatch = true
opt.smartcase = true
opt.softtabstop = 2
opt.spelllang = "en_ca"
opt.splitbelow = true
opt.splitright = true
opt.tabstop = 2
opt.tags = fn.stdpath("data") .. "/tags"
opt.tildeop = true
opt.timeoutlen = 1000
opt.undofile = true
opt.undodir = fn.stdpath("data") .. "/undo"

-- colour scheme
g.inkpot_black_background = true
opt.termguicolors = false
cmd.colorscheme("inkpot")
api.nvim_set_hl(0, 'ColorColumn', { ctermbg = 60 })

-- tbufferline
g.tbufferline_enable_on_startup = 1

-- automcds
local skeleton_au_group = vim.api.nvim_create_augroup("skeleton_files", {})
api.nvim_create_autocmd("BufNewFile", {
  pattern = "*.tex",
  group = skeleton_au_group,
  command = "0read " .. fn.stdpath("config") .. "/skeleton.tex | normal! Gdd6k$",
})
api.nvim_create_autocmd("BufNewFile", {
  pattern = "*.js",
  group = skeleton_au_group,
  command = "0read " .. fn.stdpath("config") .. "/skeleton.js | normal! G",
})

local terminal_au_group = vim.api.nvim_create_augroup("terminal", {})
api.nvim_create_autocmd("TermOpen", {
  group = terminal_au_group,
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
})

-- Utility functions
local function get_buf_cwd()
  if b.terminal_job_pid == nil then
    -- TODO: why does this need "vim." in front?
    return fn.expand("%:p:h")
  else
    -- Doing this using nvim's system() since apparently lua only lets you execute
    -- external processes as strings and I really don't want to worry about escaping.
    local dir = fn.system({ "realpath", "/proc" .. b.terminal_job_pid .. "/cwd" })
    return dir:sub(1, -2)
  end
end

local function spawn_shell(layout_callback)
  local cwd = get_buf_cwd()
  layout_callback()
  fn.termopen({ os.getenv("SHELL") }, { cwd = cwd })
  cmd.startinsert()
end

-- Mappings
local basic_modes = {'n', 'v', 's', 'o'}
local silent = { silent = true }

map(basic_modes, '/', [[/\v]])
map(basic_modes, '?', [[?\v]])
map(basic_modes, '<Tab>', '%')
map(basic_modes, 'v', '<C-b>')
map(basic_modes, 'm', '<C-f>')

map('n', '<Space>', '<Plug>tbufferline#Buffer')
map('n', 'S', 'm')
map('n', 'x', ':write<CR>', silent)
map('n', '<Up>', '<Cmd>.m-2<CR>', silent)
map('n', '<Down>', '<Cmd>.m+1<CR>', silent)

map({'v', 's'}, '<', '<gv')
map({'v', 's'}, '>', '>gv')

local swaps = { ['0'] = ')', ['9'] = '(', ['8'] = '*', ['7'] = '&', ['6'] = '^' }
for i, v in pairs(swaps) do
  map(basic_modes, i, v)
  map(basic_modes, v, i)
end

map('t', ',.', [[<C-\><C-n>]])

for _, fst in ipairs({ 'f', 'F' }) do
  for _, snd in ipairs({ 'j', 'J' }) do
    map('i', fst .. snd, '<C-]><Esc>')
    map('i', snd .. fst, '<C-]><Esc>')
  end
end

map('i', '<C-u>', '<C-g>u<C-u>')
map('i', 'fdn', '<C-n>')
map('i', 'fdp', '<C-p>')
map('i', 'fdh', '<C-]><Cmd>nohlsearch<CR>')
map('i', 'fdd', '<C-g>u<C-R>=strftime("%Y-%m-%d")<CR>')

g.mapleader = 's'
g.maplocalleader = g.mapleader .. 'l'

map(basic_modes, '<Leader>', '<NOP>')
map(basic_modes, '<LocalLeader>', '<NOP>')

-- Config files
map(basic_modes, '<Leader>r', '<NOP>')
map('n', '<Leader>re', '<Cmd>edit $MYVIMRC<CR>', silent)
map('n', '<Leader>rs', [[<Cmd>source $MYVIMRC \| filetype detect<CR>]], silent)
map('n', '<Leader>rk', '<Cmd>edit stdpath("config") . "/skeleton.%:e"<CR>', silent)
function edit_ftplugin()
  cmd.edit(stdpath("config") .. "/after/ftplugin/" .. opt.filetype:get() .. ".vim")
end
map('n', '<Leader>ra', edit_ftplugin, silent)

map(basic_modes, '<Leader>s', '<NOP>')
map('n', '<Leader>sh', '<Cmd>nohlsearch<CR>', silent)
map('n', '<Leader>sd', '<Cmd>diffoff!<CR>:bdelete<CR>:bdelete<CR>', silent)
map('n', '<Leader>sn', [[<Cmd>set number! \| set relativenumber!<CR>]], silent)
map('n', '<Leader>ss', '<Cmd>syntax sync fromstart<CR>', silent)

map(basic_modes, '<Leader>k', '<NOP>')
map(basic_modes, '<Leader>ks', '<NOP>')
map(basic_modes, '<Leader>kv', '<NOP>')

map('n', '<Leader>kf', '<C-^>')
map('n', '<Leader>kc', function() spawn_shell(cmd.enew) end, { silent = true })
map('n', '<Leader>ksl', '<Plug>tbufferline#SplitBuffer', silent)
map('n', '<Leader>ksc', function() spawn_shell(cmd.new) end, silent)
map('n', '<Leader>ksf', '<Cmd>sbuffer #<CR>', silent)
map('n', '<Leader>kvl', '<Plug>tbufferline#VSplitBuffer', silent)
map('n', '<Leader>kvc', function() spawn_shell(cmd.vnew) end, silent)
map('n', '<Leader>kvf', '<Cmd>vertical sbuffer #<CR>', silent)
map('n', '<Leader>kd', '<Cmd>bdelete<CR>', silent)
map('n', '<Leader>kj', '<Plug>tbufferline#StepForward', silent)
map('n', '<Leader>kk', '<Plug>tbufferline#StepBack', silent)
map('n', '<Leader>ko', function() return 'q:aedit ' .. get_buf_cwd() .. "/" end, { expr = true })

map(basic_modes, '<Leader>d', '<C-w>')
map(basic_modes, '<Leader>dd', '<C-w><C-w>')

map({ 'n', 'v', 's' }, '<Leader>j', 'gq')

map({ 'n', 'v', 's' }, '<Leader>;', 'q:i')

map('n', '<Leader>.', 'G')
map('n', '<Leader>,', 'gg')
map(basic_modes, '<Leader>v', 'v')

-- Commands
api.nvim_create_user_command(
  'DiffOrig',
  'vertical new | set buftype=nofile | read # | 0delete_ | diffthis | wincmd p | diffthis',
  {}
)
