local util = {}

function util.get_buf_cwd()
  if vim.b.terminal_job_pid == nil then
    return vim.fn.expand("%:p:h")
  else
    -- Doing this using nvim's system() since apparently lua only lets you execute
    -- external processes as strings and I really don't want to worry about escaping.
    local dir = vim.fn.system({ "realpath", "/proc/" .. vim.b.terminal_job_pid .. "/cwd" })
    return dir:sub(1, -2)
  end
end

function util.spawn_shell(layout_callback)
  local cwd = util.get_buf_cwd()
  if vim.fn.isdirectory(cwd) == 0 then
    cwd = os.getenv("HOME") or "/"
  end

  layout_callback()
  vim.fn.jobstart({ os.getenv("SHELL") }, { cwd = cwd, term = true })
  vim.cmd.startinsert()
end

function util.toggle_rust_analyzer_feature(target_feature)
  local clients = vim.lsp.get_clients({ name = 'rust_analyzer' })
  local features = vim.tbl_get(clients, 1, 'config', 'settings', 'rust-analyzer', 'cargo', 'features')
  if features == nil then
    features = {}
  end

  local index = vim.iter(ipairs(features)):find(function(_, feature) vim.print(feature); return feature == target_feature end)
  if index == nil then
    features[#features+1] = target_feature
  else
    table.remove(features, index)
  end

  vim.lsp.config('rust_analyzer', { settings = { ['rust-analyzer'] = {
    cargo = { features = features },
    check = { features = features },
  }}})

  local cur_buf = vim.api.nvim_get_current_buf()
  vim.lsp.stop_client(vim.lsp.get_clients({ name = 'rust_analyzer' }))
  vim.cmd.bufdo("e")
  vim.api.nvim_set_current_buf(cur_buf)

  if index == nil then
    print("feature " .. target_feature .. " set")
  else
    print("feature " .. target_feature .. " unset")
  end
end

return util
