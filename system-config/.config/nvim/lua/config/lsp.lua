vim.diagnostic.config {
  severity_sort = true,
  virtual_text = { severity = { min = vim.diagnostic.severity.INFO }},
  signs = { severity = { min = vim.diagnostic.severity.INFO }},
}

vim.lsp.config('rust_analyzer', {
  settings = {
    ['rust-analyzer'] = {
      check = {
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
  workspace_required = true,
})

vim.lsp.config('lua_ls', {
  on_init = function(client)
    if client.workspace_folders then
      local path = client.workspace_folders[1].name
      if
        path ~= vim.fn.stdpath('config')
        ---@diagnostic disable-next-line: undefined-field
        and (vim.uv.fs_stat(path .. '/.luarc.json') or vim.uv.fs_stat(path .. '/.luarc.jsonc'))
      then
        return
      end
    end

    client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
      runtime = {
        -- Tell the language server which version of Lua you're using (most
        -- likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
        -- Tell the language server how to find Lua modules same way as Neovim
        -- (see `:h lua-module-load`)
        path = {
          'lua/?.lua',
          'lua/?/init.lua',
        },
      },
      -- Make the server aware of Neovim runtime files
      workspace = {
        checkThirdParty = false,
        library = {
          vim.env.VIMRUNTIME
        }
      }
    })
  end,
  settings = {
    Lua = {}
  }
})

vim.lsp.enable({ 'rust_analyzer', 'lua_ls', 'pylsp', 'bashls' })
