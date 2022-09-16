-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

_G._packer = _G._packer or {}
_G._packer.inside_compile = true

local time
local profile_info
local should_profile = false
if should_profile then
  local hrtime = vim.loop.hrtime
  profile_info = {}
  time = function(chunk, start)
    if start then
      profile_info[chunk] = hrtime()
    else
      profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
    end
  end
else
  time = function(chunk, start) end
end

local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end
  if threshold then
    table.insert(results, '(Only showing plugins that took longer than ' .. threshold .. ' ms ' .. 'to load)')
  end

  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/Users/j/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/Users/j/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/Users/j/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/Users/j/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/Users/j/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s), name, _G.packer_plugins[name])
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["bufferize.vim"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/bufferize.vim",
    url = "https://github.com/AndrewRadev/bufferize.vim"
  },
  catppuccin = {
    config = { "\27LJ\2\né\1\0\0\4\0\t\0\r6\0\0\0'\2\1\0B\0\2\0026\1\2\0009\1\3\1'\2\5\0=\2\4\0019\1\6\0B\1\1\0016\1\2\0009\1\a\1'\3\b\0D\1\2\0\27colorscheme catppuccin\bcmd\nsetup\nmocha\23catppuccin_flavour\6g\bvim\15catppuccin\frequire\0" },
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/catppuccin",
    url = "https://github.com/catppuccin/nvim"
  },
  ["colortils.nvim"] = {
    commands = { "Colortils" },
    config = { "\27LJ\2\n7\0\0\4\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\1\2\0004\3\0\0D\1\2\0\nsetup\14colortils\frequire\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/j/.local/share/nvim/site/pack/packer/opt/colortils.nvim",
    url = "https://github.com/nvim-colortils/colortils.nvim"
  },
  conjure = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/conjure",
    url = "https://github.com/Olical/conjure"
  },
  ["fennel.vim"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/fennel.vim",
    url = "https://github.com/jaawerth/fennel.vim"
  },
  neorg = {
    config = { "\27LJ\2\nó\3\0\0\t\0\22\0 6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\2\3\0015\4\5\0005\5\4\0=\5\6\0045\5\a\0=\5\b\4B\2\2\0019\2\3\0005\4\20\0005\5\t\0004\6\0\0=\6\n\0055\6\14\0005\a\f\0005\b\v\0=\b\r\a=\a\15\6=\6\16\0054\6\0\0=\6\17\0054\6\0\0=\6\18\0054\6\0\0=\6\19\5=\5\21\4D\2\2\0\tload\1\0\0\21external.context core.integrations.telescope\24core.norg.concealer\21core.norg.dirman\vconfig\1\0\0\15workspaces\1\0\0\1\0\2\twork\17~/neorg/work\rpersonal\21~/neorg/personal\18core.defaults\1\0\0\14highlight\1\0\1\venable\2\21ensure_installed\1\0\0\1\2\0\0\tnorg\nsetup\28nvim-treesitter.configs\nneorg\frequire\0" },
    load_after = {},
    loaded = true,
    needs_bufread = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/opt/neorg",
    url = "https://github.com/nvim-neorg/neorg"
  },
  ["neorg-contexts"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/neorg-contexts",
    url = "https://github.com/max397574/neorg-contexts"
  },
  ["neorg-telescope"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/neorg-telescope",
    url = "https://github.com/nvim-neorg/neorg-telescope"
  },
  ["nvim-literate"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/nvim-literate",
    url = "https://github.com/shoumodip/nvim-literate"
  },
  ["nvim-parinfer"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/nvim-parinfer",
    url = "https://github.com/gpanders/nvim-parinfer"
  },
  ["nvim-tmux-navigation"] = {
    config = { "\27LJ\2\nø\3\0\0\6\0\20\00006\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\3\0B\1\2\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\b\0009\5\t\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\n\0009\5\v\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\f\0009\5\r\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\14\0009\5\15\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\16\0009\5\17\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\18\0009\5\19\0D\1\4\0\25NvimTmuxNavigateNext\14<C-Space>\31NvimTmuxNavigateLastActive\v<C-\\\\>\26NvimTmuxNavigateRight\n<C-l>\23NvimTmuxNavigateUp\n<C-k>\25NvimTmuxNavigateDown\n<C-j>\25NvimTmuxNavigateLeft\n<C-h>\6n\bset\vkeymap\bvim\1\0\1\24disable_when_zoomed\2\nsetup\25nvim-tmux-navigation\frequire\0" },
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/nvim-tmux-navigation",
    url = "https://github.com/alexghergh/nvim-tmux-navigation"
  },
  ["nvim-treesitter"] = {
    after = { "neorg", "orgmode" },
    config = { "\27LJ\2\nú\1\0\0\5\0\b\0\n6\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\3\0005\4\4\0=\4\5\0035\4\6\0=\4\a\3D\1\2\0\frainbow\1\0\1\venable\2\14highlight\1\0\1\venable\2\1\0\2\17sync_install\2\17auto_install\2\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    only_config = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/nvim-treesitter",
    url = "https://github.com/nvim-treesitter/nvim-treesitter"
  },
  ["nvim-ts-rainbow"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/nvim-ts-rainbow",
    url = "https://github.com/p00f/nvim-ts-rainbow"
  },
  orgmode = {
    config = { "\27LJ\2\nÌ\1\0\0\a\0\f\0\0196\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\2\3\0B\2\1\0019\2\4\0015\4\b\0005\5\5\0005\6\6\0=\6\a\5=\5\t\0045\5\n\0=\5\v\4B\2\2\0019\2\4\0D\2\1\0\21ensure_installed\1\2\0\0\borg\14highlight\1\0\0&additional_vim_regex_highlighting\1\2\0\0\borg\1\0\1\venable\2\nsetup\21setup_ts_grammar\28nvim-treesitter.configs\forgmode\frequire\0" },
    load_after = {},
    loaded = true,
    needs_bufread = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/opt/orgmode",
    url = "https://github.com/nvim-orgmode/orgmode"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/packer.nvim",
    url = "https://github.com/wbthomason/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/plenary.nvim",
    url = "https://github.com/nvim-lua/plenary.nvim"
  },
  ["project.nvim"] = {
    config = { "\27LJ\2\n6\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\17project_nvim\frequire\0" },
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/project.nvim",
    url = "https://github.com/ahmedkhalf/project.nvim"
  },
  ["telescope-file-browser.nvim"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/telescope-file-browser.nvim",
    url = "https://github.com/nvim-telescope/telescope-file-browser.nvim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\2\né\2\0\1\a\3\f\0 -\1\0\0009\1\0\0019\1\1\0019\1\2\1-\2\1\0009\2\3\2B\2\1\2-\3\2\0009\3\4\3-\4\2\0009\4\5\4 \3\4\3\6\2\6\0X\4\2Ä\a\2\a\0X\4\4Ä9\4\b\1\18\6\0\0D\4\2\0X\4\vÄ\6\2\t\0X\4\2Ä\a\2\n\0X\4\4Ä9\4\v\1\18\6\0\0D\4\2\0X\4\3Ä\18\4\3\0\18\6\0\0D\4\2\0K\0\1\0\1¿\3¿\2¿\18goto_home_dir\a~/\6~\20goto_parent_dir\b../\a..\25move_selection_worse\21toggle_selection\21get_current_line\factions\17file_browser\15extensionsÑ\5\1\0\14\0(\0>6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0026\2\0\0'\4\3\0B\2\2\0026\3\0\0'\5\4\0B\3\2\0029\4\5\0019\4\6\0049\4\a\0043\5\b\0009\6\t\0015\b\20\0005\t\n\0005\n\15\0005\v\f\0009\f\v\2=\f\r\v9\f\v\2=\f\14\v=\v\16\n5\v\17\0009\f\v\2=\f\r\v=\v\18\n=\n\19\t=\t\21\b5\t\23\0005\n\22\0=\n\24\t5\n\25\0=\n\26\t=\t\27\b5\t$\0005\n\28\0005\v#\0005\f\30\0009\r\29\4=\r\31\f9\r \4=\r!\f=\5\"\f=\f\16\v=\v\19\n=\n\6\t=\t\5\bB\6\2\0019\6%\1'\b\6\0B\6\2\0019\6%\1'\b&\0B\6\2\0019\6%\1'\b'\0002\0\0ÄD\6\2\0\17yank_history\rprojects\19load_extension\1\0\0\1\0\0\n<Tab>\6~\18goto_home_dir\6^\1\0\0\20goto_parent_dir\1\0\6\fgrouped\2\tpath\n%:p:h\16cwd_to_path\2\ntheme\bivy\17hijack_netrw\1\18select_buffer\2\fpickers\14git_files\1\0\1\ntheme\bivy\15find_files\1\0\0\1\0\1\ntheme\bivy\rdefaults\1\0\0\rmappings\6n\1\0\0\6i\1\0\0\n<Esc>\n<C-g>\1\0\0\nclose\1\0\1\ntheme\bivy\nsetup\0\factions\17file_browser\15extensions\28telescope.actions.state\22telescope.actions\14telescope\nyanky\frequire\0" },
    load_after = {},
    loaded = true,
    needs_bufread = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/opt/telescope.nvim",
    url = "https://github.com/nvim-telescope/telescope.nvim"
  },
  ["vim-autosource"] = {
    config = { "\27LJ\2\nc\0\0\2\0\4\0\0066\0\0\0009\0\1\0005\1\3\0=\1\2\0+\0\0\0L\0\2\0\1\5\0\0\14.exrc.fnl\14.exrc.lua\14.exrc.vim\n.exrc\26autosource_conf_names\6g\bvim\0" },
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/vim-autosource",
    url = "https://github.com/jenterkin/vim-autosource"
  },
  ["vim-sexp"] = {
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/vim-sexp",
    url = "https://github.com/guns/vim-sexp"
  },
  ["which-key.nvim"] = {
    config = { "\27LJ\2\n7\0\0\4\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\1\2\0004\3\0\0D\1\2\0\nsetup\14which-key\frequire\0" },
    loaded = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/which-key.nvim",
    url = "https://github.com/folke/which-key.nvim"
  },
  ["yanky.nvim"] = {
    after = { "telescope.nvim" },
    config = { "\27LJ\2\n/\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\nyanky\frequire\0" },
    loaded = true,
    only_config = true,
    path = "/Users/j/.local/share/nvim/site/pack/packer/start/yanky.nvim",
    url = "https://github.com/gbprod/yanky.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: nvim-tmux-navigation
time([[Config for nvim-tmux-navigation]], true)
try_loadstring("\27LJ\2\nø\3\0\0\6\0\20\00006\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\3\0B\1\2\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\b\0009\5\t\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\n\0009\5\v\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\f\0009\5\r\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\14\0009\5\15\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\16\0009\5\17\0B\1\4\0016\1\4\0009\1\5\0019\1\6\1'\3\a\0'\4\18\0009\5\19\0D\1\4\0\25NvimTmuxNavigateNext\14<C-Space>\31NvimTmuxNavigateLastActive\v<C-\\\\>\26NvimTmuxNavigateRight\n<C-l>\23NvimTmuxNavigateUp\n<C-k>\25NvimTmuxNavigateDown\n<C-j>\25NvimTmuxNavigateLeft\n<C-h>\6n\bset\vkeymap\bvim\1\0\1\24disable_when_zoomed\2\nsetup\25nvim-tmux-navigation\frequire\0", "config", "nvim-tmux-navigation")
time([[Config for nvim-tmux-navigation]], false)
-- Config for: project.nvim
time([[Config for project.nvim]], true)
try_loadstring("\27LJ\2\n6\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\17project_nvim\frequire\0", "config", "project.nvim")
time([[Config for project.nvim]], false)
-- Config for: yanky.nvim
time([[Config for yanky.nvim]], true)
try_loadstring("\27LJ\2\n/\0\0\3\0\3\0\0056\0\0\0'\2\1\0B\0\2\0029\1\2\0D\1\1\0\nsetup\nyanky\frequire\0", "config", "yanky.nvim")
time([[Config for yanky.nvim]], false)
-- Config for: which-key.nvim
time([[Config for which-key.nvim]], true)
try_loadstring("\27LJ\2\n7\0\0\4\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\1\2\0004\3\0\0D\1\2\0\nsetup\14which-key\frequire\0", "config", "which-key.nvim")
time([[Config for which-key.nvim]], false)
-- Config for: vim-autosource
time([[Config for vim-autosource]], true)
try_loadstring("\27LJ\2\nc\0\0\2\0\4\0\0066\0\0\0009\0\1\0005\1\3\0=\1\2\0+\0\0\0L\0\2\0\1\5\0\0\14.exrc.fnl\14.exrc.lua\14.exrc.vim\n.exrc\26autosource_conf_names\6g\bvim\0", "config", "vim-autosource")
time([[Config for vim-autosource]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\nú\1\0\0\5\0\b\0\n6\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\3\0005\4\4\0=\4\5\0035\4\6\0=\4\a\3D\1\2\0\frainbow\1\0\1\venable\2\14highlight\1\0\1\venable\2\1\0\2\17sync_install\2\17auto_install\2\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: catppuccin
time([[Config for catppuccin]], true)
try_loadstring("\27LJ\2\né\1\0\0\4\0\t\0\r6\0\0\0'\2\1\0B\0\2\0026\1\2\0009\1\3\1'\2\5\0=\2\4\0019\1\6\0B\1\1\0016\1\2\0009\1\a\1'\3\b\0D\1\2\0\27colorscheme catppuccin\bcmd\nsetup\nmocha\23catppuccin_flavour\6g\bvim\15catppuccin\frequire\0", "config", "catppuccin")
time([[Config for catppuccin]], false)
-- Load plugins in order defined by `after`
time([[Sequenced loading]], true)
vim.cmd [[ packadd orgmode ]]

-- Config for: orgmode
try_loadstring("\27LJ\2\nÌ\1\0\0\a\0\f\0\0196\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\2\3\0B\2\1\0019\2\4\0015\4\b\0005\5\5\0005\6\6\0=\6\a\5=\5\t\0045\5\n\0=\5\v\4B\2\2\0019\2\4\0D\2\1\0\21ensure_installed\1\2\0\0\borg\14highlight\1\0\0&additional_vim_regex_highlighting\1\2\0\0\borg\1\0\1\venable\2\nsetup\21setup_ts_grammar\28nvim-treesitter.configs\forgmode\frequire\0", "config", "orgmode")

vim.cmd [[ packadd neorg ]]

-- Config for: neorg
try_loadstring("\27LJ\2\nó\3\0\0\t\0\22\0 6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\2\3\0015\4\5\0005\5\4\0=\5\6\0045\5\a\0=\5\b\4B\2\2\0019\2\3\0005\4\20\0005\5\t\0004\6\0\0=\6\n\0055\6\14\0005\a\f\0005\b\v\0=\b\r\a=\a\15\6=\6\16\0054\6\0\0=\6\17\0054\6\0\0=\6\18\0054\6\0\0=\6\19\5=\5\21\4D\2\2\0\tload\1\0\0\21external.context core.integrations.telescope\24core.norg.concealer\21core.norg.dirman\vconfig\1\0\0\15workspaces\1\0\0\1\0\2\twork\17~/neorg/work\rpersonal\21~/neorg/personal\18core.defaults\1\0\0\14highlight\1\0\1\venable\2\21ensure_installed\1\0\0\1\2\0\0\tnorg\nsetup\28nvim-treesitter.configs\nneorg\frequire\0", "config", "neorg")

vim.cmd [[ packadd telescope.nvim ]]

-- Config for: telescope.nvim
try_loadstring("\27LJ\2\né\2\0\1\a\3\f\0 -\1\0\0009\1\0\0019\1\1\0019\1\2\1-\2\1\0009\2\3\2B\2\1\2-\3\2\0009\3\4\3-\4\2\0009\4\5\4 \3\4\3\6\2\6\0X\4\2Ä\a\2\a\0X\4\4Ä9\4\b\1\18\6\0\0D\4\2\0X\4\vÄ\6\2\t\0X\4\2Ä\a\2\n\0X\4\4Ä9\4\v\1\18\6\0\0D\4\2\0X\4\3Ä\18\4\3\0\18\6\0\0D\4\2\0K\0\1\0\1¿\3¿\2¿\18goto_home_dir\a~/\6~\20goto_parent_dir\b../\a..\25move_selection_worse\21toggle_selection\21get_current_line\factions\17file_browser\15extensionsÑ\5\1\0\14\0(\0>6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0026\2\0\0'\4\3\0B\2\2\0026\3\0\0'\5\4\0B\3\2\0029\4\5\0019\4\6\0049\4\a\0043\5\b\0009\6\t\0015\b\20\0005\t\n\0005\n\15\0005\v\f\0009\f\v\2=\f\r\v9\f\v\2=\f\14\v=\v\16\n5\v\17\0009\f\v\2=\f\r\v=\v\18\n=\n\19\t=\t\21\b5\t\23\0005\n\22\0=\n\24\t5\n\25\0=\n\26\t=\t\27\b5\t$\0005\n\28\0005\v#\0005\f\30\0009\r\29\4=\r\31\f9\r \4=\r!\f=\5\"\f=\f\16\v=\v\19\n=\n\6\t=\t\5\bB\6\2\0019\6%\1'\b\6\0B\6\2\0019\6%\1'\b&\0B\6\2\0019\6%\1'\b'\0002\0\0ÄD\6\2\0\17yank_history\rprojects\19load_extension\1\0\0\1\0\0\n<Tab>\6~\18goto_home_dir\6^\1\0\0\20goto_parent_dir\1\0\6\fgrouped\2\tpath\n%:p:h\16cwd_to_path\2\ntheme\bivy\17hijack_netrw\1\18select_buffer\2\fpickers\14git_files\1\0\1\ntheme\bivy\15find_files\1\0\0\1\0\1\ntheme\bivy\rdefaults\1\0\0\rmappings\6n\1\0\0\6i\1\0\0\n<Esc>\n<C-g>\1\0\0\nclose\1\0\1\ntheme\bivy\nsetup\0\factions\17file_browser\15extensions\28telescope.actions.state\22telescope.actions\14telescope\nyanky\frequire\0", "config", "telescope.nvim")

time([[Sequenced loading]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Colortils lua require("packer.load")({'colortils.nvim'}, { cmd = "Colortils", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
time([[Defining lazy-load commands]], false)


_G._packer.inside_compile = false
if _G._packer.needs_bufread == true then
  vim.cmd("doautocmd BufRead")
end
_G._packer.needs_bufread = false

if should_profile then save_profiles() end

end)

if not no_errors then
  error_msg = error_msg:gsub('"', '\\"')
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
