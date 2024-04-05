local wezterm = require 'wezterm'
local configdir = wezterm.config_dir
local homedir = wezterm.home_dir

fennel = dofile(configdir .. '/vendor/fennel.lua')

wezterm.log_error('Loaded fennel')

do
  local fnldir = (configdir .. "/fnl")
  for _, dir in ipairs({"/?.fnl", "/?/init.fnl"}) do
    fennel["path"] = (fnldir .. dir .. ";" .. fennel.path)
    fennel["macro-path"] = (fnldir .. dir .. ";" .. fennel["macro-path"])
  end
end

table.insert(package.searchers, 1, fennel.searcher)

wezterm.log_error('Installed fennel')

local config = (require "j.wezterm.config")

wezterm.log_error('Loaded custom config')

return config

