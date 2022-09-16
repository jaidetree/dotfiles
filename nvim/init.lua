local configdir = vim.fn.stdpath("config")
do
  local rtp = vim.api.nvim_get_option("runtimepath")
  local custompaths = {(configdir .. "/fnl"), (configdir .. "/lua")}
  local customrtp = table.concat(custompaths, ",")
  vim.api.nvim_set_option("runtimepath", (customrtp .. "," .. rtp))
end
fennel = require("config.fennel")
do
  local fnldir = (configdir .. "/fnl")
  for _, dir in ipairs({"/?.fnl", "/?/init.fnl"}) do
    fennel["path"] = (fnldir .. dir .. ";" .. fennel.path)
  end
end
table.insert(package.loaders, 1, fennel.searcher)
local cfg = require("config.core")
return cfg
