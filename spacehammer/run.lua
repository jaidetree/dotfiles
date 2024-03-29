--run.lua
-- A script to run fennel files as a cli

-- Support upcoming 5.4 release and also use luarocks' local path
package.path = package.path .. ";" .. os.getenv("HOME") .. "/.luarocks/share/lua/5.4/?.lua;" .. os.getenv("HOME") .. "/.luarocks/share/lua/5.4/?/init.lua"
package.cpath = package.cpath .. ";" .. os.getenv("HOME") .. "/.luarocks/lib/lua/5.4/?.so"

fennel = require("fennel")

local searcher = fennel.makeSearcher({
      useMetadata = true,
})

local runner = require "run-fennel-file"

runner["run-file"](_cli.args)
