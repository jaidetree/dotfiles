--test.lua

-- Support upcoming 5.4 release and also use luarocks' local path
package.path = package.path .. ";" .. os.getenv("HOME") .. "/.luarocks/share/lua/5.4/?.lua;" .. os.getenv("HOME") .. "/.luarocks/share/lua/5.4/?/init.lua"
package.cpath = package.cpath .. ";" .. os.getenv("HOME") .. "/.luarocks/lib/lua/5.4/?.so"

fennel = require("fennel")
table.insert(package.loaders or package.searchers, fennel.searcher)

local testRunner = require "test-runner"

testRunner["load-tests"](_cli.args)
