# BEHAVIOR CHANGES
##############################################################################
# fish_vi_key_bindings
# fish_hybrid_key_bindings
# fish_default_key_bindings

# PATH
##############################################################################
set -x PATH $HOME/.npm-global/bin $HOME/bin $HOME/.yarn/bin $HOME/.config/yarn/global/node_modules/.bin $PATH
set -x PATH $HOME/.luarocks/bin /usr/local/bin $PATH
set -x LUA_PATH '$HOME/.luarocks/share/lua/5.3/?.lua;$HOME/.luarocks/share/lua/5.3/?/init.lua;/usr/local/share/lua/5.3/?.lua;/usr/local/share/lua/5.3/?/init.lua;/usr/local/Cellar/luarocks/3.1.3/share/lua/5.3/?.lua;/usr/local/lib/lua/5.3/?.lua;/usr/local/lib/lua/5.3/?/init.lua;./?.lua;./?/init.lua'
set -x LUA_CPATH '$HOME/.luarocks/lib/lua/5.3/?.so;/usr/local/lib/lua/5.3/?.so;/usr/local/lib/lua/5.3/loadall.so;./?.so'

# CONFIG
##############################################################################
# set -gx JAVA_TOOL_OPTIONS "--add-modules=java.xml.bind"

# Conditional Configs
##############################################################################
if test "$TERM" = "dumb"
    fish_default_key_bindings
else
    fish_hybrid_key_bindings
end
