# BEHAVIOR CHANGES
##############################################################################
# fish_vi_key_bindings
# fish_hybrid_key_bindings
# fish_default_key_bindings

# PATH
##############################################################################
set -x PATH $HOME/.npm-global/bin $HOME/bin $HOME/.yarn/bin $HOME/.config/yarn/global/node_modules/.bin $PATH


# CONFIG
##############################################################################
set -gx JAVA_TOOL_OPTIONS "--add-modules=java.xml.bind"

# Conditional Configs
##############################################################################
if test "$TERM" = "dumb"
    fish_default_key_bindings
else
    fish_hybrid_key_bindings
end
