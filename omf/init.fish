# [[file:oh-my-fish.org::*Path][Path:1]]
set -x PATH \
    $HOME/.npm-global/bin \
    $HOME/bin \
    $HOME/.yarn/bin \
    $HOME/.config/yarn/global/node_modules/.bin \
    $HOME/.luarocks/bin \
    $HOME/.emacs.d/bin \
    /usr/local/bin \
    /usr/local/opt/openjdk/bin \
    /usr/bin \
    /usr/sbin \
    /sbin \
    /usr/local/MacGPG2/bin \
    /Library/Frameworks/Mono.framework/Versions/Current/bin \
    $PATH
# Path:1 ends here

# [[file:oh-my-fish.org::*Bindings][Bindings:1]]
fish_default_key_bindings
# Bindings:1 ends here

# [[file:oh-my-fish.org::*Dir Env][Dir Env:1]]
direnv hook fish | source
# Dir Env:1 ends here

# [[file:oh-my-fish.org::*ASDF Version Manager][ASDF Version Manager:1]]
source (brew --prefix asdf)"/asdf.fish"
# ASDF Version Manager:1 ends here
