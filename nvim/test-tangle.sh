#!/usr/bin/env bash
#
exec nvim --headless -c  ":e /Users/j/dotfiles/tmux/tmux.org" -c ":Tangle" -c 'echo""|qall!'
