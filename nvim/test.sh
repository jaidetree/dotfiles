#!/usr/bin/env bash
# Run a test
# Usage: 
# ./test.sh validate.fnl
# ./watch.sh ./test.sh validate.fnl
exec nvim --headless -c ":FnlFile ~/.config/nvim/fnl/tests/$1" -c 'echo""|qall!'
