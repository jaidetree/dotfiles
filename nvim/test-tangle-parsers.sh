#!/usr/bin/env bash
#
exec nvim --headless -c ":FnlFile ~/.config/nvim/fnl/tests/tangle-parsers-test.fnl" -c 'echo""|qall!'
