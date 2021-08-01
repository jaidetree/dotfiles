#!/usr/bin/env bash

brew install direnv

brew install libvterm cmake

brew install elvish

source elvish/install.sh

luarocks install --local readline HISTORY_DIR=/usr/local/Cellar/readline/8.1 READLINE_DIR=/usr/local/Cellar/readline/8.1
