#!/usr/bin/env bash

exec nodemon --watch ./fnl/ --ext fnl --exec "$@"
