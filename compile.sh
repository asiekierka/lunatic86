#!/bin/sh
cpp emu_core_pre.lua > emu_core.lua
sed -i -e "s/^#.*//g" emu_core.lua
