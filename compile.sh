#!/bin/sh
cpp emu_core_pre.lua > emu_core.lua
sed -i -e "s/^#.*//g" emu_core.lua
sed -i -e "s/SEG_ES+1/1/g" emu_core.lua
sed -i -e "s/SEG_CS+1/2/g" emu_core.lua
sed -i -e "s/SEG_SS+1/3/g" emu_core.lua
sed -i -e "s/SEG_DS+1/4/g" emu_core.lua
