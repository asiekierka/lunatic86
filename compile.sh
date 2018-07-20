#!/bin/sh
cpp emup.lua > emu.lua
sed -i -e "s/^#.*//g" emu.lua
