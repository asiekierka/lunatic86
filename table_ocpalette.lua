oc_palette = {}

local pc_16_colors = {
	0x000000, 0x0000AA, 0x00AA00, 0x00AAAA,
	0xAA0000, 0xAA00AA, 0xAA5500, 0xAAAAAA,
	0x555555, 0x5555FF, 0x55FF55, 0x55FFFF,
	0xFF5555, 0xFF55FF, 0xFFFF55, 0xFFFFFF
}

for i=0,255 do
	if (i < 16) then
		oc_palette[i] = (i * 15) << 16 | (i * 15) << 8 | (i * 15)
	else
		local j = i - 16
		local b = math.floor((j % 5) * 255 / 4.0)
		local g = math.floor((math.floor(j / 5.0) % 8) * 255 / 7.0)
		local r = math.floor((math.floor(j / 40.0) % 6) * 255 / 5.0)
		oc_palette[i] = r << 16 | g << 8 | b
	end
end

local component = require("component")
local gpu = component.gpu

for i=0,15 do
	oc_palette[i] = pc_16_colors[i+1]
	gpu.setPaletteColor(i, oc_palette[i])
end

return oc_palette
