oc_palette = {}

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

return oc_palette
