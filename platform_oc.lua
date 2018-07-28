local component = require("component")
local event = require("event")
local unicode = require("unicode")
local keyboard = require("keyboard")
local computer = require("computer")
local gpu = component.gpu
if gpu.getDepth() < 4 then
	error("Tier 2 or above GPU card required!")
end

local cp437_trans = require("table_cp437")
local oc_palette = require("table_ocpalette")
dofile("kbdmaps.lua")

local beeper = nil
local beeper_count = 0

for a,b in component.list("beep") do beeper_count = beeper_count + 1 end
if beeper_count > 0 then beeper = component.beep end

local qdr = {}
for i=0,255 do
  local dat = (i & 0x01) << 7
  dat = dat | (i & 0x02) >> 1 << 6
  dat = dat | (i & 0x04) >> 2 << 5
  dat = dat | (i & 0x08) >> 3 << 2
  dat = dat | (i & 0x10) >> 4 << 4
  dat = dat | (i & 0x20) >> 5 << 1
  dat = dat | (i & 0x40) >> 6 << 3
  dat = dat | (i & 0x80) >> 7
  qdr[i + 1] = unicode.char(0x2800 | dat)
end

function emu_debug(s)
	return false
end

function platform_beep(freq, time)
	freq = math.floor(freq)
	if freq < 20 then freq = 20
	elseif freq > 2000 then freq = 2000 end
	if beeper then beeper.beep({[freq]=(time or 0.05)}) end
end

function platform_sleep(t)
	os.sleep(t)
end

function platform_key_down(code)
	return keyboard.isKeyDown(code)
end

event.listen("redstone_changed", function(name, addr, side, oldV, newV)
	if oldV < newV and newV >= 8 then
		kbd_send_ibm(0x01, 0x1B)
	end
end)

local function oc_code_transform(code, char)
	if code == 0x1C then char = 13 end
	if code >= 0xC0 and code <= 0xDF then code = code & 0x7F end
	if code >= 0x80 then
		-- fallback
		code = map_char_to_key[char] or code
	end
	return code, char
end

local kd_matrix = {}

event.listen("key_down", function(name, addr, char, code, player)
	code, char = oc_code_transform(code, char)
	if code >= 0 and code <= 0x7F then
		if kd_matrix[code] then
			-- release first, for autorepeat
			kbd_send_ibm(0x80 | code, char)
		end
		kd_matrix[code] = true
		kbd_send_ibm(code, char)
	end
end)

event.listen("key_up", function(name, addr, char, code, player)
	code, char = oc_code_transform(code, char)
	if code >= 0 and code <= 0x7F then
		kd_matrix[code] = nil
		kbd_send_ibm(0x80 | code, char)
	end
end)

function platform_error(msg)
	print(msg)
	error(msg)
end

local function dstrings_add(dstrings, atr, x, y, str)
	if dstrings[atr] then
		table.insert(dstrings[atr], {x, y, table.concat(str)})
	else
		dstrings[atr] = {{x, y, table.concat(str)}}
	end
end

local function dstrings_draw(dstrings, pal)
	local dstrkeys = {}
	for k in pairs(dstrings) do table.insert(dstrkeys, k) end
	table.sort(dstrkeys)

	local lastBG = -1
	local lastFG = -1

	for _,atr in pairs(dstrkeys) do
		local arr = dstrings[atr]
		if pal then
			local tbg = (atr & 0xFF00) >> 8
			local tfg = atr & 0x00FF
			if lastBG ~= tbg then
				gpu.setBackground(oc_palette[tbg])
				lastBG = tbg
			end
			if lastFG ~= tfg then
				gpu.setForeground(oc_palette[tfg])
				lastFG = tfg
			end
		else
			local tbg = (atr & 0xF0) >> 4
			local tfg = atr & 0x0F
			if lastBG ~= tbg then
				gpu.setBackground(pc_16_colors[1 + tbg])
				lastBG = tbg
			end
			if lastFG ~= tfg then
				gpu.setForeground(pc_16_colors[1 + tfg])
				lastFG = tfg
			end
		end

		for i,dat in ipairs(arr) do
			gpu.set(dat[1], dat[2], dat[3], false)
		end
	end
end

local function gpu_set_res_diff(width, height)
	local rw, rh = gpu.getResolution()
	if rw ~= width or rh ~= height then
		gpu.setResolution(width, height)
	end
end


-- Takes four pixels and turns them into two.
--local plat_cga_mono_avg = {
--	0, 1, 0, 1,
--	2, 3, 2, 3,
--	0, 1, 0, 1,
--	2, 3, 2, 3
--} Naive
local plat_cga_mono_avg = {
	0, 1, 1, 1,
	2, 1, 2, 1,
	2, 1, 2, 1,
	2, 2, 2, 3
} -- Less naive?

-- 00..0F for each
local function plat_cga_chr(v1, v2, v3, v4)
	return qdr[1 +
		( (plat_cga_mono_avg[v4 + 1])
		| (plat_cga_mono_avg[v3 + 1] << 2)
		| (plat_cga_mono_avg[v2 + 1] << 4)
		| (plat_cga_mono_avg[v1 + 1] << 6) )
	]
end

local function colorDistSq(rgb1, rgb2)
  if rgb1 == rgb2 then return 0 end

  local r1 = (rgb1 >> 16) & 0xFF
  local g1 = (rgb1 >> 8) & 0xFF
  local b1 = (rgb1) & 0xFF
  local r2 = (rgb2 >> 16) & 0xFF
  local g2 = (rgb2 >> 8) & 0xFF
  local b2 = (rgb2) & 0xFF
  local rs = (r1 - r2) * (r1 - r2)
  local gs = (g1 - g2) * (g1 - g2)
  local bs = (b1 - b2) * (b1 - b2)
  local rAvg = math.floor((r1 + r2) / 2)

  return (((512 + rAvg) * rs) >> 8) + (4 * gs) + (((767 - rAvg) * bs) >> 8)
end

local function round(v)
	return math.floor(v + 0.5)
end

local function getOCPalEntry(rgb)
	local r = (rgb >> 16) & 0xFF
	local g = (rgb >> 8) & 0xFF
	local b = (rgb) & 0xFF
	if r == g and g == b then
		local i = round(g * 16.0 / 255.0)
		if i <= 0 then return 16 elseif i >= 16 then return 255 else return i - 1 end
	else
		return 16 + (round(r * 5.0 / 255.0) * 40) + (round(g * 7.0 / 255.0) * 5) + round(b * 4.0 / 255.0)
	end
end

-- p8 = bottom right, p7 = bottom left, ...
-- returns: bg, fg, chr
local AVG_CHR_2x4 = 0
local AVG_CHR_1x4 = 1
local AVG_CHR_2x2 = 2

local function plat_avg_chr(pixels, pal, avg_chr_mode, empty_color)
	-- identify most common colors
	local pixelCount = {}
	local pixelColors = 0
	for i=1,#pixels do
		if pixelCount[pixels[i]] == nil then pixelColors = pixelColors + 1 end
		pixelCount[pixels[i]] = (pixelCount[pixels[i]] or 0) + 1
	end
	local bg, fg
	if pixelColors == 1 then
		return pixels[1], empty_color or 0, 0
	elseif pixelColors == 2 then
		bg = pixels[1]
		fg = -1
		for k,v in pairs(pixelCount) do
			if k ~= bg then
				fg = k
				break
			end
		end
	else
		local bgc = -1
		local fgc = -1
		for k,v in pairs(pixelCount) do
			if v > bgc then
				bg = k
				bgc = v
			end
		end
		for k,v in pairs(pixelCount) do
			if k ~= bg then
				local contrast = colorDistSq(pal[bg], pal[k]) * v
				if contrast > fgc then
					fg = k
					fgc = contrast
				end
			end
		end
	end
	local chr = 0
	local pbg = pal[bg]
	local pfg = pal[fg]
	if avg_chr_mode == AVG_CHR_2x4 then
		for i=1,8 do
			local p = pal[pixels[i]]
			if colorDistSq(pfg, p) < colorDistSq(pbg, p) then
				chr = chr | (0x80 >> (i - 1))
			end
       		end
	elseif avg_chr_mode == AVG_CHR_1x4 then
		for i=1,4 do
			local p = pal[pixels[i]]
			if colorDistSq(pfg, p) < colorDistSq(pbg, p) then
				chr = chr | (0xC0 >> ((i - 1) << 1))
			end
       		end
	elseif avg_chr_mode == AVG_CHR_2x2 then
		local ash = {0, 1, 4, 5}
		for i=1,4 do
			local p = pal[pixels[i]]
			if colorDistSq(pfg, p) < colorDistSq(pbg, p) then
				chr = chr | (0xA0 >> ash[i])
			end
       		end
	end
	return bg, fg, chr
end

local function iter_dstrings(dlines, dstrings, func)
	for y,dline in pairs(dlines) do
		local lastAtr = -1
		local lastX = dline[1]
		local str = {}
		for x=dline[1],dline[2] do
			local atr, chr = func(x, y)
			if lastAtr ~= atr then
				if #str > 0 then
					dstrings_add(dstrings, lastAtr, lastX + 1, y + 1, str)
				end

				lastAtr = atr
				lastX = x
				str = {chr}
			else
				str[#str + 1] = chr
			end
		end
		if #str > 0 then
			dstrings_add(dstrings, lastAtr, lastX + 1, y + 1, str)
		end
	end
end

local function dlines_scale(dlines, amt)
	for y,dline in pairs(dlines) do
		dline[1] = dline[1]*amt
		dline[2] = dline[2]*amt+(amt-1)
	end
end

function platform_render_mcga_13h(vram, addr)
	gpu_set_res_diff(160, 50)
	local dlines = video_pop_dirty_lines()
	local dstrings = {}

	iter_dstrings(dlines, dstrings, function(x, y)
		local base = addr + y*1280
		local bg, fg, chr = plat_avg_chr({
			getOCPalEntry(video_vga_get_palette(vram[base + x*2] or 0)),
			getOCPalEntry(video_vga_get_palette(vram[base + x*2 + 1] or 0)),
			getOCPalEntry(video_vga_get_palette(vram[base + x*2 + 320] or 0)),
			getOCPalEntry(video_vga_get_palette(vram[base + x*2 + 321] or 0)),
			getOCPalEntry(video_vga_get_palette(vram[base + x*2 + 640] or 0)),
			getOCPalEntry(video_vga_get_palette(vram[base + x*2 + 641] or 0)),
			getOCPalEntry(video_vga_get_palette(vram[base + x*2 + 960] or 0)),
			getOCPalEntry(video_vga_get_palette(vram[base + x*2 + 961] or 0))
		}, oc_palette, AVG_CHR_2x4)
		local atr = (bg << 8) | fg
		return atr, qdr[1 + chr]
	end)

	dstrings_draw(dstrings, true)
end

function platform_render_pcjr_160(vram, addr)
	gpu_set_res_diff(160, 50)
	local dlines = video_pop_dirty_lines()
	local dstrings = {}

	dlines_scale(dlines, 2)
	iter_dstrings(dlines, dstrings, function(x, y)
		local base = addr + x + y*160 -- 4/2 lines
		local shift = 4
		if (x & 1) == 1 then shift = 0 end
		local bg, fg, chr = plat_avg_chr({
			(((vram[base] or 0) >> shift) & 0x0F) + 1,
			(((vram[base + 0x2000] or 0) >> shift) & 0x0F) + 1,
			(((vram[base + 80] or 0) >> shift) & 0x0F) + 1,
			(((vram[base + 80 + 0x2000] or 0) >> shift) & 0x0F) + 1
		}, pc_16_colors, AVG_CHR_1x4, 1)
		local atr = ((bg - 1) << 4) | (fg - 1)
		return atr, qdr[1 + chr]
	end)

	dstrings_draw(dstrings, false)
end

function platform_render_pcjr_320(vram, addr)
	gpu_set_res_diff(160, 50)
	local dlines = video_pop_dirty_lines()
	local dstrings = {}

	iter_dstrings(dlines, dstrings, function(x, y)
		local base = addr + x + y*160 -- 4/4 lines
		local bg, fg, chr = plat_avg_chr({
			(((vram[base] or 0) >> 4) & 0x0F) + 1,
			(((vram[base] or 0)     ) & 0x0F) + 1,
			(((vram[base+0x2000] or 0) >> 4) & 0x0F) + 1,
			(((vram[base+0x2000] or 0)     ) & 0x0F) + 1,
			(((vram[base+0x4000] or 0) >> 4) & 0x0F) + 1,
			(((vram[base+0x4000] or 0)     ) & 0x0F) + 1,
			(((vram[base+0x6000] or 0) >> 4) & 0x0F) + 1,
			(((vram[base+0x6000] or 0)     ) & 0x0F) + 1
		}, pc_16_colors, AVG_CHR_2x4, 1)
		local atr = ((bg - 1) << 4) | (fg - 1)
		return atr, qdr[1 + chr]
	end)

	dstrings_draw(dstrings, false)
end

local cga_col_pals = {}
cga_col_pals[0] = {0, 11, 13, 15}
cga_col_pals[1] = {0, 10, 12, 14}
cga_col_pals[2] = {0, 11, 12, 15}
cga_col_pals[3] = {0, 3, 5, 7}
cga_col_pals[4] = {0, 2, 4, 6}
cga_col_pals[5] = {0, 3, 4, 7}

local function plat_avg_chr_cga(c1, c2, c3, c4, sh, cgp)
	local cga_col_pal = cga_col_pals[cgp]
	local bg, fg, ch = plat_avg_chr({
		cga_col_pal[((c1 >> (sh+2)) & 3) + 1] + 1,
		cga_col_pal[((c1 >> sh) & 3) + 1] + 1,
		cga_col_pal[((c2 >> (sh+2)) & 3) + 1] + 1,
		cga_col_pal[((c2 >> sh) & 3) + 1] + 1,
		cga_col_pal[((c3 >> (sh+2)) & 3) + 1] + 1,
		cga_col_pal[((c3 >> sh) & 3) + 1] + 1,
		cga_col_pal[((c4 >> (sh+2)) & 3) + 1] + 1,
		cga_col_pal[((c4 >> sh) & 3) + 1] + 1
	}, pc_16_colors, AVG_CHR_2x4, 1)
	return bg - 1, fg - 1, ch
end

function platform_render_cga_color(vram, addr, mode)
	gpu_set_res_diff(160, 50)
	local dlines = video_pop_dirty_lines()
	local dstrings = {}

	local cgp = (((cga_get_palette() >> 4) & 1) * 3)
	if mode == 5 then
		cgp = cgp + 2
	else
		cgp = cgp + (cga_get_palette() >> 5) & 1
	end
--	cga_col_pals[cgp][1] = (cga_get_palette() & 0x0F)

	dlines_scale(dlines, 2)
	iter_dstrings(dlines, dstrings, function(x, y)
		local base = addr + y*160
		local xs = x >> 1
		local bg, fg, chr = plat_avg_chr_cga(
			vram[base + xs] or 0,
			vram[base + xs + 0x2000] or 0,
			vram[base + xs + 80] or 0,
			vram[base + xs + 0x2000 + 80] or 0,
			(4 - ((x & 1) * 4)), cgp
		)
		local atr = (bg << 4) | fg
		return atr, qdr[1 + chr]
	end)

	dstrings_draw(dstrings, false)
end

function platform_render_cga_mono(vram, addr)
	gpu_set_res_diff(160, 50)
	gpu.setBackground(pc_16_colors[1])
	gpu.setForeground(pc_16_colors[16])
--	gpu.setForeground(pc_16_colors[(cga_get_palette() & 0x0F) + 1])

	local dlines = video_pop_dirty_lines()
	for y,dline in pairs(dlines) do
		local str = {}
		-- y = 0..49 -> 0, 4, ... -> jump by two odd scanlines (160 bytes)
		local base = addr + y*160
		for x=dline[1],dline[2] do
			-- left
			local c1 = vram[base + x] or 0
			local c2 = vram[base + x + 0x2000] or 0
			local c3 = vram[base + x + 80] or 0
			local c4 = vram[base + x + 0x2000 + 80] or 0
			str[#str + 1] = plat_cga_chr(c1 >> 4, c2 >> 4, c3 >> 4, c4 >> 4)
			str[#str + 1] = plat_cga_chr(c1 & 0x0F, c2 & 0x0F, c3 & 0x0F, c4 & 0x0F)	
		end
		gpu.set(dline[1] * 2 + 1, y + 1, table.concat(str))
	end
end

function platform_render_text(vram, addr, width, height, pitch)
	gpu_set_res_diff(width, height)

	local dlines = video_pop_dirty_lines()
	local dstrings = {}

	for y,dline in pairs(dlines) do
		local base = addr + (y * pitch)
		local lastAtr = -1
		local lastX = 0
		local str = {}
		if dline[2] >= width then dline[2] = width-1 end
		for x=dline[1],dline[2] do
			local chr = cp437_trans[vram[base + x*2] or 0]
			local atr = vram[base + x*2 + 1] or 0
			-- TO TASTE
			if chr == 0x2593 then
				chr = 0x2591
				atr = (atr >> 4) | ((atr << 4) & 0xF0)
			end

			if lastAtr ~= atr then
				if #str > 0 then
					dstrings_add(dstrings, lastAtr, lastX + 1, y + 1, str)
				end

				lastAtr = atr
				lastX = x
				str = {unicode.char(chr)}
			else
				str[#str + 1] = unicode.char(chr)
			end
		end
		if #str > 0 then
			dstrings_add(dstrings, lastAtr, lastX + 1, y + 1, str)
		end
	end

	dstrings_draw(dstrings)
end

function platform_finish()
end

dofile("emu_core.lua")
