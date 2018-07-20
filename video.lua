-- SUPPORTED MODES:
-- CGA: 00h, 01h, 02h, 03h, 04h, 05h, 06h
-- PCjr: 08h, 09h
-- VGA: 12h (pretend; just runs 13h code, used to not upset certain MCGA software)
-- MCGA: 13h (shaky; 0x3D** ports only supported for palette)

local video_mode = 3

pc_16_colors = {
	0x000000, 0x0000AA, 0x00AA00, 0x00AAAA,
	0xAA0000, 0xAA00AA, 0xAA5500, 0xAAAAAA,
	0x555555, 0x5555FF, 0x55FF55, 0x55FFFF,
	0xFF5555, 0xFF55FF, 0xFFFF55, 0xFFFFFF
}

RAM[0x450] = 0 -- cursor pos
RAM[0x451] = 0 
RAM[0x462] = 0 -- current page
RAM:w16(0x463, 0x3D4) -- base IO port for video

local vram = {}
local dirty_lines = {}

function video_pop_dirty_lines()
	local d = dirty_lines
	dirty_lines = {}
	return d
end

function video_vram_read(addr)
	return vram[addr - 0x9FFFF] or 0
end

function video_vram_write(addr, val)
	if vram[addr - 0x9FFFF] == val then return end
	vram[addr - 0x9FFFF] = val

	local l = nil
	local lx = nil

	if (video_mode >= 0 and video_mode <= 3) or video_mode == 7 then
		if addr >= 0xB8000 and addr < (0xB8000 + (25 * 160)) then
			l = math.floor((addr - 0xB8000) / 160)
			lx = math.floor((addr - 0xB8000) % 160) >> 1
		end
	elseif ((video_mode >= 4 and video_mode <= 6) and (addr >= 0xB8000 and addr < 0xBC000))
		or (video_mode == 8 and (addr >= 0xB0000 and addr < 0xB4000)) then

		if ((addr & 0x1FFF) < 8000) then
			-- two banks, 100 lines each, so 80 bytes per line
			l = (math.floor((addr & 0x1FFF) / 80)) >> 1 -- 0..99 div 2
			lx = math.floor((addr & 0x1FFF) % 80)
		end
	elseif video_mode == 9 and (addr >= 0xB0000 and addr < 0xB8000) then

		if ((addr & 0x1FFF) < 8000) then
			-- four banks! 50 lines each, so 160 bytes per line
			l = (math.floor((addr & 0x1FFF) / 160))
			lx = math.floor((addr & 0x1FFF) % 160)
		end
	elseif video_mode >= 0x12 and video_mode <= 0x13 then
		if (addr >= 0xA0000 and addr < (0xA0000 + 64000)) then
			-- woo, no interleaving on this one
			lx = math.floor(((addr - 0xA0000) >> 1) % 160) -- 0..159
			l = math.floor((addr - 0xA0000) / 1280) -- 0..49
		end
	end

	if l then
		if not dirty_lines[l] then
			dirty_lines[l] = {lx, lx}
		else
			local ld = dirty_lines[l]
			if lx < ld[1] then ld[1] = lx
			elseif lx > ld[2] then ld[2] = lx end
		end
	end
end

function video_text_addr(x, y)
	return 0xB8000 + (y * 160) + (x * 2)
end

function video_scroll_up(lines, empty_attr, y1, x1, y2, x2)
	if lines == 0 then
		for y=y1,y2 do
		for x=x1,x2 do
			RAM:w16(video_text_addr(x,y), empty_attr << 8)
		end
		end
	else
		for y=y1+lines,y2 do
		for x=x1,x2 do
			RAM:w16(video_text_addr(x,y-lines), RAM:r16(video_text_addr(x,y)))
		end
		end
		for y=y2-lines+1,y2 do
		for x=x1,x2 do
			RAM:w16(video_text_addr(x,y), empty_attr << 8)
		end
		end
	end
end

function video_scroll_down(lines, empty_attr, y1, x1, y2, x2)
	if lines == 0 then
		for y=y1,y2 do
		for x=x1,x2 do
			RAM:w16(video_text_addr(x,y), empty_attr << 8)
		end
		end
	else
		for y=y2-lines,y1,-1 do
		for x=x1,x2 do
			RAM:w16(video_text_addr(x,y+lines), RAM:r16(video_text_addr(x,y)))
		end
		end
		for y=y1,y1+lines-1 do
		for x=x1,x2 do
			RAM:w16(video_text_addr(x,y), empty_attr << 8)
		end
		end
	end
end

local cga_mode = 0x08
local cga_palette = 0x30
local cga_status = 0x09

function cga_get_palette()
	return cga_palette
end

--local herc_status = 0
local herc_status = 0xFF

function video_update()
--	herc_status = herc_status ~ 0x80
	if video_mode == 0 or video_mode == 1 then
		platform_render_text(vram, 0xB8000 - 0x9FFFF, 40, 25, 160)
	elseif video_mode == 2 or video_mode == 3 or video_mode == 7 then
		platform_render_text(vram, 0xB8000 - 0x9FFFF, 80, 25, 160)
	elseif video_mode >= 4 and video_mode <= 6 then
		if video_mode < 6 and platform_render_cga_color then
			platform_render_cga_color(vram, 0xB8000 - 0x9FFFF, video_mode)
		else
			platform_render_cga_mono(vram, 0xB8000 - 0x9FFFF)
		end
	elseif video_mode == 0x08 then
		platform_render_pcjr_160(vram, 0xB0000 - 0x9FFFF)
	elseif video_mode == 0x09 then
		platform_render_pcjr_320(vram, 0xB0000 - 0x9FFFF)
	elseif video_mode == 0x0A then
		platform_render_pcjr_640(vram, 0xB0000 - 0x9FFFF)
	elseif video_mode == 0x13 or video_mode == 0x12 then
		platform_render_mcga_13h(vram, 0xA0000 - 0x9FFFF)
	end
end

local cga_mode_mask = 0x17 -- 0b10111
local cga_reg_to_mode = {}
cga_reg_to_mode[0] = 0x04 -- 40x25 mono
cga_reg_to_mode[1] = 0x00 -- 40x25 color
cga_reg_to_mode[2] = 0x05 -- 80x25 mono
cga_reg_to_mode[3] = 0x01 -- 80x25 color
cga_reg_to_mode[4] = 0x02 -- 320x200 graphics
cga_reg_to_mode[5] = 0x06 -- 320x200 alt graphics
cga_reg_to_mode[6] = 0x16 -- 640x200 graphics

-- Hercules ports
cpu_port_set(0x3BA, function(cond, val)
	if not val then
		emu_debug(2, "polling hercules status")
		return herc_status
	end
end)

-- CGA ports
cpu_port_set(0x3D8, function(cond, val)
	if not val then return cga_mode else
		cga_mode = val & 0x3F
		for i=0,6 do
			if cga_reg_to_mode[i] == (cga_mode & cga_mode_mask) then
				-- TODO: set mode from here
			end
		end
		RAM[0x465] = cga_mode
	end
end)
cpu_port_set(0x3D9, function(cond, val)
	if not val then return cga_palette else
		cga_palette = val & 0x3F
		RAM[0x466] = cga_palette
	end
end)
cpu_port_set(0x3DA, function(cond, val)
	if not val then
		cga_status = cga_status ~ 0x09
		return cga_status
	end
end)
cpu_port_set(0x3DF, function(cond, val)
	if not val then return 0 end
end)

function video_set_mode(vmode, clr)
	local mode_supported = false
	if (vmode >= 4 and vmode <= 6) and platform_render_cga_mono then
		mode_supported = true
	elseif vmode == 0x08 and platform_render_pcjr_160 then
		mode_supported = true
	elseif vmode == 0x09 and platform_render_pcjr_320 then
		mode_supported = true
	elseif vmode == 0x0A and platform_render_pcjr_640 then
		mode_supported = true
	elseif (vmode == 0x13 or vmode == 0x12) and platform_render_mcga_13h then
		mode_supported = true
	elseif vmode >= 0 and vmode <= 3 then
		mode_supported = true
	end
	if not mode_supported then
		return false
	end

	video_mode = vmode

	cga_mode = 0x08
	if vmode >= 0 and vmode <= 6 then
		cga_mode = cga_mode | cga_reg_to_mode[vmode]
	end
	cga_status = 0x09

	-- 0x449 - byte, video mode
	RAM[0x449] = video_mode

	-- 0x44A - word, text width
	if video_mode == 7 or (cga_mode & 1) == 1 then
		RAM[0x44A] = 80
	else
		RAM[0x44A] = 40
	end
	RAM[0x44B] = 0
	RAM[0x465] = cga_mode
	RAM[0x466] = cga_palette

	if clr then
		if (vmode >= 0 and vmode <= 3) or vmode == 7 then
			for y=0,24 do
			for x=0,RAM[0x44A]-1 do
				RAM:w16(video_text_addr(x,y), 0x0700)
			end
			end
		else
			for i=0,7999 do
				RAM[0xB8000 + i] = 0
				RAM[0xBA000 + i] = 0
			end
		end
	end

	return true
end

video_set_mode(3, true)

function video_get_cursor(page)
	if page == nil then
		page = (RAM[0x462] & 7) << 1
	end
	return RAM[0x450+page], RAM[0x451+page]
end

function video_set_cursor(x, y, page)
	if page == nil then
		page = (RAM[0x462] & 7) << 1
	end
	RAM[0x450+page] = x
	RAM[0x451+page] = y
end	

local vgapal={}

function video_vga_get_palette(i)
	return vgapal[1+i] or 0xFF00FF
end

function video_vga_get_palette_orig(i)
	-- just wing it this time, should be good enough
	return (video_vga_get_palette(i) >> 2) & 0x3F3F3F
end

function video_vga_set_palette(i,r,g,b)
	local rs = math.floor(r * 255 / 63.0) & 0xFF
	local gs = math.floor(g * 255 / 63.0) & 0xFF
	local bs = math.floor(b * 255 / 63.0) & 0xFF
	vgapal[1+i] = (rs<<16)|(gs<<8)|bs

	if video_mode >= 0x10 then
		for i=0,49 do dirty_lines[i] = {0,159} end
	end
end

for i=1,16 do
	vgapal[i] = pc_16_colors[i]
end

-- VGA DAC
local dac_pal_idx = 0
local dac_write = true

cpu_port_set(0x3C7, function(cond, val)
	if not val then return 0 else
		dac_pal_idx = (val & 0xFF) * 3
		dac_write = false
	end
end)
cpu_port_set(0x3C8, function(cond, val)
	if not val then return 0 else
		dac_pal_idx = (val & 0xFF) * 3
		dac_write = true
	end
end)
cpu_port_set(0x3C9, function(cond, val)
	local shift = (8*(2 - math.floor(dac_pal_idx % 3)))
	local idx = math.floor(dac_pal_idx / 3)
	local mask = 0xFF << shift
	if not val and not dac_write then
		dac_pal_idx = math.floor((dac_pal_idx + 1) % 768)
		return (video_vga_get_palette_orig(idx) >> (shift)) & 0xFF
	elseif val and dac_write then
		dac_pal_idx = math.floor((dac_pal_idx + 1) % 768)
		local pal = video_vga_get_palette_orig(idx) & (mask ~ 0xFFFFFF)
		pal = pal | ((val & 0xFF) << shift)
		video_vga_set_palette(idx, (pal >> 16) & 0xFF, (pal >> 8) & 0xFF, pal & 0xFF)
	elseif not val then return 0 end
end)

-- interrupt	
cpu_register_interrupt_handler(0x10, function(ax,ah,al)
	if ah == 0x00 then
		-- set video mode
		local mode = al & 0xFF7F
		emu_debug(1, "setting mode to " .. mode)
		video_set_mode(mode, (al & 0x80) == 0)
		return true
	elseif ah == 0x01 then
		-- set cursor shape, TODO
		return true
	elseif ah == 0x02 then
		-- set cursor pos
		video_set_cursor(CPU["regs"][3] & 0xFF, CPU["regs"][3] >> 8, CPU["regs"][4] >> 8)
		return true
	elseif ah == 0x03 then
		-- get cursor_pos
		local cursor_x, cursor_y = video_get_cursor(CPU["regs"][4] >> 8)
		CPU["regs"][3] = (cursor_y << 8) | (cursor_x)
		return true
	elseif ah == 0x04 then
		-- query light pen
		CPU["regs"][1] = al -- AH=0, not triggered
		return true
	elseif ah == 0x05 then
		-- select video page
		RAM[0x462] = al & 0x07
		return true
	elseif ah == 0x06 then
		-- scroll up
		video_scroll_up(al, (CPU["regs"][4] >> 8),
			(CPU["regs"][2] >> 8), (CPU["regs"][2] & 0xFF), 
			(CPU["regs"][3] >> 8), (CPU["regs"][3] & 0xFF));
		return true
	elseif ah == 0x07 then
		-- scroll down
		video_scroll_down(al, (CPU["regs"][4] >> 8),
			(CPU["regs"][2] >> 8), (CPU["regs"][2] & 0xFF), 
			(CPU["regs"][3] >> 8), (CPU["regs"][3] & 0xFF));
		return true
	elseif ah == 0x08 then
		-- read character
		local cursor_x, cursor_y = video_get_cursor(CPU["regs"][4] >> 8)
		local addr = video_text_addr(cursor_x, cursor_y)
		CPU["regs"][1] = 0x0800 | RAM[addr]
		CPU["regs"][4] = (CPU["regs"][4] & 0xFF) | (RAM[addr + 1] << 8)
		return true
	elseif ah == 0x09 or ah == 0x0A then
		-- write character/attribute (0x09) or char (0x0A)
		local cursor_x, cursor_y = video_get_cursor(CPU["regs"][4] >> 8)
		local addr = video_text_addr(cursor_x, cursor_y)
		local bl = CPU["regs"][4] & 0xFF
		for i=1,CPU["regs"][2] do
			if addr < (160*25) then
				RAM[addr] = al
				if ah == 0x09 then
					RAM[addr + 1] = bl
				end
				addr = addr + 2
			end
		end
		return true
	elseif ah == 0x0B then
		-- configure video
		local p = cga_palette
		local bh = CPU["regs"][4] >> 8
		local bl = CPU["regs"][4] & 0xFF
		if bh == 0x00 then
			-- TODO: set border color
		elseif bh == 0x01 then
			-- set palette
			p = (p & 0xDF) | ((bl & 0x01) << 5)
		end
		cga_palette = p
		RAM[0x466] = p
		return true
	elseif ah == 0x0E then
		-- put char like a tty
		local cursor_x, cursor_y = video_get_cursor()
		local addr = video_text_addr(cursor_x, cursor_y)
		local cursor_width = RAM[0x44A]
		local cursor_height = 25
		if al == 0x0D then
			cursor_x = 0
		elseif al == 0x0A then
			if cursor_y < (cursor_height - 1) then
				cursor_y = cursor_y + 1
			else
				video_scroll_up(1, 0x07, 0, 0, cursor_height - 1, cursor_width - 1)
			end
		elseif al == 0x08 then
			if cursor_x > 0 then
				cursor_x = cursor_x - 1
			end
			RAM[video_text_addr(cursor_x, cursor_y)] = 0
		elseif al == 0x07 then
			-- bell
		else
			RAM[addr] = al
			cursor_x = cursor_x + 1
			if cursor_x >= cursor_width then
				cursor_x = 0
				if cursor_y < (cursor_height - 1) then
					cursor_y = cursor_y + 1
				else
					video_scroll_up(1, 0x07, 0, 0, cursor_height - 1, cursor_width - 1)
				end
			end
		end
		video_set_cursor(cursor_x, cursor_y)
		return true
	elseif ah == 0x0F then
		-- read video mode
		local ah = RAM[0x44A]
		local al = video_mode
		local bh = RAM[0x462]
		CPU["regs"][1] = (ah << 8) | (al)
		CPU["regs"][4] = (CPU["regs"][4] & 0xFF) | (bh << 8)
		return true
	elseif ax == 0x1010 then
		-- set single palette register
		-- BX=i, CH=g, CL=b, DH=r
		video_vga_set_palette(CPU["regs"][4] & 0xFF, CPU["regs"][3]>>8, CPU["regs"][2]>>8, CPU["regs"][2]&0xFF)
		return true
	elseif ax == 0x1012 then
		-- set palette block, BX = start, CX = count, ES:DX = offset
		local addrIn = seg(SEG_ES,CPU["regs"][3])
		local addrOut = CPU["regs"][4] - 1
		for i=1,CPU["regs"][2] do
			video_vga_set_palette((addrOut + i) & 0xFF, RAM[addrIn], RAM[addrIn + 1], RAM[addrIn + 2])
			addrIn = addrIn + 3
		end
		return true
	elseif ax == 0x1015 then
		-- read single palette register
		local col = video_vga_get_palette_orig(CPU["regs"][4] & 0xFF)
		CPU["regs"][2] = col & 0xFFFF
		CPU["regs"][3] = ((col >> 8) & 0xFF00) | (CPU["regs"][3] & 0xFF)
		return true
	elseif ax == 0x1017 then
		-- read palette block
		local addrIn = seg(SEG_ES,CPU["regs"][3])
		local addrOut = CPU["regs"][4] - 1
		for i=1,CPU["regs"][2] do
			local col = video_vga_get_palette_orig((addrOut + i) & 0xFF)
			RAM[addrIn] = (col >> 16) & 0xFF
			RAM[addrIn + 1] = (col >> 8) & 0xFF
			RAM[addrIn + 2] = col & 0xFF
			addrIn = addrIn + 3
		end
		return true
	elseif ax == 0x1130 then
		-- get font information
		local bh = CPU["regs"][4] >> 8
		if bh == 0 then
			-- set ES:BP
			CPU["regs"][6] = RAM:r16(0x1F * 4)
			CPU["segments"][1] = RAM:r16(0x1F * 4 + 2)
		elseif bh == 1 then
			CPU["regs"][6] = RAM:r16(0x43 * 4)
			CPU["segments"][1] = RAM:r16(0x43 * 4 + 2)
		else
			CPU["regs"][6] = 0x0800
			CPU["segments"][1] = 0xF000
		end
		CPU["regs"][2] = 0x08
		CPU["regs"][3] = (CPU["regs"][3] & 0xFF00) | RAM[0x484]
		return true
	elseif ax == 0x1A00 then
		-- get display combination code
		CPU["regs"][1] = 0x1A1A
		CPU["regs"][4] = 0x020A
		return true
	else
		return false
	end
end)

-- CRTC ports

local crtc_index = 0
cpu_port_set(0x3D4, function(cond, val)
	if not val then return crtc_index
	elseif val >= 0 and val <= 0x11 then crtc_index = val end
end)

local cga_cursor = 0
cpu_port_set(0x3D5, function(cond, val)
	if val then -- writes
		if crtc_index == 0x0E then
			cga_cursor = (cga_cursor & 0xFF) | (val & 0xFF)
		elseif crtc_index == 0x0F then
			cga_cursor = (cga_cursor & 0xFF00) | (val & 0xFF)
		end
	else -- reads
		if crtc_index == 0x0E then
			return (cga_cursor >> 8)
		elseif crtc_index == 0x0F then
			return (cga_cursor & 0xFF)
		else
			return 0
		end
	end
end)

-- mirroring
for i=0,6,2 do
	cpu_port_set(0x3B0 + i, cpu_port_get(0x3D4))
	cpu_port_set(0x3D0 + i, cpu_port_get(0x3D4))
	cpu_port_set(0x3B1 + i, cpu_port_get(0x3D5))
	cpu_port_set(0x3D1 + i, cpu_port_get(0x3D5))
end
