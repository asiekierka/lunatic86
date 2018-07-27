local drives = {}

RAM[0x475] = 0

local function disk_init_data(fptr, d)
	local fsize = 0
	if fptr ~= nil then
		fsize = fptr:seek("end")
		fptr:seek("set", 0)
	end

	emu_debug(2, string.format("fsize %02X == %d", d.id, fsize))
	d.inserted = true
	if fsize == 0 then
		d.inserted = false
		d.sector_size = 0
		d.sectors = 0
		d.heads = 0
		d.cylinders = 0
	elseif d.floppy then
		d.sector_size = 512
		if fsize == 2949120 then
			d.sectors = 36
			d.heads = 2
			d.cylinders = 80
		elseif fsize == 1761280 then
			d.sectors = 21
			d.heads = 2
			d.cylinders = 82
		elseif fsize == 1720320 then
			d.sectors = 21
			d.heads = 2
			d.cylinders = 80
		elseif fsize == 1474560 then
			d.sectors = 18
			d.heads = 2
			d.cylinders = 80
		elseif fsize == 1228800 then
			d.sectors = 15
			d.heads = 2
			d.cylinders = 80
		elseif fsize == 737280 then
			d.sectors = 9
			d.heads = 2
			d.cylinders = 80
		elseif fsize == (360*1024) then
			d.sectors = 9
			d.heads = 2
			d.cylinders = 40
		elseif fsize == (320*1024) then
			d.sectors = 8
			d.heads = 2
			d.cylinders = 40
		elseif fsize == (180*1024) then
			d.sectors = 9
			d.heads = 1
			d.cylinders = 40
		elseif fsize == (160*1024) then
			d.sectors = 8
			d.heads = 1
			d.cylinders = 40
		else
			error("unknown fsize: " .. fsize)
		end
	else
		d.sector_size = 512
		d.sectors = 63
		d.heads = 16
		d.cylinders = math.floor(fsize / (d.sector_size*d.sectors*d.heads))
		if d.cylinders <= 0 then
			error("unknown fsize: " .. fsize)
		end
	end
	emu_debug(2, string.format("disks: added %d x %d x %d x %d @ %02X", d.cylinders, d.heads, d.sectors, d.sector_size, d.id))

	-- configure table
	local tba = 0xF2000 + d.id*16
	if d.id == 0x80 then
		RAM:w16(0x0104, tba & 0xFFFF)
		RAM:w16(0x0106, 0xF000)
	elseif d.id == 0x81 then
		RAM:w16(0x0118, tba & 0xFFFF)
		RAM:w16(0x011A, 0xF000)
	elseif d.id == 0x00 then
		RAM:w16(0x0078, tba & 0xFFFF)
		RAM:w16(0x007A, 0xF000)
	end
	if d.floppy then
		RAM[tba] = 0xF0
		RAM[tba + 1] = 0x00
		RAM[tba + 2] = 0x00
		RAM[tba + 3] = math.ceil(d.sector_size / 128) - 1
		RAM[tba + 4] = d.sectors
		RAM[tba + 5] = 0
		RAM[tba + 6] = 0
		RAM[tba + 7] = 0
		RAM[tba + 8] = 0xF6
		RAM[tba + 9] = 0
		RAM[tba + 10] = 0
	else
		RAM:w16(tba, d.cylinders)
		RAM[tba + 2] = d.heads
		RAM:w16(tba + 3, 0)
		RAM:w16(tba + 5, 0)
		RAM[tba + 7] = 0
		RAM[tba + 8] = 0xC0
		if d.heads > 8 then
			RAM[tba + 8] = RAM[tba + 8] | 0x08
		end
		RAM[tba + 9] = 0
		RAM[tba + 10] = 0
		RAM[tba + 11] = 0
		RAM:w16(tba + 12, 0)
		RAM[tba + 14] = d.sectors
	end
end

function disk_init(fn, id)
	local d = {}
	d.id = id
	local is_floppy = id < 0x80
	d.floppy = is_floppy

	if type(fn) == "function" then
		d.ptr = fn
	else
		local ptrmode = nil
		local f = nil
		d.ptr = function(a, mode)
			if ptrmode ~= mode then
				if f ~= nil then f:close() end
				f = io.open(fn, mode)
				disk_init_data(f, a)
				f:seek("set", 0)
				ptrmode = mode
			end
			return f
		end
	end

	drives[id] = d
	if d.id >= 0x80 then
		RAM[0x475] = RAM[0x475] + 1
	end
end

function disk_has(id)
	return drives[id] ~= nil
end

local last_status = 0x00

local function ret_status(v)
	if v ~= nil then
		emu_debug(2, "disks: setting status to " .. v)
		last_status = v & 0xFF
	end
	CPU["regs"][1] = (CPU["regs"][1] & 0xFF) | (last_status << 8)
	cpu_write_flag(0, last_status ~= 0)
end

cpu_register_interrupt_handler(0x13, function(ax,ah,al)
	if ah == 0x00 then
		emu_debug(1, "disks: disk system reset")
		ret_status(0)
		return true
	elseif ah == 0x01 then
		ret_status(nil)
		return true
	elseif ah == 0x02 then
		local cx = CPU["regs"][2]
		local dx = CPU["regs"][3]
		local bx = CPU["regs"][4]
		local sector = (cx & 0x3F)
		local cylinder = (cx >> 8)
		local head = (dx >> 8)
		local drive = (dx & 0xFF)
		if drive >= 0x80 then
			cylinder = cylinder | ((cx & 0xC0) << 2)
		end
		local d = drives[drive]
		if not d or d:ptr("rb") == nil then
			ret_status(1)
			return true
		else
			d:ptr("rb")
			if sector == 0 or sector > d.sectors or head >= d.heads or cylinder >= d.cylinders then
				ret_status(4)
				emu_debug(1, string.format("disks: out of bounds - %02X c=%d h=%d s=%d", drive, cylinder, head, sector))
				return true
			end
		end
		local pos = cylinder
		pos = pos * d.heads + head
		pos = pos * d.sectors + (sector - 1)
		pos = pos * d.sector_size
		CPU["regs"][1] = al -- AH = 0 (OK), AL = sectors transferred
		d:ptr("rb"):seek("set", pos)
		local count = al * d.sector_size
		local data = d:ptr("rb"):read(count)
		for i=0,count-1 do
			RAM[seg(SEG_ES, bx + i)] = data:byte(i+1,i+2)
		end
		emu_debug(1, string.format("disks: read %d bytes from %02X %d to %04X %04X", count, drive, pos, CPU["segments"][SEG_ES+1], bx))
		ret_status(0)
		return true
	elseif ah == 0x03 then
		local cx = CPU["regs"][2]
		local dx = CPU["regs"][3]
		local bx = CPU["regs"][4]
		local sector = (cx & 0x3F)
		local cylinder = (cx >> 8)
		local head = (dx >> 8)
		local drive = (dx & 0xFF)
		if drive >= 0x80 then
			cylinder = cylinder | ((cx & 0xC0) << 2)
		end
		local d = drives[drive]
		if not d or d:ptr("ab") == nil then
			ret_status(1)
			return true
		else
			d:ptr("ab")
			if sector == 0 or sector > d.sectors or head >= d.heads or cylinder >= d.cylinders then
				ret_status(4)
				emu_debug(1, string.format("disks: out of bounds - %02X c=%d h=%d s=%d", drive, cylinder, head, sector))
				return true
			end
		end
		local pos = cylinder
		pos = pos * d.heads + head
		pos = pos * d.sectors + (sector - 1)
		pos = pos * d.sector_size
		CPU["regs"][1] = al -- AH = 0 (OK), AL = sectors transferred
		d:ptr("ab"):seek("set", pos)
		local count = al * d.sector_size
		for i=0,count-1 do
			d:ptr("ab"):write(string.char(RAM[seg(SEG_ES, bx + i)]))
		end
		emu_debug(1, string.format("disks: wrote %d bytes from %02X %d to %04X %04X", count, drive, pos, CPU["segments"][SEG_ES+1], bx))
		ret_status(0)
		return true
	elseif ah == 0x04 then -- verify
		local drive = CPU["regs"][3] & 0xFF
		emu_debug(1, string.format("disks: verify drive %02X", drive))
		ret_status(0)
		return true
	elseif ah == 0x08 then -- get drive parameters
		local drive = CPU["regs"][3] & 0xFF
		local d = drives[drive]
		if not d or d:ptr("rb") == nil then
			ret_status(1)
		else
			d:ptr("rb") -- init disk data
			local maxc = d.cylinders - 1
			local drives = RAM[0x475]
			if d.floppy then drives = ((RAM[0x410] >> 6) & 3) + 1 end
			CPU["regs"][2] = ((maxc & 0xFF) << 8) | (d.sectors & 0x3F) | ((maxc & 0x300) >> 2) -- CX = cylinder number | sector number
			CPU["regs"][3] = ((d.heads - 1) << 8) | drives
			CPU["regs"][8] = 2000 + (drive*16)
			CPU["segments"][SEG_ES+1] = 0xF000 -- ES:DI - hdpt ptr
			if d.floppy then
				if d.sectors == 18 then
					CPU["regs"][4] = (CPU["regs"][4] & 0xFF00) | 4
				else
					CPU["regs"][4] = (CPU["regs"][4] & 0xFF00) | 3
				end
			else
				CPU["regs"][4] = (CPU["regs"][4] & 0xFF00)
			end
			ret_status(0)
		end
		emu_debug(1, string.format("disks: get drive parameters %02X %04X", drive, CPU["regs"][1]))
		return true
	elseif ah == 0x15 then -- get disk type
		local drive = CPU["regs"][3] & 0xFF
		local d = drives[drive]
		local code = 0
		if d and d:ptr("rb") ~= nil then
			if d.floppy then code = 1
			else code = 3 end
		end
		cpu_clear_flag(0) -- clear carry
		CPU["regs"][1] = (code << 8) | (CPU["regs"][1] & 0xFF) -- AH = drive code
		emu_debug(1, string.format("disks: get disk type %02X", drive))
		return true
	elseif ah == 0x18 then -- set media type for format
		-- TODO
		local drive = CPU["regs"][3] & 0xFF
		local code = 0x80
		if drives[drive] then code = 0 end
		cpu_clear_flag(0) -- clear carry
		CPU["regs"][1] = (code << 8) | (CPU["regs"][1] & 0xFF) -- AH = drive code
		emu_debug(1, string.format("disks: set media type %02X", drive))			
		return true
	elseif ah == 0x41 then -- check extensions present
		ret_status(1)
		return true
	else
		cpu_set_flag(0)
		return false
	end
end)

function disk_boot(id)
	local f = drives[id]:ptr("rb")
	f:seek("set", 0)
	local bootsector = f:read(512)
	for i=0,511 do
		RAM[0x7c00 + i] = bootsector:byte(i+1,i+2)
	end
	cpu_set_ip(0x0000, 0x7C00)
	CPU["regs"][3] = 0x0000 | id
	CPU["regs"][5] = 0x8000
end
