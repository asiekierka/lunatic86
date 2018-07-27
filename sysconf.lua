-- machine info
cpu_register_interrupt_handler(0x11, function()
	-- equipment list
	CPU["regs"][1] = RAM:r16(0x410)
	return true
end)

cpu_register_interrupt_handler(0x12, function()
	-- get memory size
	CPU["regs"][1] = RAM:r16(0x413)
	return true
end)

cpu_register_interrupt_handler(0x15, function(ax,ah,al)
	emu_debug(1, string.format("unknown sysconf: %02X\n", ah))
	CPU["regs"][1] = 0x8600 | (CPU["regs"][1] & 0xFF) -- AH = 0x86 (function not supported)
	cpu_set_flag(0) -- set carry
	return true
end)

function sysconf_init()
	local equipment = 0x0061
	local fdd_count = 1
	for i=0,3 do
		if disk_has(i) and fdd_count == i then
			fdd_count = i + 1
		end
	end
	equipment = equipment | ((fdd_count - 1) << 6)

	RAM:w16(0x410, equipment) -- equipment list
	RAM:w16(0x413, 640) -- memory size
end

RAM[0xFFFFE] = 0xFB

local rdate = "07/06/98"
for i=1,#rdate do RAM[0xFFFF4 + i] = string.byte(rdate, i, i) end
