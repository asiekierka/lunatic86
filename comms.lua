cpu_register_interrupt_handler(0x14, function(ax,ah,al)
	-- serial
	if (ah == 0x00) or (ah == 0x03) then
		CPU["regs"][1] = 0x0000
		emu_debug(0, "serial: get port status/init\n")
		return true
	else
		cpu_set_flag(0)
	end
end)

cpu_register_interrupt_handler(0x17, function(ax,ah,al)
	-- printer
	if (ah == 0x01) or (ah == 0x02) then
		CPU["regs"][1] = CPU["regs"][1] & 0xFF
		emu_debug(0, "printer: get port status/init\n")
		return true
	else
		cpu_set_flag(0)
	end
end)
