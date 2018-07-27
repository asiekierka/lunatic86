local function get_timer_ticks()
	return RAM:r32(0x46c)
end

local function set_timer_ticks(v)
	return RAM:w32(0x46c, v)
end

local timer_last_midnight = 0
local timer_midnight = 24*60*60*20

cpu_register_interrupt_handler(0x08, function(ax,ah,al)
	set_timer_ticks(get_timer_ticks()+1)
	cpu_emit_interrupt(0x1C,false)
	return true
end)

cpu_register_interrupt_handler(0x1C, function(ax,ah,al)
	-- dummy - override by user
	return true
end)

cpu_register_interrupt_handler(0x1A, function(ax,ah,al)
	if (ah == 0x00) then
		local midnight = 0
		local timer_ticks = get_timer_ticks()
		while (timer_ticks - timer_midnight) >= timer_last_midnight do
			midnight = 1
			timer_last_midnight = timer_last_midnight + timer_midnight
		end
		CPU["regs"][1] = (CPU["regs"][1] & 0xFF00) | midnight
		CPU["regs"][2] = (timer_ticks >> 16) & 0xFFFF
		CPU["regs"][3] = (timer_ticks & 0xFFFF)
		emu_debug(0, "time: get clock time\n")
		return true
	elseif (ah == 0x01) then
		local timer_ticks = CPU["regs"][3] | (CPU["regs"][2] << 16)
		set_timer_ticks(timer_ticks)
		-- reset midnight flag
		timer_last_midnight = 0
		while (timer_ticks - timer_midnight) >= timer_last_midnight do
			timer_last_midnight = timer_last_midnight + timer_midnight
		end
		emu_debug(0, "time: set clock time\n")
		return true
	elseif (ah == 0x03) or (ah == 0x05) then
		-- TODO
		emu_debug(0, "time: set RTC (dummy)\n")
		return true
	elseif (ah == 0x02) or (ah == 0x04) then
		-- TODO
		cpu_clear_flag(0) -- clear carry
		CPU["regs"][2] = 0x0000
		CPU["regs"][3] = 0x0000
		emu_debug(0, "time: get RTC (dummy)\n")
		return true
	else
		cpu_set_flag(0)
	end
end)

set_timer_ticks(0)
