-- 8259 PIC
-- TODO: Not actually emulated.

PIC_INTERRUPT_PRINTER = 7
PIC_INTERRUPT_FDD = 6
PIC_INTERRUPT_HDD = 5
PIC_INTERRUPT_COM1 = 4
PIC_INTERRUPT_COM2 = 3
PIC_INTERRUPT_VIDEO = 2
PIC_INTERRUPT_KEYBOARD = 1
PIC_INTERRUPT_MOUSE = 1
PIC_INTERRUPT_RTC = 1
PIC_INTERRUPT_TIMER = 0

local int_mask = 0x00
local pic_irr = 0x00
local pic_isr = 0x00

function pic_interrupt_enabled(i)
	return true
--	return (int_mask & (1 << i)) == 0
end

--function pic_interrupt_sent(i)
--	pic_isr = pic_isr | (1 << i)
--end

local next_20 = 0x00

pic_isr = 0xFF

cpu_port_set(0x20, function(cond, val)
	if val then next_20 = val else
		if next_20 == 0x0A then
			return pic_irr
		elseif next_20 == 0x0B then
			pic_isr = pic_isr ~ 0xFF
			return pic_isr
		elseif next_20 == 0x20 then
			-- TODO
			return 0
		else
			-- unknown
			return 0xFF
		end
	end
end)

local int_22 = 0
local int_23 = 0

cpu_port_set(0x21, function(cond, val)
	if not val then return int_mask else int_mask = val end
end)

cpu_port_set(0x22, function(cond, val)
	if not val then return int_22 else int_22 = val end
end)

cpu_port_set(0x23, function(cond, val)
	if not val then return int_23 else int_23 = val end
end)
