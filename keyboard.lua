-- TODO: The keyboard code is a massive hack which in no way reflects real hardware.

-- joysticks (for now)
cpu_port_set(0x201, function(cond,val)
	if not val then
		return 0
	end
end)

local charqueue = {}
local kbd_data_queue = {}

local function populate_bitmask(kz)
	local keys = 0
	for i,k in ipairs(kz) do
		if k >= 0 and platform_key_down(k) then
			keys = keys | (1 << (i - 1))
		end
	end
	return keys
end

function keyboard_update()
	RAM[0x417] = populate_bitmask({0x36, 0x2A, 0x1D, 0x38, 0x46, -1, 0x3A, 0x52})
	RAM[0x418] = populate_bitmask({-1, -1, -1, -1, -1, -1, -1, -1})
end

function kbd_send_ibm(code, chr)
	while #kbd_data_queue > 0 do table.remove(kbd_data_queue, 1) end
	if code <= 0x7F then
		table.insert(charqueue, {code, chr or 0})
	end
	table.insert(kbd_data_queue, code)
	emu_debug(2, string.format("kbd_send_ibm: %02X", code))
	if pic_interrupt_enabled(PIC_INTERRUPT_KEYBOARD) then
		cpu_emit_interrupt(9, false)
	end
end

RAM[0x471] = 0 -- break key check
RAM[0x496] = 0 -- keyboard mode/type
RAM[0x497] = 0 -- keyboard LED flags
RAM[0x417] = 0 -- keyboard flags 1
RAM[0x418] = 0 -- keyboard flags 2

local kbd_port_b = 0x03
local kbd_spkr_latch = 0

function kbd_get_spkr_latch()
	local v = kbd_spkr_latch
	kbd_spkr_latch = 0
	return v
end

function kbd_get_port_b()
	return kbd_port_b
end

local kbd_status = 0x10
local kbd_a2 = 0

-- keyboard I/O
cpu_port_set(0x60, function(cond,val)
	if not val then
		emu_debug(1, string.format("kbd 0060: read"))
		if #kbd_data_queue > 0 then
			local v = kbd_data_queue[1]
			table.remove(kbd_data_queue, 1)
			if type(v) == "table" then return v[1] end
			return v
		end
		return 0x00
	else
		kbd_a2 = 0
		emu_debug(1, string.format("kbd 0060: received %02X", val))
		if val == 5 then -- TODO: What is this?
			kbd_data_queue = {}
			table.insert(kbd_data_queue, 0x00)
			table.insert(kbd_data_queue, 0x00)
			table.insert(kbd_data_queue, 0x00)
		end
	end
end)

cpu_port_set(0x61, function(cond,val)
	if not val then
		emu_debug(1, "kbd port b read")
		return kbd_port_b
	else
		emu_debug(1, "kbd port b write " .. val)
		kbd_port_b = val
		kbd_spkr_latch = kbd_spkr_latch | val
	end
end)

cpu_port_set(0x64, function(cond,val)
	if not val then
		emu_debug(1, string.format("kbd 0064: read"))
		local v = kbd_status | (kbd_a2 << 3)
		if #kbd_data_queue > 0 then
			kbd_status = kbd_status ~ 3
		else
			kbd_status = kbd_status & (0xFC)
		end
		return v
	else
		kbd_a2 = 1
		emu_debug(1, string.format("kbd 0064: received %02X", val))
	end
end)

cpu_register_interrupt_handler(0x16, function(ax,ah,al)
	if (ah == 0x00) then
		local bios, ascii = nil, nil
		local dd = nil
		if type(charqueue[1]) == "table" then
			dd = charqueue[1]
			table.remove(charqueue, 1)
		end

		if dd ~= nil then
			bios = dd[1]
			ascii = dd[2]
		end

		if bios == nil then
			cpu_set_flag(6)
			return "block"
		else
			cpu_clear_flag(6)
			CPU["regs"][1] = ((bios & 0xFF) << 8) | (ascii & 0xFF)
			return true
		end
	elseif (ah == 0x01) then
		-- check for keystroke
		local ascii, bios = nil, nil
		if (#charqueue > 0) and type(charqueue[1]) == "table" then
			ascii = charqueue[1][2]
			bios = charqueue[1][1]
		end

		if bios == nil then
			cpu_set_flag(6)
			return true
		else
			cpu_clear_flag(6)
			CPU["regs"][1] = ((bios & 0xFF) << 8) | (ascii & 0xFF)
			return true
		end
	elseif (ah == 0x02) then
		-- query shift flags
		if ah == 0x12 then
			ah = RAM[0x418]
		end
		al = RAM[0x417]
		CPU["regs"][1] = (ah << 8) | al
		return true
	end
end)

