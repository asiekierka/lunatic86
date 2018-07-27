os.setlocale('')

local curses = require "curses"
local cp437_trans = require "table_cp437"
dofile("kbdmaps.lua")

function platform_sleep(t)
	-- for what pvrpose
	os.execute("sleep " .. tonumber(t))
end

local stdscr = curses.initscr()
curses.cbreak()
curses.echo(false)
curses.nl(false)
stdscr:clear()
stdscr:nodelay(true)
stdscr:scrollok(true)
stdscr:keypad(true)

function platform_beep(freq)
	emu_debug(2, "BEEP " .. freq)
end

function platform_key_down(v)
	return false
end

function emu_debug(level, s, tb)
	if level >= 1 then
		io.stderr:write(s .. "\n")
		if tb then debug.traceback() end
		io.stderr:flush()
	end
end

local queued_up = {}

-- non-blocking, returns (ascii char, bios scan code) or nil on none
function platform_getc()
	local c = stdscr:getch()
	if c == nil then return nil end
	if type(c) == "string" then c = string.byte() end
	if c == 263 then c = 8 end
	emu_debug(2, string.format("getc %d", c))
	if map_char_to_key[c] then
		if c >= 0 and c < 128 then return c,map_char_to_key[c]
		else return 0,map_char_to_key[c] end
	end
	return nil
end

function platform_error(msg)
	curses.endwin()
	print(msg)
	print(debug.traceback())
end

local function platform_kbd_tick()
	local getmore = true
	while getmore do
		if #queued_up > 0 then
			kbd_send_ibm(queued_up[1][1], queued_up[1][2])
			table.remove(queued_up, 1)
		end

		local ch, code = platform_getc()
		if ch ~= nil then
			kbd_send_ibm(code, ch)
--			queued_up[#queued_up+1] = {code | 0x80, ch}
		else getmore = false end
	end
end

function platform_render_cga_mono(vram, addr)
	platform_kbd_tick()	
end

function platform_render_mcga_13h(vram, addr)
	platform_kbd_tick()
end

function platform_render_pcjr_160(vram, addr)
	platform_kbd_tick()
end

function platform_render_pcjr_320(vram, addr)
	platform_kbd_tick()
end

function platform_render_text(vram, addr, width, height, pitch)
	platform_kbd_tick()

	local dlines = video_pop_dirty_lines()
	for y,v in pairs(dlines) do
		local base = addr + (y * pitch)
		for x=0,width-1 do
			local chr = cp437_trans[vram[base + x*2] or 0]
			local atr = vram[base + x*2 + 1] or 0
			stdscr:mvaddstr(y, x, utf8.char(chr))
		end
	end
	stdscr:refresh()
end

function platform_finish()
	curses.endwin()
end

dofile("emu_core.lua")
