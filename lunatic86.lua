function has_module(m)
	res = pcall(function()
		require(m)
	end)
	if res then return true else return false end
end

local is_opencomputers = has_module("component")

reduced_memory_mode = 3
if (1<<62) == 0 then reduced_memory_mode = 2 end

memory_preallocate = false

if is_opencomputers then
	dofile("platform_oc.lua")
else
	dofile("platform_curses.lua")
end
dofile("emu_core.lua")

xpcall(function()
	emu_execute()
end, function(err)
	platform_error(err)
end)

platform_finish()
