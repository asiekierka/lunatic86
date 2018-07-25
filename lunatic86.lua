local args = {...}
local argp = {}

local drive_map = {["a"]=0x00,["b"]=0x01,["c"]=0x80,["d"]=0x81,["e"]=0x82,["f"]=0x83}

function has_module(m)
	res = pcall(function()
		require(m)
	end)
	if res then return true else return false end
end

local is_opencomputers = has_module("component")

-- defaults
argp["boot"] = "a"
argp["mempack"] = 0

if is_opencomputers then
	argp["mempack"] = 3
	if (1<<62) == 0 then argp["mempack"] = 2 end
end

for i=1,#args do
	local a = args[i]
	if a:sub(1,1) == "-" then
		local key = a:sub(2)
		local value = ((i + 1) <= #args) and args[i + 1] or "-"
		if value:sub(1,1) == "-" then
			argp[key] = true
		else
			i = i + 1
			argp[key] = value
		end
	end
end

if argp["h"] or argp["help"] then
	print("lunatic86")
	print("Usage: lunatic86 [args]")
	print("    -boot [drive]        Boot from the given drive: a, b, c, d, e, f")
	print("    -[drive] [filename]  Initialize a given drive letter from a specified file")
	print("    -mempack [0,1,2,3]   Set the memory packing level. 0 is fastest; 3 requires")
	print("                         64-bit Lua and is about 10% slower; 2 requires only")
	print("                         32-bit Lua but is a few percent slower still.")
	os.exit()
end

if argp[argp["boot"]] == nil then
	print("Error: Did not find file for boot drive!")
	print("See 'lunatic86 -h' for usage.")
	os.exit()
end

reduced_memory_mode = math.floor(tonumber(argp["mempack"]))
memory_preallocate = false

if is_opencomputers then
	local shell = require("shell")
	local filesystem = require("filesystem")
	if filesystem.exists("emu_core.lua") then
		dofile("platform_oc.lua")
	else
		local cwd = shell.getWorkingDirectory()
		if filesystem.exists("/usr/lib/lunatic86/emu_core.lua") then
			shell.setWorkingDirectory("/usr/lib/lunatic86")
		elseif filesystem.exists("/lib/lunatic86/emu_core.lua") then
			shell.setWorkingDirectory("/lib/lunatic86")
		end
		dofile("platform_oc.lua")
		shell.setWorkingDirectory(cwd)
	end
else
	dofile("platform_curses.lua")
end

for dk,did in pairs(drive_map) do
	if argp[dk] then
		disk_init(argp[dk],did)
	end
end

disk_boot(drive_map[argp["boot"]])

xpcall(function()
	emu_execute()
end, function(err)
	platform_error(err)
end)

platform_finish()
