-- Uncomment this line if you're on OpenComputers/Kallisti.
-- local filesystem = require("filesystem")

local function disk_finder(fn)
	local tmpfn = nil
	local ptrmode = nil
	local f = nil

	return function(a, mode)
		if tmpfn == nil or not filesystem.exists(tmpfn) then
			ptrmode = nil
		end
		if ptrmode ~= mode then
			if f ~= nil then pcall(function() f:close() end) end
			tmpfn = nil
			for p1 in filesystem.list("/mnt") do
				local tfn = "/mnt/" .. p1 .. fn
				if filesystem.exists(tfn) then
					tmpfn = tfn
				end
			end
			if tmpfn ~= nil then
				f = io.open(tmpfn, mode)
				disk_init_data(f, a)
				f:seek("set", 0)
				ptrmode = mode
			else
				f = nil
				disk_init_data(nil, a)
			end
		end
		return f
	end
end

-- Lunatic86 will currently boot from the first floppy disk drive.
-- Function: disk_init(filename or disk_finder, drive ID)
-- 0x00 is A:, 0x01 is B:, 0x80 and later are hard drives.
-- disk_finder(filename) will use OpenComputers to find
-- /mnt/.../filename dynamically and use it as a floppy disk,
-- potentially allowing for some degree of hotswapping.

-- disk_init(disk_finder("disk.img"), 0x00)
