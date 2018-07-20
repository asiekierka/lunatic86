local ROTATE_MODE_ROR = 0
local ROTATE_MODE_ROL = 1
local ROTATE_MODE_RCR = 2
local ROTATE_MODE_RCL = 3

local function cpu_rotate(v2, v1, mode, cf)
	opcode = 1
	local w = (opcode & 0x01)
	local shift = 15
	if w == 0 then shift = 7 end

	local vr = v2
	local of = 0

	local shifts = v1
	if shifts > 0 then
		if mode == ROTATE_MODE_ROR then
			while shifts > 0 do
				cf = (vr & 0x01)
				vr = (vr >> 1) | ((vr & 0x01) << shift)
				shifts = shifts - 1
			end
			of = ((vr >> shift) ~ (vr >> (shift - 1))) & 0x01
		elseif mode == ROTATE_MODE_ROL then
			while shifts > 0 do
				cf = (vr >> shift) & 0x01
				vr = ((vr << 1) & ((1 << (shift + 1)) - 1)) | (vr >> shift)
				shifts = shifts - 1
			end
			of = ((vr >> shift) ~ cf) & 0x01
		elseif mode == ROTATE_MODE_RCR then
			while shifts > 0 do
				local newcf = (vr & 0x01)
				vr = (vr >> 1) | (cf << shift)
				shifts = shifts - 1
				cf = newcf
			end
			of = ((vr >> shift) ~ (vr >> (shift - 1))) & 0x01
		elseif mode == ROTATE_MODE_RCL then
			while shifts > 0 do
				local newcf = (vr >> shift) & 0x01
				vr = ((vr << 1) & ((1 << (shift + 1)) - 1)) | cf
				shifts = shifts - 1
				cf = newcf
			end
			of = ((vr >> shift) ~ cf) & 0x01
		end
	end
	return (vr & 0xFFFF) | (cf << 16) | (of << 17)
end

local function cpu_rotate_new(v2, v1, mode, cf)
	opcode = 1
	local w = (opcode & 0x01)
	local shift = 15
	if w == 0 then shift = 7 end

	local vr = v2
	local of = 0

	local shifts = v1
	if shifts > 0 then
		if mode == ROTATE_MODE_ROR then
			local shiftmask = (1 << shifts) - 1
			cf = (vr >> (shifts - 1)) & 0x01
			vr = (vr >> shifts) | ((vr & shiftmask) << (shift - shifts + 1))
			of = ((vr >> shift) ~ (vr >> (shift - 1))) & 0x01
		elseif mode == ROTATE_MODE_ROL then
			cf = (vr >> (shift - shifts + 1)) & 0x01
			vr = ((vr << shifts) & ((1 << (shift + 1)) - 1)) | (vr >> (shift - shifts + 1))
			of = ((vr >> shift) ~ cf) & 0x01
		elseif mode == ROTATE_MODE_RCR then
			while shifts > 0 do
				local newcf = (vr & 0x01)
				vr = (vr >> 1) | (cf << shift)
				shifts = shifts - 1
				cf = newcf
			end
			of = ((vr >> shift) ~ (vr >> (shift - 1))) & 0x01
		elseif mode == ROTATE_MODE_RCL then
			while shifts > 0 do
				local newcf = (vr >> shift) & 0x01
				vr = ((vr << 1) & ((1 << (shift + 1)) - 1)) | cf
				shifts = shifts - 1
				cf = newcf
			end
			of = ((vr >> shift) ~ cf) & 0x01
		end
	end
	return (vr & 0xFFFF) | (cf << 16) | (of << 17)
end

for i=0,3 do
	print("Testing mode " .. i)
	for j=0,15 do
		for k=0,65535 do
			for l=0,1 do
				if cpu_rotate_new(k,j,i,l) ~= cpu_rotate(k,j,i,l) then
					print("MISMATCH! " .. k .. "(" .. l .. ") rot " .. j .. " = " .. cpu_rotate_new(k,j,i,l) .. ", should " .. cpu_rotate(k,j,i,l))
				end
			end
		end
	end
end

print("Benchmarking...")

local speeds_old={}
local speeds_new={}

for i=0,3 do
	speeds_old[i] = os.clock()
	for j=0,15 do
		for k=0,65535 do
			for l=0,1 do
				cpu_rotate(k,j,i,l)
			end
		end
	end
	speeds_old[i] = os.clock() - speeds_old[i]
	speeds_new[i] = os.clock()
	for j=0,15 do
		for k=0,65535 do
			for l=0,1 do
				cpu_rotate_new(k,j,i,l)
			end
		end
	end
	speeds_new[i] = os.clock() - speeds_new[i]
	print("Mode " .. i .. ": old = " .. speeds_old[i] .. ", new = " .. speeds_new[i])
end
