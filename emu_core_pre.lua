// #define DEBUG_IO
// #define DEBUG_INTRS
// #define DEBUG_IPS
// #define DEBUG_MEM_UNINITIALIZED
local ram_640k = {}
local io_ports = {}
local ram_rom = {}
-- local run_one = nil
local opc = 0

RAM = {}
if memory_preallocate then
	local max_mem = 0xA0000
	if reduced_memory_mode > 0 then max_mem = max_mem >> reduced_memory_mode end
	for i=1,max_mem do
		ram_640k[i]=0
	end
end

if reduced_memory_mode > 0 then
	local rmm_mask = (1 << reduced_memory_mode) - 1
	local rmm = reduced_memory_mode
	setmetatable(RAM, {
		__index = function(t, key)
			if (key < 0xA0000) then
				local k = (key >> rmm) + 1
				return ((ram_640k[k] or 0) >> ((key & rmm_mask) << 3)) & 0xFF
			elseif (key < 0xC0000) then
				return video_vram_read(key)
			elseif (key >= 0xF0000 and key < 0x100000) then
				return ram_rom[key - 0xEFFFF] or 0x00
			else
				return 0xFF
			end
		end,
		__newindex = function(t, key, value)
			if (key < 0xA0000) then
				local shift = ((key & rmm_mask) << 3)
				local k = (key >> rmm) + 1
				local mask = 255 << shift
				local nmask = mask ~ (-1)
				ram_640k[k] = ((ram_640k[k] or 0) & nmask) | ((value & 0xFF) << shift)
			elseif (key < 0xC0000) then
				video_vram_write(key, value)
			elseif (key >= 0xF0000 and key < 0x100000) then
				ram_rom[key - 0xEFFFF] = value
			end
		end
	})
	rawset(RAM, "r16", function(ram, key)
		if (key < 0x9FFFE) and ((key & rmm_mask) < rmm_mask) then
			local k = (key >> rmm) + 1
			return ((ram_640k[k] or 0) >> ((key & rmm_mask) << 3)) & 0xFFFF
		else
			return ram[key] | (ram[key + 1] << 8)
		end
	end)
	rawset(RAM, "w16", function(ram, key, value)
		if (key < 0x9FFFE) and ((key & rmm_mask) < rmm_mask) then
			local k = (key >> rmm) + 1
			local shift = ((key & rmm_mask) << 3)
			local mask = 0xFFFF << shift
			local nmask = mask ~ (-1)
			ram_640k[k] = ((ram_640k[k] or 0) & nmask) | ((value & 0xFFFF) << shift)
		else
			ram[key] = (value & 0xFF)
			ram[key + 1] = (value >> 8)
		end
	end)
else
	setmetatable(RAM, {
		__index = function(t, key)
			if (key < 0xA0000) then
#ifdef DEBUG_MEM_UNINITIALIZED
				if ram_640k[key + 1] == nil then
					emu_debug(1, string.format("accessing uninitialized memory at %05x",  key), true)
					ram_640k[key + 1] = 0
				end
#endif
				return ram_640k[key + 1] or 0
			elseif (key < 0xC0000) then
				return video_vram_read(key)
			elseif (key >= 0xF0000 and key < 0x100000) then
				return ram_rom[key - 0xEFFFF] or 0x00
			else
				return 0xFF
			end
		end,
		__newindex = function(t, key, value)
			if (key < 0xA0000) then
				ram_640k[key + 1] = value
			elseif (key < 0xC0000) then
				video_vram_write(key, value)
			elseif (key >= 0xF0000 and key < 0x100000) then
				ram_rom[key - 0xEFFFF] = value
			end
		end
	})
	rawset(RAM, "r16", function(ram, key)
		if key < 0x9FFFF then
			return (ram_640k[key + 1] or 0) | ((ram_640k[key + 2] or 0) << 8)
		else
			return ram[key] | (ram[key + 1] << 8)
		end
	end)
	rawset(RAM, "w16", function(ram, key, value)
		if key < 0x9FFFF then
			ram_640k[key + 1] = (value & 0xFF)
			ram_640k[key + 2] = (value >> 8)
		else
			ram[key] = (value & 0xFF)
			ram[key + 1] = (value >> 8)
		end
	end)
end

rawset(RAM, "w32", function(ram, key, value)
	ram[key] = (value & 0xFF)
	ram[key + 1] = (value >> 8) & 0xFF
	ram[key + 2] = (value >> 16) & 0xFF
	ram[key + 3] = (value >> 24)
end)
rawset(RAM, "r32", function(ram, key)
	return ram[key] | (ram[key + 1] << 8) | (ram[key + 2] << 16) | (ram[key + 3] << 32)
end)

SEG_ES = 0
SEG_CS = 1
SEG_SS = 2
SEG_DS = 3

-- ax,cx,dx,bx,sp,bp,si,di
local CPU_REGS = {0,0,0,0,0,0,0,0}
-- es,cs,ss,ds,fs,gs
local CPU_SEGMENTS = {0,0,0,0,0,0}
local CPU_IP = 0
local CPU_FLAGS = 0
local CPU_SEGMOD = nil
local CPU_HALTED = false
local CPU_INTQUEUE = {}

CPU = {
	regs = CPU_REGS,
	segments = CPU_SEGMENTS,
	flags = CPU_FLAGS
}

local CPU = CPU
local RAM = RAM
local SEG_ES = SEG_ES
local SEG_SS = SEG_SS
local SEG_CS = SEG_CS
local SEG_DS = SEG_DS

-- initialize ivt (early, so it can be overridden)
for i=0,255 do
	RAM:w16(i*4, 0x1100 + i)
	RAM:w16(i*4 + 2, 0xF000)
end

function seg(s, v)
	return (CPU_SEGMENTS[s+1]<<4) + v
end
#define seg(s, v) ((CPU_SEGMENTS[(s)+1]<<4)+(v))

function segmd(s, v)
	return (CPU_SEGMENTS[(CPU_SEGMOD or s)+1]<<4) + v
end
#define segmd(s, v) ((CPU_SEGMENTS[(CPU_SEGMOD or (s))+1]<<4) + (v))

local function cpu_advance_ip()
	local ip = seg(SEG_CS, CPU_IP)
	CPU_IP = (CPU_IP + 1) & 0xFFFF
	return RAM[ip]
end

local function cpu_advance_ip16()
	local ip = seg(SEG_CS, CPU_IP)
	CPU_IP = (CPU_IP + 2) & 0xFFFF
	return RAM:r16(ip)
end

local function to_s8(i)
	if i >= 0x80 then return i - 0x100 else return i end
end

local function to_s16(i)
	if i >= 0x8000 then return i - 0x10000 else return i end
end

local function to_s32(i)
	if i >= 0x80000000 then return i - 0x100000000 else return i end
end

function cpu_flag(t)
	return (CPU_FLAGS & (1 << t)) ~= 0
end
#define cpu_flag(t) ((CPU_FLAGS & (1<<(t))) ~= 0)

function cpu_clear_flag(t)
	CPU_FLAGS = CPU_FLAGS & (~(1 << t))
end
#define cpu_clear_flag(t) CPU_FLAGS = CPU_FLAGS & (~(1 << (t)))

function cpu_set_flag(t)
	CPU_FLAGS = CPU_FLAGS | (1 << t)
end
#define cpu_set_flag(t) CPU_FLAGS = CPU_FLAGS | (1 << (t))

function cpu_write_flag(t, v)
	if v then
		CPU_FLAGS = CPU_FLAGS | (1 << t)
	else
		CPU_FLAGS = CPU_FLAGS & (~(1 << t))
	end
end

function cpu_complement_flag(t)
	CPU_FLAGS = CPU_FLAGS ~ (1 << t)
end
#define cpu_complement_flag(t) CPU_FLAGS = CPU_FLAGS ~ (1 << (t))

local function cpu_incdec_dir(t, amount)
	if cpu_flag(10) then
		CPU_REGS[t] = (CPU_REGS[t] - amount) & 0xFFFF
	else
		CPU_REGS[t] = (CPU_REGS[t] + amount) & 0xFFFF
	end
end
local function cpu_print(t)
	emu_debug(1, string.format("[%04X %04X] ", CPU_SEGMENTS[SEG_CS+1], (CPU_IP-1) & 0xFFFF) .. t .. "\n")
end

function cpu_set_ip(vcs, vip)
	CPU_SEGMENTS[SEG_CS+1] = vcs
	CPU_IP = vip
end

-- src format:
-- 0-7 = ax,cx,dx,bx,sp,bp,si,di
-- 8-15 = bx+si+disp, bx+di+disp, bp+si+disp, bp+di+disp, si+disp, di+disp, bp+disp, bx+disp
-- 16-23 = al,cl,dl,bl,ah,ch,dh,bh
-- 24-25 = address8(disp), address16(disp)
-- 26-31 = es,cs,ss,ds,fs,gs
-- 32-39 = 8-15(8-bit)
-- 40 = imm8, 41 = imm16

local _cpu_rm_seg_t = {
	SEG_DS, SEG_DS,
	SEG_SS, SEG_SS,
	SEG_DS, SEG_DS,
	SEG_SS, SEG_DS
}

local function _cpu_rm_seg(data, v)
	return _cpu_rm_seg_t[v + 1]
end
#define _cpu_rm_seg(data, v) _cpu_rm_seg_t[(v) + 1]

local _cpu_rm_addr_t = {
	function(data) return (CPU_REGS[4] + CPU_REGS[7] + data.disp) & 0xFFFF end,
	function(data) return (CPU_REGS[4] + CPU_REGS[8] + data.disp) & 0xFFFF end,
	function(data) return (CPU_REGS[6] + CPU_REGS[7] + data.disp) & 0xFFFF end,
	function(data) return (CPU_REGS[6] + CPU_REGS[8] + data.disp) & 0xFFFF end,
	function(data) return (CPU_REGS[7] + data.disp) & 0xFFFF end,
	function(data) return (CPU_REGS[8] + data.disp) & 0xFFFF end,
	function(data) return (CPU_REGS[6] + data.disp) & 0xFFFF end,
	function(data) return (CPU_REGS[4] + data.disp) & 0xFFFF end
}

local function _cpu_rm_addr(data, v)
	return _cpu_rm_addr_t[v+1](data)
end
#define _cpu_rm_addr(data, v) _cpu_rm_addr_t[(v) + 1]((data))

local function cpu_seg_rm(data, v)
	if v >= 8 and v < 16 then
		return _cpu_rm_seg(data, v - 8)
	elseif v >= 32 and v < 40 then
		return _cpu_rm_seg(data, v - 32)
	else return SEG_DS end
end

local function cpu_addr_rm(data, v)
	if v >= 8 and v < 16 then
		return _cpu_rm_addr(data, v - 8)
	elseif v >= 24 and v <= 25 then
		return data.disp
	elseif v >= 32 and v < 40 then
		return _cpu_rm_addr(data, v - 32)
	else
		platform_error("cpu_addr_rm todo: " .. v)
	end
end

local readrm_table = {}
for i=0,7 do readrm_table[i] = function(data, v) return CPU_REGS[v + 1] end end
for i=8,15 do readrm_table[i] = function(data, v)
	return RAM:r16(segmd(_cpu_rm_seg(data, v - 8), _cpu_rm_addr(data, v - 8)))
end end
for i=16,19 do readrm_table[i] = function(data, v)
	return CPU_REGS[(v & 3) + 1] & 0xFF
end end
for i=20,23 do readrm_table[i] = function(data, v)
	return CPU_REGS[(v & 3) + 1] >> 8
end end
readrm_table[24] = function(data,v) return RAM[segmd(SEG_DS, data.disp)] end
readrm_table[25] = function(data,v) return RAM:r16(segmd(SEG_DS, data.disp)) end
for i=26,31 do readrm_table[i] = function(data, v)
	return CPU_SEGMENTS[v - 25]
end end
for i=32,39 do readrm_table[i] = function(data, v)
	return RAM[segmd(_cpu_rm_seg(data, v - 32), _cpu_rm_addr(data, v - 32))]
end end
readrm_table[40] = function(data, v) return data.imm & 0xFF end
readrm_table[41] = function(data, v) return data.imm & 0xFFFF end

local function cpu_read_rm(data, v)
	return readrm_table[v](data, v)
end

local writerm_table = {}
for i=0,7 do writerm_table[i] = function(data, v, val)
	CPU_REGS[v + 1] = val & 0xFFFF
end end
for i=8,15 do writerm_table[i] = function(data, v, val)
	RAM:w16(segmd(_cpu_rm_seg(data, v - 8), _cpu_rm_addr(data, v - 8)), val)
end end
for i=16,19 do writerm_table[i] = function(data, v, val)
	local r = CPU_REGS[(v & 3) + 1]
	CPU_REGS[(v & 3) + 1] = (r & 0xFF00) | (val & 0xFF)
end end
for i=20,23 do writerm_table[i] = function(data, v, val)
	local r = CPU_REGS[(v & 3) + 1]
	CPU_REGS[(v & 3) + 1] = (r & 0x00FF) | ((val & 0xFF) << 8)
end end
writerm_table[24] = function(data, v, val)
	RAM[segmd(SEG_DS, data.disp)] = val & 0xFF
end
writerm_table[25] = function(data, v, val)
	RAM:w16(segmd(SEG_DS, data.disp), val & 0xFFFF)
end
for i=26,31 do writerm_table[i] = function(data, v, val)
	CPU_SEGMENTS[v - 25] = val & 0xFFFF
end end
for i=32,39 do writerm_table[i] = function(data, v, val)
	RAM[segmd(_cpu_rm_seg(data, v - 32), _cpu_rm_addr(data, v - 32))] = val & 0xFF
end end
local function cpu_write_rm(data, v, val)
	writerm_table[v](data, v, val)
end

local mrm_table = {}
for i=0,2047 do
	local is_seg = (i & 1024) ~= 0
	local mod = (i >> 6) & 0x03
	local reg = (i >> 3) & 0x07
	local rm = i & 0x07
	local d = (i >> 9) & 0x01
	local w = (i >> 8) & 0x01
	if is_seg then w = 1 end

	local op1 = reg
	local op2 = rm

	if is_seg then
		op1 = (op1 % 6) + 26
	elseif w == 0 then
		op1 = op1 + 16
	end

	if mod == 0 and rm == 6 then
		op2 = 24 + w
	elseif mod ~= 3 then -- do not treat rm as reg field
		if w == 0 then
			op2 = op2 + 32
		else
			op2 = op2 + 8
		end
	else -- rm is reg field
		if w == 0 then
			op2 = op2 + 16
		end
	end

	local src, dst
	if d == 0 then
		src = op1
		dst = op2
	else
		src = op2
		dst = op1
	end

	local cdisp = 0
	if mod == 2 then cdisp = 2
	elseif mod == 1 then cdisp = 1
	elseif mod == 0 and rm == 6 then cdisp = 3 end
	mrm_table[i+1] = {src=src,dst=dst,cdisp=cdisp,disp=0}
end

local function cpu_mod_rm(opcode, is_seg)
	local modrm = cpu_advance_ip() | ((opcode & 3) << 8) | (is_seg or 0)
	local data = mrm_table[modrm+1]

	if data.cdisp == 0 then
		return data
	elseif data.cdisp == 2 then
		data.disp = to_s16(cpu_advance_ip16())
	elseif data.cdisp == 1 then
		data.disp = to_s8(cpu_advance_ip())
	elseif data.cdisp == 3 then
		data.disp = cpu_advance_ip16()
	end

	return data
end

local function cpu_mod_rm_copy(opcode, is_seg)
	local data = cpu_mod_rm(opcode, is_seg)
	return {src=data.src,dst=data.dst,disp=data.disp}
end

local mrm6_4 = {src=40,dst=16,imm=0}
local mrm6_5 = {src=41,dst=0,imm=0}

local mrm6_table = {
	cpu_mod_rm,
	cpu_mod_rm,
	cpu_mod_rm,
	cpu_mod_rm,
--	function(v) return {src=40,dst=16,imm=cpu_advance_ip()} end,
--	function(v) return {src=41,dst=0,imm=cpu_advance_ip16()} end,
	function(v)
		mrm6_4.imm=cpu_advance_ip()
		return mrm6_4
	end,
	function(v)
		mrm6_5.imm=cpu_advance_ip16()
		return mrm6_5
	end,
	cpu_mod_rm,
	cpu_mod_rm
}

local function cpu_mod_rm6(opcode)
	local v = opcode & 0x07
	return mrm6_table[v+1](v)
end
#define cpu_mod_rm6(opcode) mrm6_table[((opcode) & 0x7)+1]((opcode))

local parity_table = {}
for i = 0,255 do
	local p = 0
	local v = i
	while v ~= 0 do
		p = p + (v & 1)
		v = v >> 1
	end
	parity_table[i+1] = (p & 1) == 0
end

local function cpu_write_parity(v)
	-- only effects LSB
	cpu_write_flag(2, parity_table[(v & 0xFF)+1])
end
#define cpu_write_parity(v) cpu_write_flag(2, parity_table[(v & 0xFF)+1])

local function cpu_push16(v)
	CPU_REGS[5] = (CPU_REGS[5] - 2 & 0xFFFF)
	RAM:w16(seg(SEG_SS,CPU_REGS[5]), v & 0xFFFF)
	--emu_debug(0, string.format("stack: >%04X @%04X\n", v, CPU_REGS[5]))
end

local function cpu_pop16()
	local sp = CPU_REGS[5]
	CPU_REGS[5] = (sp + 2) & 0xFFFF
	--emu_debug(0, string.format("stack: <%04X @%04X\n", RAM:r16(seg(SEG_SS,sp)), sp))
	return RAM:r16(seg(SEG_SS,sp))
end

local function cpu_mov(mrm)
	local v1 = cpu_read_rm(mrm, mrm.src)	
	cpu_write_rm(mrm, mrm.dst, v1)
end

local function _cpu_uf_zsp(vr, opc)
	if (opc & 0x01) == 1 then
		cpu_write_flag(6, (vr & 0xFFFF) == 0)
		cpu_write_flag(7, (vr & 0x8000) ~= 0)
		cpu_write_parity(vr)
	else
		cpu_write_flag(6, (vr & 0xFF) == 0)
		cpu_write_flag(7, (vr & 0x80) ~= 0)
		cpu_write_parity(vr)
	end
end

local function _cpu_uf_inc(vr, opc)
	_cpu_uf_zsp(vr, opc)
	cpu_write_flag(4, (vr & 0xF) == 0x0) -- 15 + 1 = 16
	if (opc & 0x01) == 1 then
		cpu_write_flag(11, vr == 0x8000)
	else
		cpu_write_flag(11, vr == 0x80)
	end
end
	
local function _cpu_uf_dec(vr, opc)
	_cpu_uf_zsp(vr, opc)
	cpu_write_flag(4, (vr & 0xF) == 0xF) -- 0 - 1 = 15
	if (opc & 0x01) == 1 then
		cpu_write_flag(11, vr == 0x7FFF)
	else
		cpu_write_flag(11, vr == 0x7F)
	end
end

local function _cpu_uf_co_add(v1, v2, vc, vr, opc)
	cpu_write_flag(4, ((v1 & 0xF) + (v2 & 0xF) + vc) >= 0x10)
	if (opc & 0x01) == 1 then
		cpu_write_flag(0, (vr & 0xFFFF) ~= vr)
		cpu_write_flag(11, ((v1 & 0x8000) == (v2 & 0x8000)) and ((vr & 0x8000) ~= (v1 & 0x8000)))
	else
		cpu_write_flag(0, (vr & 0xFF) ~= vr)
		cpu_write_flag(11, ((v1 & 0x80) == (v2 & 0x80)) and ((vr & 0x80) ~= (v1 & 0x80)))
	end
end

local function _cpu_uf_co_sub(v1, v2, vb, vr, opc)
	-- v2 - v1
	cpu_write_flag(4, ((v2 & 0xF) - (v1 & 0xF) - vb) < 0)
	if (opc & 0x01) == 1 then
		cpu_write_flag(0, (vr & 0xFFFF) ~= vr)
		cpu_write_flag(11, ((v1 & 0x8000) ~= (v2 & 0x8000)) and ((vr & 0x8000) == (v1 & 0x8000)))
	else
		cpu_write_flag(0, (vr & 0xFF) ~= vr)
		cpu_write_flag(11, ((v1 & 0x80) ~= (v2 & 0x80)) and ((vr & 0x80) == (v1 & 0x80)))
	end
end

local function _cpu_uf_bit(vr, opc)
	CPU_FLAGS = CPU_FLAGS & (~0x0801) -- clear carry (0) and overflow (11)
	_cpu_uf_zsp(vr, opc)
end

local function cpu_shl(mrm, opcode)
	local v1 = cpu_read_rm(mrm, mrm.src) & 0x1F
	if v1 >= 1 then
		-- todo: is the ifcheck correct?
		local v2 = cpu_read_rm(mrm, mrm.dst)
		local w = (opcode & 0x01)
		local mask = 0xFFFF
		if w == 0 then mask = 0xFF end
		local msb = ((mask >> 1) + 1)

		local vr = v2 << v1
		cpu_write_flag(0, (vr & (mask + 1)) ~= 0)
		cpu_write_rm(mrm, mrm.dst, vr & mask)
		_cpu_uf_zsp(vr & mask, opcode)
		if v1 == 1 then
			local msb_result = (vr & msb) ~= 0
			cpu_write_flag(11, cpu_flag(0) ~= msb_result)
		end
	end
end

local function cpu_shr(mrm, opcode, arith)
	local w = (opcode & 0x01)
	local mask = 0x8000
	if w == 0 then mask = 0x80 end

	local v1 = cpu_read_rm(mrm, mrm.src) & 0x1F
	local v2 = cpu_read_rm(mrm, mrm.dst)
	local vr
	if arith then
		vr = v2
		local shift1 = v1
		while shift1 > 0 do
			vr = (vr & mask) | ((vr >> 1) & (~mask))
			shift1 = shift1 - 1
		end
	else
		vr = v2 >> v1
	end
	cpu_write_rm(mrm, mrm.dst, vr)
	_cpu_uf_zsp(vr, opcode)
	if (1 << (v1 - 1)) > mask then
		cpu_write_flag(0, arith and ((v2 & mask) ~= 0))
	else
		cpu_write_flag(0, (v2 & (1 << (v1 - 1))) ~= 0)
	end
	if v1 == 1 then
		cpu_write_flag(11, (not arith) and ((v2 & mask) ~= 0))
	end
end

local ROTATE_MODE_ROR = 0
local ROTATE_MODE_ROL = 1
local ROTATE_MODE_RCR = 2
local ROTATE_MODE_RCL = 3

local function cpu_rotate(mrm, opcode, mode)
	local w = (opcode & 0x01)
	local shift = 15
	if w == 0 then shift = 7 end

	local v1 = cpu_read_rm(mrm, mrm.src) & 0x1F
	local v2 = cpu_read_rm(mrm, mrm.dst)
	local vr = v2
	local cf = 0
	local of = 0
	if cpu_flag(0) then cf = 1 end

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

		cpu_write_rm(mrm, mrm.dst, vr)
		cpu_write_flag(0, cf == 1)
		if v1 == 1 then
			cpu_write_flag(11, of == 1)
		end
	end
end

local function cpu_mul(mrm, opcode)
	local w = (opcode & 0x01)
	local v1 = cpu_read_rm(mrm, mrm.src)
	local v2 = cpu_read_rm(mrm, mrm.dst)
	local vr = v1 * v2
	if w == 1 then
		vr = vr & 0xFFFFFFFF
		CPU_REGS[3] = (vr >> 16)
		CPU_REGS[1] = vr & 0xFFFF

		cpu_write_flag(0, (vr >> 16) ~= 0)
		cpu_write_flag(11, (vr >> 16) ~= 0)
	else
		vr = vr & 0xFFFF
		CPU_REGS[1] = vr
		cpu_write_flag(0, (vr >> 8) ~= 0)
		cpu_write_flag(11, (vr >> 8) ~= 0)
	end
end

local function cpu_imul(mrm, opcode)
	local w = (opcode & 0x01)
	local v1 = cpu_read_rm(mrm, mrm.src)
	local v2 = cpu_read_rm(mrm, mrm.dst)
	local vr = 0
	if w == 1 then
		vr = (to_s16(v1) * to_s16(v2))
		CPU_REGS[3] = (vr >> 16) & 0xFFFF
		CPU_REGS[1] = vr & 0xFFFF

		cpu_write_flag(0, (vr < -0x8000) or (vr >= 0x8000))
		cpu_write_flag(11, (vr < -0x8000) or (vr >= 0x8000))

	else
		vr = (to_s8(v1) * to_s8(v2))
		CPU_REGS[1] = vr & 0xFFFF

		cpu_write_flag(0, (vr < -0x80) or (vr >= 0x80))
		cpu_write_flag(11, (vr < -0x80) or (vr >= 0x80))
	end
end

local function cpu_div(mrm, opcode)
	local w = (opcode & 0x01)
	local v2 = cpu_read_rm(mrm, mrm.dst)
	if w == 1 then
		local v = (CPU_REGS[3] << 16) | (CPU_REGS[1])
		if v2 == 0 then
			emu_debug(1, "throw exception (/ by 0: " .. v .. " / " .. v2 .. ")")
			cpu_emit_interrupt(0, false)
			return
		end
		local vd = math.floor(v / v2)
		local vr = v % v2
		if vd > 0xFFFF then
			emu_debug(1, "throw exception (overflow: " .. v .. " / " .. v2 .. " = " .. vd .. ")")
			cpu_emit_interrupt(0, false)
			return
		end

		CPU_REGS[3] = vr & 0xFFFF
		CPU_REGS[1] = vd & 0xFFFF
	else
		local v = (CPU_REGS[1])
		if v2 == 0 then
			emu_debug(1, "throw exception (/ by 0: " .. v .. " / " .. v2 .. ")")
			cpu_emit_interrupt(0, false)
			return
		end
		local vd = math.floor(v / v2)
		local vr = v % v2
		if vd > 0xFF then
			emu_debug(1, "throw exception (overflow: " .. v .. " / " .. v2 .. " = " .. vd .. ")")
			cpu_emit_interrupt(0, false)
			return
		end

		CPU_REGS[1] = ((vr & 0xFF) << 8) | (vd & 0xFF)
	end
end

local function cpu_idiv(mrm, opcode)
	local w = (opcode & 0x01)
	local v2 = cpu_read_rm(mrm, mrm.dst)
	if w == 1 then
		local v = (CPU_REGS[3] << 16) | (CPU_REGS[1])
		v = to_s32(v)
		v2 = to_s16(v2)
		if v2 == 0 then
			emu_debug(1, "throw exception (/ by 0: " .. v .. " / " .. v2 .. ")")
			cpu_emit_interrupt(0, false)
			return
		end
		local vd = v / v2
		if vd >= 0 then vd = math.floor(vd) else vd = math.ceil(vd) end
		local vr = math.fmod(v, v2)
		if (vd >= 0x8000) or (vd < -0x8000) then
			emu_debug(1, "throw exception (overflow: " .. v .. " / " .. v2 .. " = " .. vd .. ")")
			cpu_emit_interrupt(0, false)
			return
		end

		CPU_REGS[3] = vr & 0xFFFF
		CPU_REGS[1] = vd & 0xFFFF
	else
		local v = (CPU_REGS[1])
		v = to_s16(v)
		v2 = to_s8(v2)
		if v2 == 0 then
			emu_debug(1, "throw exception (/ by 0: " .. v .. " / " .. v2 .. ")")
			cpu_emit_interrupt(0, false)
			return
		end
		local vd = math.floor(v / v2)
		if vd >= 0 then vd = math.floor(vd) else vd = math.ceil(vd) end
		local vr = math.fmod(v, v2)
		if (vd >= 0x80) or (vd < -0x80) then
			emu_debug(1, "throw exception (overflow: " .. v .. " / " .. v2 .. " = " .. vd .. ")")
			cpu_emit_interrupt(0, false)
			return
		end

		CPU_REGS[1] = ((vr & 0xFF) << 8) | (vd & 0xFF)
	end
end

local function cpu_add(mrm, opcode, carry)
	local w = (opcode & 0x01)
	local v1 = cpu_read_rm(mrm, mrm.src)
	local v2 = cpu_read_rm(mrm, mrm.dst)
	local vc = 0
	if carry and cpu_flag(0) then
		vc = 1
	end
	local vr = v1 + v2 + vc
	if w == 1 then
		cpu_write_rm(mrm, mrm.dst, vr & 0xFFFF)
	else
		cpu_write_rm(mrm, mrm.dst, vr & 0xFF)
	end
	_cpu_uf_zsp(vr, opcode)
	_cpu_uf_co_add(v1, v2, vc, vr, opcode)
end

local function cpu_cmp(v1, v2, opcode)
	local w = (opcode & 0x01)
	local vr = v1 - v2
	_cpu_uf_co_sub(v2, v1, 0, vr, opcode)
	if w == 1 then
		vr = vr & 0xFFFF
	else
		vr = vr & 0xFF
	end
	_cpu_uf_zsp(vr, opcode)
end

local function cpu_sub(mrm, opcode, borrow, is_cmp)
	local w = (opcode & 0x01)
	local v1 = cpu_read_rm(mrm, mrm.src)
	local v2 = cpu_read_rm(mrm, mrm.dst)
	local vb = 0
	if borrow and cpu_flag(0) then
		vb = 1
	end
	local vr = v2 - v1 - vb
	_cpu_uf_co_sub(v1, v2, vb, vr, opcode)
	if w == 1 then
		vr = vr & 0xFFFF
	else
		vr = vr & 0xFF
	end
	if not is_cmp then
		cpu_write_rm(mrm, mrm.dst, vr)
	end
	_cpu_uf_zsp(vr, opcode)
end

local function cpu_xor(mrm, opc)
	local v1 = cpu_read_rm(mrm, mrm.src)
	local v2 = cpu_read_rm(mrm, mrm.dst)
	local vr = v1 ~ v2
	cpu_write_rm(mrm, mrm.dst, vr)
	_cpu_uf_bit(vr, opc)
end

local function cpu_and(mrm, opc, is_test)
	local v1 = cpu_read_rm(mrm, mrm.src)
	local v2 = cpu_read_rm(mrm, mrm.dst)
	local vr = v1 & v2
	if not is_test then
		cpu_write_rm(mrm, mrm.dst, vr)
	end
	_cpu_uf_bit(vr, opc)
end

local function cpu_or(mrm, opc)
	local v1 = cpu_read_rm(mrm, mrm.src)
	local v2 = cpu_read_rm(mrm, mrm.dst)
	local vr = v1 | v2
	cpu_write_rm(mrm, mrm.dst, vr)
	_cpu_uf_bit(vr, opc)
end

local function cpu_print_state(opcode, adv)
	cpu_print(string.format("%02X (flags %04X, seg ES %04X CS %04X SS %04X DS %04X)", opcode, CPU_FLAGS, CPU_SEGMENTS[1], CPU_SEGMENTS[2], CPU_SEGMENTS[3], CPU_SEGMENTS[4]))
	cpu_print(string.format("AX %04X CX %04X DX %04X BX %04X SP %04X BP %04X SI %04X DI %04X", CPU_REGS[1], CPU_REGS[2], CPU_REGS[3], CPU_REGS[4], CPU_REGS[5], CPU_REGS[6], CPU_REGS[7], CPU_REGS[8]))
	if adv then
		local s = "c?p?a?zstidoppn?"
		local s2 = ""
		local fl = CPU_FLAGS
		for i=0,15 do
			local s3 = s:sub(i + 1, i + 1)
			if (fl & 1) == 1 then s3 = s3:upper() end
			s2 = s2 .. s3
			fl = fl >> 1
		end
		cpu_print("FLAGS ADV:" .. s2)
	end
end

local function cpu_rep(cond)
	local old_ip = CPU_IP
	local opcode = cpu_advance_ip()

	-- check for string instructions
--	local opc_c = opcode & 0xFE
--	if opc_c ~= 0x6C and opc_c ~= 0x6E and opc_c ~= 0xA4 and opc_c ~= 0xA6 and opc_c ~= 0xAA and opc_c ~= 0xAC
--		and opc_c ~= 0xAE then
--		CPU_IP = old_ip
--		return true
--	end

	-- if length zero, skip
	-- TODO: this relies on the opcode after REP always being 1 long...
	if (CPU_REGS[2] == 0) then
		return true
	end

	CPU_IP = old_ip

	local pr_state = true
	while CPU_REGS[2] ~= 0 do
		local r = run_one(true, pr_state)

		if not r then return false
		elseif r == "block" then platform_error("this should never see 'block'") end

		CPU_REGS[2] = (CPU_REGS[2] - 1) & 0xFFFF
		if CPU_REGS[2] == 0 then break end

		local condResult = opcode ~= 0xA6 and opcode ~= 0xA7 and opcode ~= 0xAE and opcode ~= 0xAF
		if not condResult then condResult = cond() end
		if condResult then
			CPU_IP = old_ip
			pr_state = false
		else
			break
		end
	end
	return true
end

local function cpu_in(cond)
	local p = io_ports[cond+1]
#ifdef DEBUG_IO
	emu_debug(1, string.format("port (IN): %04X", cond), true)
#endif
	if p == nil then
#ifdef DEBUG_IO
		emu_debug(1, string.format("unknown port (IN): %04X", cond), true)
#endif
		return 0xFF
	elseif type(p) == "function" then return p(cond)
	else return p end
end

local function cpu_out(cond, val)
	local p = io_ports[cond+1]
#ifdef DEBUG_IO
		emu_debug(1, string.format("port (OUT): %04X <= %02X", cond, val), true)
#endif
	if type(p) == "function" then p(cond,val)
	elseif p ~= nil then io_ports[cond+1] = val
	else
#ifdef DEBUG_IO
		emu_debug(1, string.format("unknown port (OUT): %04X", cond), true)
#endif
	end
end

function cpu_port_get(cond)
	return io_ports[cond+1]
end

function cpu_port_set(cond, val, val2)
	if type(val) == "function" and type(val2) == "function" then
		io_ports[cond+1] = function(cond,v)
			if v then val2(cond,v) else val(cond) end
		end
	else
		io_ports[cond+1] = val
	end
end

local interrupt_handlers = {

}

function cpu_register_interrupt_handler(v, hnd)
	interrupt_handlers[v + 1] = hnd
end

local function cpu_int_fake(cond)
	local ax = CPU_REGS[1]
	local ah = (ax >> 8)
	local al = (ax & 0xFF)

	local h = interrupt_handlers[cond + 1]
	if h then
		local r = h(ax,ah,al)
		if r then
			return r
		end
	end

--	emu_debug(1, string.format("unknown interrupt: %02X AX=%04X", cond, ax))
end

local function cpu_int(cond)
	local addr = RAM:r16(cond * 4)
	local seg = RAM:r16(cond * 4 + 2)
#ifdef DEBUG_INTRS
	emu_debug(1, string.format("INT %02X AX=%04X", cond, CPU_REGS[1]))
#endif
	cpu_push16(CPU_FLAGS)
	cpu_push16(CPU_SEGMENTS[SEG_CS+1])
	cpu_push16(CPU_IP)

	CPU_SEGMENTS[SEG_CS+1]=seg
	CPU_IP=addr
	CPU_HALTED=false

	cpu_clear_flag(9)
end

local rel_jmp_conds = {
	function() return cpu_flag(11) end,
	function() return not cpu_flag(11) end,
	function() return cpu_flag(0) end,
	function() return not cpu_flag(0) end,
	function() return cpu_flag(6) end,
	function() return not cpu_flag(6) end,
	function() return cpu_flag(0) or cpu_flag(6) end,
	function() return not (cpu_flag(0) or cpu_flag(6)) end,
	function() return cpu_flag(7) end,
	function() return not cpu_flag(7) end,
	function() return cpu_flag(2) end,
	function() return not cpu_flag(2) end,
	function() return cpu_flag(11) ~= cpu_flag(7) end,
	function() return cpu_flag(11) == cpu_flag(7) end,
	function() return (cpu_flag(11) ~= cpu_flag(7)) or cpu_flag(6) end,
	function() return not ((cpu_flag(11) ~= cpu_flag(7)) or cpu_flag(6)) end
}

local function dump_memory()
	local f = io.open("mem", "wb")
	for i=0,0x9FFFF do
		f:write(string.char(RAM[i]))
	end
	f:close()
end

local opcode_map = {}

opcode_map[0x90] = function(opcode) end

-- ADD
opcode_map[0x00] = function(opcode) cpu_add(cpu_mod_rm6(opcode), opcode) end
for i=0x01,0x05 do opcode_map[i] = opcode_map[0x00] end

-- PUSH/POP ES
opcode_map[0x06] = function(opcode) cpu_push16(CPU_SEGMENTS[SEG_ES+1]) end
opcode_map[0x07] = function(opcode) CPU_SEGMENTS[SEG_ES+1] = cpu_pop16() end

-- OR
opcode_map[0x08] = function(opcode) cpu_or(cpu_mod_rm6(opcode), opcode) end
for i=0x09,0x0D do opcode_map[i] = opcode_map[0x08] end

-- PUSH CS
opcode_map[0x0E] = function(opcode) cpu_push16(CPU_SEGMENTS[SEG_CS+1]) end

-- ADC
opcode_map[0x10] = function(opcode) cpu_add(cpu_mod_rm6(opcode), opcode, true) end
for i=0x11,0x15 do opcode_map[i] = opcode_map[0x10] end

-- PUSH/POP SS
opcode_map[0x16] = function(opcode) cpu_push16(CPU_SEGMENTS[SEG_SS+1]) end
opcode_map[0x17] = function(opcode) CPU_SEGMENTS[SEG_SS+1] = cpu_pop16() end

-- SBB
opcode_map[0x18] = function(opcode) cpu_sub(cpu_mod_rm6(opcode), opcode, true) end
for i=0x19,0x1D do opcode_map[i] = opcode_map[0x18] end

-- PUSH/POP DS
opcode_map[0x1E] = function(opcode) cpu_push16(CPU_SEGMENTS[SEG_DS+1]) end
opcode_map[0x1F] = function(opcode) CPU_SEGMENTS[SEG_DS+1] = cpu_pop16() end

-- AND
opcode_map[0x20] = function(opcode) cpu_and(cpu_mod_rm6(opcode), opcode) end
for i=0x21,0x25 do opcode_map[i] = opcode_map[0x20] end

-- ES:
opcode_map[0x26] = function(opcode)
	CPU_SEGMOD = SEG_ES
	local r = run_one(true, true)
	CPU_SEGMOD = nil
	return r
end

-- DAA
opcode_map[0x27] = function(opcode)
	local al = CPU_REGS[1] & 0xFF
	local old_al = al
	local old_cf = cpu_flag(0)
	if ((al & 0x0F) > 0x9) or cpu_flag(4) then
		al = al + 0x6
		cpu_write_flag(0, old_cf or (al > 0xFF))
		cpu_set_flag(4)
	else
		cpu_clear_flag(4)
	end
	if ((al) > 0x99) or old_cf then
		al = al + 0x60
		cpu_set_flag(0)
	else
		cpu_clear_flag(0)
	end
	CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | (al & 0xFF)
	_cpu_uf_zsp(al, 0)
end

-- SUB
opcode_map[0x28] = function(opcode) cpu_sub(cpu_mod_rm6(opcode), opcode) end
for i=0x29,0x2D do opcode_map[i] = opcode_map[0x28] end

-- CS:
opcode_map[0x2E] = function(opcode)
	CPU_SEGMOD = SEG_CS
	local r = run_one(true, true)
	CPU_SEGMOD = nil
	return r
end

-- DAS
opcode_map[0x2F] = function(opcode)
	local al = CPU_REGS[1] & 0xFF
	local old_al = al
	local old_cf = cpu_flag(0)
	if ((al & 0x0F) > 0x9) or cpu_flag(4) then
		al = al - 0x6
		cpu_write_flag(0, old_cf or (al < 0))
		cpu_set_flag(4)
	else
		cpu_clear_flag(4)
	end
	if ((al) > 0x99) or old_cf then
		al = al - 0x60
		cpu_set_flag(0)
	else
		cpu_clear_flag(0)
	end
	CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | (al & 0xFF)
	_cpu_uf_zsp(al, 0)
end

-- XOR
opcode_map[0x30] = function(opcode) cpu_xor(cpu_mod_rm6(opcode), opcode) end
for i=0x31,0x35 do opcode_map[i] = opcode_map[0x30] end

-- SS:
opcode_map[0x36] = function(opcode)
	CPU_SEGMOD = SEG_SS
	local r = run_one(true, true)
	CPU_SEGMOD = nil
	return r
end

-- AAA
opcode_map[0x37] = function(opcode)
	local al = CPU_REGS[1] & 0xFF
	if ((al & 0x0F) >= 0x9) or cpu_flag(4) then
		CPU_REGS[1]  = (CPU_REGS[1] + 0x106) & 0xFFFF
		cpu_set_flag(0)
		cpu_set_flag(4)
	else
		cpu_clear_flag(0)
		cpu_clear_flag(4)
	end
	CPU_REGS[1] = (CPU_REGS[1] & 0xFF0F)
end

-- CMP
opcode_map[0x38] = function(opcode) cpu_sub(cpu_mod_rm6(opcode), opcode, false, true) end
for i=0x39,0x3D do opcode_map[i] = opcode_map[0x38] end

-- DS:
opcode_map[0x3E] = function(opcode)
	CPU_SEGMOD = SEG_DS
	local r = run_one(true, true)
	CPU_SEGMOD = nil
	return r
end

-- AAS
opcode_map[0x3F] = function(opcode)
	local al = CPU_REGS[1] & 0xFF
	if ((al & 0x0F) >= 0x9) or cpu_flag(4) then
		CPU_REGS[1] = (CPU_REGS[1] - 0x006) & 0xFFFF
		local ah = (CPU_REGS[1] & 0xFF00) >> 8
		ah = (ah - 1) & 0xFF
		CPU_REGS[1] = (CPU_REGS[1] & 0xFF) | (ah << 8)
		cpu_set_flag(0)
		cpu_set_flag(4)
	else
		cpu_clear_flag(0)
		cpu_clear_flag(4)
	end
	CPU_REGS[1] = (CPU_REGS[1] & 0xFF0F)
end

-- INC
opcode_map[0x40] = function(opcode)
	local v = CPU_REGS[(opcode & 0x07) + 1]
	v = (v + 1) & 0xFFFF
	CPU_REGS[(opcode & 0x07) + 1] = v
	_cpu_uf_inc(v, 1)
end
for i=0x41,0x47 do opcode_map[i] = opcode_map[0x40] end

-- DEC
opcode_map[0x48] = function(opcode)
	local v = CPU_REGS[(opcode & 0x07) + 1]
	v = (v - 1) & 0xFFFF
	CPU_REGS[(opcode & 0x07) + 1] = v
	_cpu_uf_dec(v, 1)
end
for i=0x49,0x4F do opcode_map[i] = opcode_map[0x48] end

-- PUSH/POP
opcode_map[0x50] = function(opcode) cpu_push16(CPU_REGS[(opcode & 0x07) + 1]) end
for i=0x51,0x57 do opcode_map[i] = opcode_map[0x50] end
opcode_map[0x58] = function(opcode) CPU_REGS[(opcode & 0x07) + 1] = cpu_pop16() end
for i=0x59,0x5F do opcode_map[i] = opcode_map[0x58] end

-- PUSH SP (8086/80186 bug reproduction)
opcode_map[0x54] = function(opcode) cpu_push16(CPU_REGS[5] - 2) end

-- JMP
for i=0x70,0x7F do
	local cond = rel_jmp_conds[i - 0x6F]
	opcode_map[i] = function(opcode)
		local offset = cpu_advance_ip()
		if cond() then
			CPU_IP = (CPU_IP + to_s8(offset)) & 0xFFFF
		end
	end
end

local grp1_table = {
	cpu_add,
	cpu_or,
	function(a,b) cpu_add(a,b,true) end,
	function(a,b) cpu_sub(a,b,true) end,
	cpu_and,
	function(a,b) cpu_sub(a,b,false) end,	
	cpu_xor,
	function(a,b) cpu_sub(a,b,false,true) end
}

-- GRP1
opcode_map[0x80] = function(opcode)
	local mrm_base = cpu_mod_rm(opcode & 0x01)
	local v = mrm_base.src & 0x07
	local mrm = {src=(40+(opcode & 0x01)),dst=mrm_base.dst,disp=mrm_base.disp}
	if opcode == 0x81 then
		mrm.imm = cpu_advance_ip16()
	elseif opcode == 0x83 then
		mrm.imm = to_s8(cpu_advance_ip()) & 0xFFFF
	else
		mrm.imm = cpu_advance_ip()
	end
	grp1_table[v+1](mrm, opcode)
end
for i=0x81,0x83 do opcode_map[i] = opcode_map[0x80] end

-- TEST
opcode_map[0x84] = function(opcode) cpu_and(cpu_mod_rm(opcode & 0x01), opcode, true) end
opcode_map[0x85] = opcode_map[0x84]

-- XCHG
opcode_map[0x86] = function(opcode)
	local mrm = cpu_mod_rm(opcode & 0x01)
	local t = cpu_read_rm(mrm, mrm.src)
	cpu_write_rm(mrm, mrm.src, cpu_read_rm(mrm, mrm.dst))
	cpu_write_rm(mrm, mrm.dst, t)
end
opcode_map[0x87] = opcode_map[0x86]

-- MOV mod/rm
opcode_map[0x88] = function(opcode) cpu_mov(cpu_mod_rm(opcode)) end
for i=0x89,0x8B do opcode_map[i] = opcode_map[0x88] end

-- MOV segment
opcode_map[0x8C] = function(opcode)
	local mrm = cpu_mod_rm(opcode, 1024)
	if mrm.dst == 26+SEG_CS then
		platform_error("Tried writing to CS segment!")
	end
	cpu_mov(mrm)
	-- Loading the SS register with a MOV inhibits all interrupts until after the next instruction,
	-- so let's just call an extra instruction marked non-interruptible here
	if mrm.dst == 26+SEG_SS then
		return run_one(true, true)
	end
end
opcode_map[0x8E] = opcode_map[0x8C]

-- LEA
opcode_map[0x8D] = function(opcode)
	local mrm = cpu_mod_rm(3)
	cpu_write_rm(mrm, mrm.dst, cpu_addr_rm(mrm, mrm.src))
end

-- POP m16
opcode_map[0x8F] = function(opcode)
	local mrm = cpu_mod_rm(1)
	cpu_write_rm(mrm, mrm.dst, cpu_pop16())
end

-- XCHG (XCHG AX, AX == NOP)
opcode_map[0x91] = function(opcode)
	local t = CPU_REGS[1]
	CPU_REGS[1] = CPU_REGS[opcode - 0x8F]
	CPU_REGS[opcode - 0x8F] = t
end
for i=0x92,0x97 do opcode_map[i] = opcode_map[0x91] end

-- CBW
opcode_map[0x98] = function(opcode)
	local v = CPU_REGS[1] & 0xFF
	if v >= 0x80 then v = v | 0xFF00 end
	CPU_REGS[1] = v
end

-- CWD
opcode_map[0x99] = function(opcode)
	local v = 0x0000
	if CPU_REGS[1] >= 0x8000 then v = 0xFFFF end
	CPU_REGS[3] = v
end


-- CALL far
opcode_map[0x9A] = function(opcode)
	local newIp = cpu_advance_ip16()
	local newCs = cpu_advance_ip16()
	cpu_push16(CPU_SEGMENTS[SEG_CS+1])
	cpu_push16(CPU_IP)
	CPU_IP = newIp
	CPU_SEGMENTS[SEG_CS+1] = newCs
end

-- PUSHF/POPF
opcode_map[0x9C] = function(opcode) cpu_push16(CPU_FLAGS) end
opcode_map[0x9D] = function(opcode) CPU_FLAGS = cpu_pop16() | 0xF002 end

-- SAHF/LAHF
opcode_map[0x9E] = function(opcode)
		CPU_FLAGS = (CPU_FLAGS & 0xFF00) | ((CPU_REGS[1] & 0xFF00) >> 8)
end
opcode_map[0x9F] = function(opcode)
		CPU_REGS[1] = (CPU_REGS[1] & 0xFF) | ((CPU_FLAGS & 0xFF) << 8)
end

-- MOV offs->AL
opcode_map[0xA0] = function(opcode)
	local addr = cpu_advance_ip16()
	CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | RAM[segmd(SEG_DS, addr)]
end

-- MOV offs->AX
opcode_map[0xA1] = function(opcode)
	local addr = cpu_advance_ip16()
	CPU_REGS[1] = RAM:r16(segmd(SEG_DS, addr))
end

-- MOV AL->offs
opcode_map[0xA2] = function(opcode)
	local addr = cpu_advance_ip16()
	RAM[segmd(SEG_DS, addr)] = CPU_REGS[1] & 0xFF
end

-- MOV AX->offs
opcode_map[0xA3] = function(opcode)
	local addr = cpu_advance_ip16()
	RAM:w16(segmd(SEG_DS, addr), CPU_REGS[1])
end

-- MOVSB/MOVSW
opcode_map[0xA4] = function(opcode)
	local addrSrc = segmd(SEG_DS, CPU_REGS[7])
	local addrDst = seg(SEG_ES, CPU_REGS[8])
	RAM[addrDst] = RAM[addrSrc]
	cpu_incdec_dir(7, 1)
	cpu_incdec_dir(8, 1)
end
opcode_map[0xA5] = function(opcode)
	local addrSrc = segmd(SEG_DS, CPU_REGS[7])
	local addrDst = seg(SEG_ES, CPU_REGS[8])
	RAM:w16(addrDst, RAM:r16(addrSrc))
	cpu_incdec_dir(7, 2)
	cpu_incdec_dir(8, 2)
end

-- CMPSB/CMPSW
opcode_map[0xA6] = function(opcode)
	local addrSrc = segmd(SEG_DS, CPU_REGS[7])
	local addrDst = seg(SEG_ES, CPU_REGS[8])
	cpu_cmp(RAM[addrSrc], RAM[addrDst], opcode)
	cpu_incdec_dir(7, 1)
	cpu_incdec_dir(8, 1)
end
opcode_map[0xA7] = function(opcode)
	local addrSrc = segmd(SEG_DS, CPU_REGS[7])
	local addrDst = seg(SEG_ES, CPU_REGS[8])
	cpu_cmp(RAM:r16(addrSrc), RAM:r16(addrDst), opcode)
	cpu_incdec_dir(7, 2)
	cpu_incdec_dir(8, 2)
end

-- TEST AL, imm8
opcode_map[0xA8] = function(opcode) cpu_and({src=40,dst=16,imm=cpu_advance_ip()}, 0, true) end
-- TEST AX, imm16
opcode_map[0xA9] = function(opcode) cpu_and({src=41,dst=0,imm=cpu_advance_ip16()}, 1, true) end

-- STOSB/STOSW
opcode_map[0xAA] = function(opcode)
	local addrDst = seg(SEG_ES, CPU_REGS[8])
	RAM[addrDst] = CPU_REGS[1] & 0xFF
	cpu_incdec_dir(8, 1)
end
opcode_map[0xAB] = function(opcode)
	local addrDst = seg(SEG_ES, CPU_REGS[8])
	RAM:w16(addrDst, CPU_REGS[1])
	cpu_incdec_dir(8, 2)
end

-- LODSB/LODSW
opcode_map[0xAC] = function(opcode)
	local addrSrc = segmd(SEG_DS, CPU_REGS[7])
	CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | RAM[addrSrc]
	cpu_incdec_dir(7, 1)
end
opcode_map[0xAD] = function(opcode)
	local addrSrc = segmd(SEG_DS, CPU_REGS[7])
	CPU_REGS[1] = RAM:r16(addrSrc)
	cpu_incdec_dir(7, 2)
end

-- SCASB/SCASW
opcode_map[0xAE] = function(opcode)
	local addrDst = seg(SEG_ES, CPU_REGS[8])
	cpu_cmp(CPU_REGS[1] & 0xFF, RAM[addrDst], opcode)
	cpu_incdec_dir(8, 1)
end
opcode_map[0xAF] = function(opcode)
	local addrDst = seg(SEG_ES, CPU_REGS[8])
	cpu_cmp(CPU_REGS[1], RAM:r16(addrDst), opcode)
	cpu_incdec_dir(8, 2)
end

-- MOV imm8
opcode_map[0xB0] = function(opcode)
	local r = opcode - 0xAF
	if r >= 5 then
		r = r - 4
		CPU_REGS[r] = (CPU_REGS[r] & 0xFF) | ((cpu_advance_ip() & 0xFF) << 8)
	else
		CPU_REGS[r] = (CPU_REGS[r] & 0xFF00) | (cpu_advance_ip() & 0xFF)
	end
end
for i=0xB1,0xB7 do opcode_map[i] = opcode_map[0xB0] end

-- MOV imm16
opcode_map[0xB8] = function(opcode)
		CPU_REGS[opcode - 0xB7] = cpu_advance_ip16()
end
for i=0xB9,0xBF do opcode_map[i] = opcode_map[0xB8] end

-- RET near + pop
opcode_map[0xC2] = function(opcode)
	local btp = cpu_advance_ip16()
	CPU_IP = cpu_pop16()
	CPU_REGS[5] = CPU_REGS[5] + btp
end

-- RET near
opcode_map[0xC3] = function(opcode)
	CPU_IP = cpu_pop16()
end

-- LES/LDS
opcode_map[0xC4] = function(opcode)
	local mrm = cpu_mod_rm(3)
	local addr = cpu_addr_rm(mrm, mrm.src)
	local defseg = cpu_seg_rm(mrm, mrm.src)
	cpu_write_rm(mrm, mrm.dst, RAM:r16(segmd(defseg,addr)))
	if opcode == 0xC5 then
		CPU_SEGMENTS[SEG_DS+1] = RAM:r16(segmd(defseg,addr + 2))
	else
		CPU_SEGMENTS[SEG_ES+1] = RAM:r16(segmd(defseg,addr + 2))
	end
end
opcode_map[0xC5] = opcode_map[0xC4]

-- MOV imm(rm)
opcode_map[0xC6] = function(opcode)
	local mrm = cpu_mod_rm(opcode ~ 0x02)
	if opcode == 0xC6 then cpu_write_rm(mrm, mrm.dst, cpu_advance_ip())
	else cpu_write_rm(mrm, mrm.dst, cpu_advance_ip16()) end
end
opcode_map[0xC7] = opcode_map[0xC6]

-- RET far + pop
opcode_map[0xCA] = function(opcode)
	local btp = cpu_advance_ip16()
	CPU_IP = cpu_pop16()
	CPU_SEGMENTS[SEG_CS+1] = cpu_pop16()
	CPU_REGS[5] = CPU_REGS[5] + btp
end

-- RET far
opcode_map[0xCB] = function(opcode)
	CPU_IP = cpu_pop16()
	CPU_SEGMENTS[SEG_CS+1] = cpu_pop16()
end

-- INT 3
opcode_map[0xCC] = function(opcode)
--	cpu_int(3)
	dump_memory()
	platform_error("breakpoint")
end

-- INT imm
opcode_map[0xCD] = function(opcode) cpu_int(cpu_advance_ip()) end

-- INTO
opcode_map[0xCE] = function(opcode)
	if cpu_flag(11) then
		cpu_int(4)
	end
end

-- IRET far
opcode_map[0xCF] = function(opcode)
	CPU_IP = cpu_pop16()
	CPU_SEGMENTS[SEG_CS+1] = cpu_pop16()
	CPU_FLAGS = cpu_pop16()
end

local grp2_table = {
	function(a,b) cpu_rotate(a,b,ROTATE_MODE_ROL) end,
	function(a,b) cpu_rotate(a,b,ROTATE_MODE_ROR) end,
	function(a,b) cpu_rotate(a,b,ROTATE_MODE_RCL) end,
	function(a,b) cpu_rotate(a,b,ROTATE_MODE_RCR) end,
	cpu_shl,
	cpu_shr,
	cpu_shl, -- SAL
	function(a,b) cpu_shr(a,b,true) end
}

-- GRP2 (C0, C1, D0, D1, D2, D3)
-- TODO: verify C0/C1 are 8086
opcode_map[0xC0] = function(opcode)
	local mrm = cpu_mod_rm_copy(opcode & 0x01)
	local v = mrm.src & 0x07
--	emu_debug(0, "GRP2/"..v)

	if (opcode & 0xFE) == 0xC0 then -- C0/C1 - imm8
		mrm.src = 40
		mrm.imm = cpu_advance_ip()
	elseif (opcode & 0x02) == 2 then -- D2/D3 - CL
		mrm.src = 17
	else -- D0/D1 - 1
		mrm.src = 40
		mrm.imm = 1
	end

	grp2_table[v+1](mrm,opcode)
end

opcode_map[0xC1] = opcode_map[0xC0]
opcode_map[0xD0] = opcode_map[0xC0]
opcode_map[0xD1] = opcode_map[0xC0]
opcode_map[0xD2] = opcode_map[0xC0]
opcode_map[0xD3] = opcode_map[0xC0]

-- AAM
opcode_map[0xD4] = function(opcode)
	local base = cpu_advance_ip()
	local old_al = CPU_REGS[1] & 0xFF
	local ah = math.floor(old_al / base)
	local al = old_al % base
	CPU_REGS[1] = ((ah & 0xFF) << 8) | (al & 0xFF)
	_cpu_uf_zsp(al, 0)
end

-- AAD
opcode_map[0xD5] = function(opcode)
	local base = cpu_advance_ip()
	local old_al = CPU_REGS[1] & 0xFF
	local old_ah = (CPU_REGS[1] >> 8) & 0xFF
	CPU_REGS[1] = (old_al + (old_ah * base)) & 0xFF
	_cpu_uf_zsp(CPU_REGS[1], 0)
end

-- SALC (undocumented)
opcode_map[0xD6] = function(opcode)
	if cpu_flag(0) then
		CPU_REGS[1] = CPU_REGS[1] | 0xFF
	else
		CPU_REGS[1] = CPU_REGS[1] & 0xFF00
	end
end

-- XLAT
opcode_map[0xD7] = function(opcode)
	local addr = (CPU_REGS[4] + (CPU_REGS[1] & 0xFF)) & 0xFFFF
	CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | RAM[segmd(SEG_DS,addr)]
end

-- LOOPNZ r8
opcode_map[0xE0] = function(opcode)
	local offset = to_s8(cpu_advance_ip())
	CPU_REGS[2] = CPU_REGS[2] - 1
	if CPU_REGS[2] ~= 0 and not cpu_flag(6) then
		CPU_IP = (CPU_IP + offset) & 0xFFFF
	end
end
-- LOOPZ r8
opcode_map[0xE1] = function(opcode)
	local offset = to_s8(cpu_advance_ip())
	CPU_REGS[2] = CPU_REGS[2] - 1
	if CPU_REGS[2] ~= 0 and cpu_flag(6) then
		CPU_IP = (CPU_IP + offset) & 0xFFFF
	end
end
-- LOOP r8
opcode_map[0xE2] = function(opcode)
	local offset = to_s8(cpu_advance_ip())
	CPU_REGS[2] = CPU_REGS[2] - 1
	if CPU_REGS[2] ~= 0 then
		CPU_IP = (CPU_IP + offset) & 0xFFFF
	end
end
-- JCXZ r8
opcode_map[0xE3] = function(opcode)
	local offset = to_s8(cpu_advance_ip())
	if CPU_REGS[2] == 0 then
		CPU_IP = (CPU_IP + offset) & 0xFFFF
	end
end

-- IN AL, Ib
opcode_map[0xE4] = function(opcode)
	CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | (cpu_in(cpu_advance_ip()) & 0xFF)
end

-- IN AX, Ib
opcode_map[0xE5] = function(opcode)
	CPU_REGS[1] = (cpu_in(cpu_advance_ip()) & 0xFFFF)
end

-- OUT AL, Ib
opcode_map[0xE6] = function(opcode)
	cpu_out(cpu_advance_ip(), CPU_REGS[1] & 0xFF)
end

-- OUT AX, Ib
opcode_map[0xE7] = function(opcode)
	cpu_out(cpu_advance_ip(), CPU_REGS[1])
end

-- CALL rel16
opcode_map[0xE8] = function(opcode)
	local offset = to_s16(cpu_advance_ip16())
	cpu_push16(CPU_IP)
	CPU_IP = (CPU_IP + offset) & 0xFFFF
end

-- JMP rel16
opcode_map[0xE9] = function(opcode)
	local offset = to_s16(cpu_advance_ip16())
	CPU_IP = (CPU_IP + offset) & 0xFFFF
end

-- JMP ptr
opcode_map[0xEA] = function(opcode)
	local newIp = cpu_advance_ip16()
	local newCs = cpu_advance_ip16()
	CPU_IP = newIp
	CPU_SEGMENTS[SEG_CS+1] = newCs
end

-- JMP r8
opcode_map[0xEB] = function(opcode)
	local offset = to_s8(cpu_advance_ip())
	CPU_IP = (CPU_IP + offset) & 0xFFFF
end

-- IN AL, DX
opcode_map[0xEC] = function(opcode)
	CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | (cpu_in(CPU_REGS[3]) & 0xFF)
end

-- IN AX, DX
opcode_map[0xED] = function(opcode)
	CPU_REGS[1] = (cpu_in(CPU_REGS[3]) & 0xFFFF)
end

-- OUT AL, DX
opcode_map[0xEE] = function(opcode)
	cpu_out(CPU_REGS[3], CPU_REGS[1] & 0xFF)
end

-- OUT AX, DX
opcode_map[0xEF] = function(opcode)
	cpu_out(CPU_REGS[3], CPU_REGS[1])
end

-- LOCK
opcode_map[0xF0] = function(opcode)
	-- We're not hardware, we can ignore this
end

-- REPNZ
opcode_map[0xF2] = function(opcode)
	if not cpu_rep(function() return not cpu_flag(6) end) then return false end
end

-- REPZ
opcode_map[0xF3] = function(opcode)
	if not cpu_rep(function() return cpu_flag(6) end) then return false end
end

-- HLT
opcode_map[0xF4] = function(opcode)
	CPU_HALTED = true
	return "block"
end

-- CMC
opcode_map[0xF5] = function(opcode) cpu_complement_flag(0) end

-- GRP3
opcode_map[0xF6] = function(opcode)
	local mrm = cpu_mod_rm_copy(opcode & 0x01)
	local v = mrm.src & 0x07
--	emu_debug(0, "GRP3/"..v)

	if (opcode & 0x01) == 1 then mrm.src = 0 else mrm.src = 16 end

	if	v == 0 then
		if opcode == 0xF7 then
			mrm.src = 41
			mrm.imm = cpu_advance_ip16()
		else
			mrm.src = 40
			mrm.imm = cpu_advance_ip()
		end
		cpu_and(mrm, opcode, true)
	elseif	v == 2 then -- NOT
		if (opcode & 0x01) == 1 then
			cpu_write_rm(mrm, mrm.dst, cpu_read_rm(mrm, mrm.dst) ~ 0xFFFF)
		else
			cpu_write_rm(mrm, mrm.dst, cpu_read_rm(mrm, mrm.dst) ~ 0xFF)
		end
		-- does not set flags
	elseif	v == 3 then -- NEG
		local src = cpu_read_rm(mrm, mrm.dst)
		local result = 0
		if (opcode & 0x01) == 1 then
			result = ((src ~ 0xFFFF) + 1) & 0xFFFF
			cpu_write_flag(11, src == 0x8000)
		else
			result = ((src ~ 0xFF) + 1) & 0xFF
			cpu_write_flag(11, src == 0x80)
		end
		cpu_write_rm(mrm, mrm.dst, result)
		cpu_write_flag(0, src ~= 0)
		cpu_write_flag(4, ((src ~ result) & 0x10) ~= 0)
		_cpu_uf_zsp(result, opcode)
	elseif	v == 4 then cpu_mul(mrm, opcode)
	elseif	v == 5 then cpu_imul(mrm, opcode)
	elseif	v == 6 then cpu_div(mrm, opcode)
	elseif	v == 7 then cpu_idiv(mrm, opcode)
	else platform_error("GRP3 todo: " .. v) end
end
opcode_map[0xF7] = opcode_map[0xF6]

-- flag setters
opcode_map[0xF8] = function(opcode)  cpu_clear_flag(0) end
opcode_map[0xF9] = function(opcode)    cpu_set_flag(0) end
opcode_map[0xFA] = function(opcode)  cpu_clear_flag(9) end
opcode_map[0xFB] = function(opcode)    cpu_set_flag(9) end
opcode_map[0xFC] = function(opcode)  cpu_clear_flag(10) end
opcode_map[0xFD] = function(opcode)    cpu_set_flag(10) end

-- GRP4
opcode_map[0xFE] = function(opcode)
	local mrm = cpu_mod_rm(0)
	local v = mrm.src & 0x07
--	emu_debug(0, "GRP4/"..v)

	if	v == 0 then -- INC
		local v = (cpu_read_rm(mrm,mrm.dst) + 1) & 0xFF
		cpu_write_rm(mrm,mrm.dst,v)
		_cpu_uf_inc(v, 0)
	elseif	v == 1 then -- DEC
		local v = (cpu_read_rm(mrm,mrm.dst) - 1) & 0xFF
		cpu_write_rm(mrm,mrm.dst,v)
		_cpu_uf_dec(v, 0)
	else platform_error("GRP4 todo: " .. v) end
end

-- GRP5
opcode_map[0xFF] = function(opcode)
	local mrm = cpu_mod_rm(1)
	local v = mrm.src & 0x07
--	emu_debug(0,"GRP5/"..v)

	if	v == 0 then -- INC
		local v = (cpu_read_rm(mrm,mrm.dst) + 1) & 0xFFFF
		cpu_write_rm(mrm,mrm.dst,v)
		_cpu_uf_inc(v, 1)
	elseif	v == 1 then -- DEC
		local v = (cpu_read_rm(mrm,mrm.dst) - 1) & 0xFFFF
		cpu_write_rm(mrm,mrm.dst,v)
		_cpu_uf_dec(v, 1)
	elseif	v == 2 then -- CALL near abs
		local newIp = cpu_read_rm(mrm,mrm.dst)
		cpu_push16(CPU_IP)
		CPU_IP = newIp
	elseif	v == 3 then -- CALL far abs
		local addr = segmd(cpu_seg_rm(mrm,mrm.dst),cpu_addr_rm(mrm,mrm.dst))
		local newIp = RAM:r16(addr)
		local newCs = RAM:r16(addr+2)
		cpu_push16(CPU_SEGMENTS[SEG_CS+1])
		cpu_push16(CPU_IP)
		CPU_IP = newIp
		CPU_SEGMENTS[SEG_CS+1] = newCs
	elseif	v == 4 then -- JMP near abs
		CPU_IP = cpu_read_rm(mrm,mrm.dst)
	elseif	v == 5 then -- JMP far
		local addr = segmd(cpu_seg_rm(mrm,mrm.dst),cpu_addr_rm(mrm,mrm.dst))
		local newIp = RAM:r16(addr)
		local newCs = RAM:r16(addr+2)
		CPU_IP = newIp
		CPU_SEGMENTS[SEG_CS+1] = newCs
	elseif	v == 6 then cpu_push16(cpu_read_rm(mrm,mrm.dst))
	else platform_error("GRP5 todo: " .. v) end
end

function run_one(no_interrupting, pr_state)
	if not no_interrupting and (#CPU_INTQUEUE > 0) then
--		local intr = table.remove(CPU_INTQUEUE, 1)
		local intr = CPU_INTQUEUE[1]
		if intr ~= nil then
			if intr >= 256 or cpu_flag(9) then
				table.remove(CPU_INTQUEUE, 1)
				cpu_int(intr & 0xFF)
			end
		end
	end

	if CPU_HALTED then return "block" end

	if (CPU_SEGMENTS[SEG_CS+1] == 0xF000) and ((CPU_IP & 0xFF00) == 0x1100) then
		cpu_set_flag(9) -- enable interrupts during (after!) fake handlers
		local intr = cpu_int_fake(CPU_IP & 0xFF)
		if intr ~= "block" then
			CPU_IP = cpu_pop16()
			CPU_SEGMENTS[SEG_CS+1] = cpu_pop16()
			local old_flags = cpu_pop16()
			local old_flag_mask = 0x0200
			CPU_FLAGS = (CPU_FLAGS & (~old_flag_mask)) | (old_flags & old_flag_mask)
			return true
		else
			return "block"
		end
	end

	local opcode = cpu_advance_ip()
--	if pr_state then cpu_print_state(opcode) end

	local om = opcode_map[opcode]

	if om ~= nil then
		local result = om(opcode)
		if result ~= nil then
			return result
		end
	else
		emu_debug(2, "unknown opcode: " .. string.format("%02X", opcode))
		cpu_print_state(opcode)
		cpu_emit_interrupt(6, false)
	end

	return true
end

function cpu_emit_interrupt(v, nmi)
	nmi = (v == 2)
	if nmi then
		table.insert(CPU_INTQUEUE, v + 256)
	else
		table.insert(CPU_INTQUEUE, v)
	end
end

-- invalid opcode interrupt
cpu_register_interrupt_handler(0x06, function(ax,ah,al)
	-- dummy - override by user
	return true
end)

-- keyboard interrupt
cpu_register_interrupt_handler(0x09, function(ax,ah,al)
	-- dummy - override by user
	return true
end)

dofile("pic.lua")
dofile("pit.lua")
dofile("timer.lua")
dofile("video.lua")
dofile("disks.lua")
dofile("keyboard.lua")
dofile("sysconf.lua")
dofile("comms.lua")

local last_opc = 0
local opc_avg = {}
local opc_avg_i = 0
local clock = os.clock()

local function upd_count(last_clock)
	opc_avg_i = (opc_avg_i + 1) % 20
	opc_avg[opc_avg_i + 1] = ((opc - last_opc) / (clock - last_clock))

	local oa = 0
	local oac = 0
	for i=1,20 do
		if opc_avg[i] then
			oa = oa + opc_avg[i]
			oac = oac + 1
		end
	end

	emu_debug(2, (oa / oac) .. " IPS")
	opc = last_opc
end

local oc_tick_i = 0

local function upd_tick(cv)
	local last_clock = clock
	clock = cv
#ifdef DEBUG_IPS
	upd_count(last_clock)
#endif
	-- handle pc speaker
	if (kbd_get_spkr_latch() & 0x03) == 0x03 then
		local pch = pit_channel(3)
		if (pch.mode == 2 or pch.mode == 3) and (pch.reload_set_lo and pch.reload_set_hi) then
			local freq = (pit_osc_freq * 1000000) / pch.reload
			platform_beep(freq)
		end
	end
	-- handle video
	video_update()
	keyboard_update()
	-- handle OC waits
	cv = os.clock()
	if (cv - clock) < 0.05 then
		oc_tick_i = oc_tick_i + 1
--		if (oc_tick_i % 3) == 0 then
			platform_sleep(0)
--		end
	end
	clock = cv
	-- handle interrupt
	for i=last_clock,clock,0.05 do
		if pic_interrupt_enabled(PIC_INTERRUPT_TIMER) then
			cpu_emit_interrupt(0x08, false)
		end
	end
end

local function cpu_execute()
	execute = true
	while execute == true do
		execute = run_one(false, true)
		if execute == "block" then
			upd_tick(os.clock())
			execute = true
		end
		if ((opc & 0x1FF) == 0) and (os.clock() - clock) >= 0.05 then
			upd_tick(os.clock())
		end
		opc = opc + 1
	end
	platform_error("execution stopped\n")
end

function emu_execute()
	CPU_FLAGS = 0x0202
	sysconf_init()
	cpu_execute()
end
