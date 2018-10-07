












local ram_640k = {}
local io_ports = {}
local ram_rom = {}
local run_one = nil
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
    local k = ((key - 0xF0000) >> rmm) + 1
    return ((ram_rom[k] or 0) >> ((key & rmm_mask) << 3)) & 0xFF
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
    local shift = ((key & rmm_mask) << 3)
    local k = ((key - 0xF0000) >> rmm) + 1
    local mask = 255 << shift
    local nmask = mask ~ (-1)
    ram_rom[k] = ((ram_rom[k] or 0) & nmask) | ((value & 0xFF) << shift)
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
local CPU_HASINT = false

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


function segmd(s, v)
 return (CPU_SEGMENTS[(CPU_SEGMOD or (s+1))]<<4) + v
end


local function cpu_advance_ip()
 local ip = ((CPU_SEGMENTS[(SEG_CS)+1]<<4)+(CPU_IP))
 CPU_IP = (CPU_IP + 1) & 0xFFFF
 return RAM[ip]
end

local function cpu_advance_ip16()
 local ip = ((CPU_SEGMENTS[(SEG_CS)+1]<<4)+(CPU_IP))
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


function cpu_clear_flag(t)
 CPU_FLAGS = CPU_FLAGS & (~(1 << t))
end


function cpu_set_flag(t)
 CPU_FLAGS = CPU_FLAGS | (1 << t)
end


function cpu_write_flag(t, v)
 if v then
  CPU_FLAGS = CPU_FLAGS | (1 << t)
 else
  CPU_FLAGS = CPU_FLAGS & (~(1 << t))
 end
end
local cpu_write_flag = cpu_write_flag

function cpu_complement_flag(t)
 CPU_FLAGS = CPU_FLAGS ~ (1 << t)
end


local function cpu_incdec_dir(t, amount)
 if ((CPU_FLAGS & (1<<(10))) ~= 0) then
  CPU_REGS[t] = (CPU_REGS[t] - amount) & 0xFFFF
 else
  CPU_REGS[t] = (CPU_REGS[t] + amount) & 0xFFFF
 end
end
local function cpu_print(t)
 emu_debug(1, string.format("[%04X %04X] ", CPU_SEGMENTS[2], (CPU_IP-1) & 0xFFFF) .. t)
end

function cpu_set_ip(vcs, vip)
 CPU_SEGMENTS[2] = vcs
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

local function _cpu_rm_seg(v)
 return _cpu_rm_seg_t[v]
end


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
 return _cpu_rm_addr_t[v](data)
end


local function cpu_seg_rm(data, v)
 if v >= 8 and v < 16 then
  return _cpu_rm_seg_t[(v - 7)]
 elseif v >= 32 and v < 40 then
  return _cpu_rm_seg_t[(v - 31)]
 else return SEG_DS end
end

local function cpu_addr_rm(data, v)
 if v >= 8 and v < 16 then
  return _cpu_rm_addr_t[(v - 7)]((data))
 elseif v >= 24 and v <= 25 then
  return data.disp
 elseif v >= 32 and v < 40 then
  return _cpu_rm_addr_t[(v - 31)]((data))
 else
  platform_error("cpu_addr_rm todo: " .. v)
 end
end

local readrm_table = {}
readrm_table[0] = function(data, v) return CPU_REGS[1] end
readrm_table[1] = function(data, v) return CPU_REGS[2] end
readrm_table[2] = function(data, v) return CPU_REGS[3] end
readrm_table[3] = function(data, v) return CPU_REGS[4] end
readrm_table[4] = function(data, v) return CPU_REGS[5] end
readrm_table[5] = function(data, v) return CPU_REGS[6] end
readrm_table[6] = function(data, v) return CPU_REGS[7] end
readrm_table[7] = function(data, v) return CPU_REGS[8] end
for i=8,15 do readrm_table[i] = function(data, v)
 return RAM:r16(((CPU_SEGMENTS[(CPU_SEGMOD or (_cpu_rm_seg_t[(v - 7)]+1))]<<4) + (_cpu_rm_addr_t[(v - 7)]((data)))))
end end
readrm_table[16] = function(data, v) return CPU_REGS[1] & 0xFF end
readrm_table[17] = function(data, v) return CPU_REGS[2] & 0xFF end
readrm_table[18] = function(data, v) return CPU_REGS[3] & 0xFF end
readrm_table[19] = function(data, v) return CPU_REGS[4] & 0xFF end
readrm_table[20] = function(data, v) return CPU_REGS[1] >> 8 end
readrm_table[21] = function(data, v) return CPU_REGS[2] >> 8 end
readrm_table[22] = function(data, v) return CPU_REGS[3] >> 8 end
readrm_table[23] = function(data, v) return CPU_REGS[4] >> 8 end
readrm_table[24] = function(data, v) return RAM[((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (data.disp))] end
readrm_table[25] = function(data, v) return RAM:r16(((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (data.disp))) end
for i=26,31 do readrm_table[i] = function(data, v)
 return CPU_SEGMENTS[v - 25]
end end
for i=32,39 do readrm_table[i] = function(data, v)
 return RAM[((CPU_SEGMENTS[(CPU_SEGMOD or (_cpu_rm_seg_t[(v - 31)]+1))]<<4) + (_cpu_rm_addr_t[(v - 31)]((data))))]
end end
readrm_table[40] = function(data, v) return data.imm & 0xFF end
readrm_table[41] = function(data, v) return data.imm end

local function cpu_read_rm(data, v)
 return readrm_table[v](data, v)
end

local writerm_table = {}
writerm_table[0] = function(data, v, val) CPU_REGS[1] = val end
writerm_table[1] = function(data, v, val) CPU_REGS[2] = val end
writerm_table[2] = function(data, v, val) CPU_REGS[3] = val end
writerm_table[3] = function(data, v, val) CPU_REGS[4] = val end
writerm_table[4] = function(data, v, val) CPU_REGS[5] = val end
writerm_table[5] = function(data, v, val) CPU_REGS[6] = val end
writerm_table[6] = function(data, v, val) CPU_REGS[7] = val end
writerm_table[7] = function(data, v, val) CPU_REGS[8] = val end
for i=8,15 do writerm_table[i] = function(data, v, val)
 RAM:w16(((CPU_SEGMENTS[(CPU_SEGMOD or (_cpu_rm_seg_t[(v - 7)]+1))]<<4) + (_cpu_rm_addr_t[(v - 7)]((data)))), val)
end end
writerm_table[16] = function(data, v, val) CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | (val & 0xFF) end
writerm_table[17] = function(data, v, val) CPU_REGS[2] = (CPU_REGS[2] & 0xFF00) | (val & 0xFF) end
writerm_table[18] = function(data, v, val) CPU_REGS[3] = (CPU_REGS[3] & 0xFF00) | (val & 0xFF) end
writerm_table[19] = function(data, v, val) CPU_REGS[4] = (CPU_REGS[4] & 0xFF00) | (val & 0xFF) end
writerm_table[20] = function(data, v, val) CPU_REGS[1] = (CPU_REGS[1] & 0xFF) | ((val & 0xFF) << 8) end
writerm_table[21] = function(data, v, val) CPU_REGS[2] = (CPU_REGS[2] & 0xFF) | ((val & 0xFF) << 8) end
writerm_table[22] = function(data, v, val) CPU_REGS[3] = (CPU_REGS[3] & 0xFF) | ((val & 0xFF) << 8) end
writerm_table[23] = function(data, v, val) CPU_REGS[4] = (CPU_REGS[4] & 0xFF) | ((val & 0xFF) << 8) end
writerm_table[24] = function(data, v, val)
 RAM[((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (data.disp))] = val & 0xFF
end
writerm_table[25] = function(data, v, val)
 RAM:w16(((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (data.disp)), val)
end
for i=26,31 do writerm_table[i] = function(data, v, val)
 CPU_SEGMENTS[v - 25] = val
end end
for i=32,39 do writerm_table[i] = function(data, v, val)
 RAM[((CPU_SEGMENTS[(CPU_SEGMOD or (_cpu_rm_seg_t[(v - 31)]+1))]<<4) + (_cpu_rm_addr_t[(v - 31)]((data))))] = val & 0xFF
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
 mrm_table[i] = {src=src,dst=dst,cdisp=cdisp,disp=0}
end

local function cpu_mod_rm(opcode, is_seg)
 local modrm = cpu_advance_ip() | ((opcode & 3) << 8) | (is_seg or 0)
 local data = mrm_table[modrm]

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

local function cpu_mrm_copy(data)
 return {src=data.src,dst=data.dst,disp=data.disp}
end

local mrm6_4 = {src=40,dst=16,imm=0}
local mrm6_5 = {src=41,dst=0,imm=0}

local mrm6_table = {
 [0]=cpu_mod_rm,
 [1]=cpu_mod_rm,
 [2]=cpu_mod_rm,
 [3]=cpu_mod_rm,
 [4]=function(v)
  mrm6_4.imm=cpu_advance_ip()
  return mrm6_4
 end,
 [5]=function(v)
  mrm6_5.imm=cpu_advance_ip16()
  return mrm6_5
 end,
 [6]=cpu_mod_rm,
 [7]=cpu_mod_rm
}

local function cpu_mod_rm6(opcode)
 local v = opcode & 0x07
 return mrm6_table[v](v)
end


local parity_table = {}
for i = 0,255 do
 local p = 0
 local v = i
 while v ~= 0 do
  p = p + (v & 1)
  v = v >> 1
 end
 if (p & 1) == 0 then
  parity_table[i] = 4
 else
  parity_table[i] = 0
 end
end

local function cpu_write_parity(v)
 CPU_FLAGS = CPU_FLAGS & 0xFFFB | parity_table[v & 0xFF]
end


local function cpu_push16(v)
 CPU_REGS[5] = (CPU_REGS[5] - 2) & 0xFFFF
 RAM:w16(((CPU_SEGMENTS[(SEG_SS)+1]<<4)+(CPU_REGS[5])), v & 0xFFFF)
 --emu_debug(0, string.format("stack: >%04X @%04X\n", v, CPU_REGS[5]))
end

local function cpu_pop16()
 local sp = CPU_REGS[5]
 CPU_REGS[5] = (sp + 2) & 0xFFFF
 --emu_debug(0, string.format("stack: <%04X @%04X\n", RAM:r16(((CPU_SEGMENTS[(SEG_SS)+1]<<4)+(sp))), sp))
 return RAM:r16(((CPU_SEGMENTS[(SEG_SS)+1]<<4)+(sp)))
end

local function cpu_mov(mrm)
 local v1 = cpu_read_rm(mrm, mrm.src)
 cpu_write_rm(mrm, mrm.dst, v1)
end

local function _cpu_uf_zsp(vr, opc)
 if (opc & 0x01) == 1 then
  cpu_write_flag(6, (vr & 0xFFFF) == 0)
  cpu_write_flag(7, (vr & 0x8000) ~= 0)
  CPU_FLAGS = CPU_FLAGS & 0xFFFB | parity_table[vr & 0xFF]
 else
  cpu_write_flag(6, (vr & 0xFF) == 0)
  cpu_write_flag(7, (vr & 0x80) ~= 0)
  CPU_FLAGS = CPU_FLAGS & 0xFFFB | parity_table[vr & 0xFF]
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

local cpu_shift_mask = 0x1F
if cpu_arch == "8086" then
 cpu_shift_mask = 0xFF
end

local function cpu_shl(mrm, opcode)
 local v1 = cpu_read_rm(mrm, mrm.src) & cpu_shift_mask
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
   cpu_write_flag(11, ((CPU_FLAGS & (1<<(0))) ~= 0) ~= msb_result)
  end
 end
end

local function cpu_shr(mrm, opcode, arith)
 local w = (opcode & 0x01)
 local mask = 0x8000
 if w == 0 then mask = 0x80 end

 local v1 = cpu_read_rm(mrm, mrm.src) & cpu_shift_mask
 local v2 = cpu_read_rm(mrm, mrm.dst)
 local vr
 if (arith) then
  vr = v2
  local shift1 = v1
  while shift1 > 0 do
   vr = (vr & mask) | ((vr >> 1) & (mask - 1))
   shift1 = shift1 - 1
  end
 else
  vr = v2 >> v1
 end
 cpu_write_rm(mrm, mrm.dst, vr)
 _cpu_uf_zsp(vr, opcode)
 -- TODO: handle 0xFF
 if (1 << ((v1 & 0x1F) - 1)) > mask then
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

 local v1 = (cpu_read_rm(mrm, mrm.src) & cpu_shift_mask)
 local v2 = cpu_read_rm(mrm, mrm.dst)
 local vr = v2
 local cf = 0
 local of = 0
 if ((CPU_FLAGS & (1<<(0))) ~= 0) then cf = 1 end

 local shifts = v1
 if shifts > 0 then
  if mode == ROTATE_MODE_ROR then
   shifts = shifts & shift
   local shiftmask = (1 << shifts) - 1
   cf = (vr >> ((shifts - 1) & shift)) & 0x01
   vr = (vr >> shifts) | ((vr & shiftmask) << ((shift - shifts + 1) & shift))
   of = ((vr >> shift) ~ (vr >> (shift - 1))) & 0x01
  elseif mode == ROTATE_MODE_ROL then
   shifts = shifts & shift
   cf = (vr >> ((shift - shifts + 1) & shift)) & 0x01
   vr = ((vr << shifts) & ((1 << (shift + 1)) - 1)) | (vr >> ((shift - shifts + 1) & shift))
   of = ((vr >> shift) ~ cf) & 0x01
  elseif mode == ROTATE_MODE_RCR then
   shifts = shifts % (shift + 2)
   while shifts > 0 do
    local newcf = (vr & 0x01)
    vr = (vr >> 1) | (cf << shift)
    shifts = shifts - 1
    cf = newcf
   end
   of = ((vr >> shift) ~ (vr >> (shift - 1))) & 0x01
  elseif mode == ROTATE_MODE_RCL then
   shifts = shifts % (shift + 2)
   while shifts > 0 do
    local newcf = (vr >> shift) & 0x01
    vr = ((vr << 1) & ((1 << (shift + 1)) - 1)) | cf
    shifts = shifts - 1
    cf = newcf
   end
   of = ((vr >> shift) ~ cf) & 0x01
  end

  cpu_write_rm(mrm, mrm.dst, vr & 0xFFFF)
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
 local vrf
 if w == 1 then
  vr = vr & 0xFFFFFFFF
  CPU_REGS[3] = (vr >> 16)
  CPU_REGS[1] = vr & 0xFFFF
  vrf = vr >> 16
 else
  vr = vr & 0xFFFF
  CPU_REGS[1] = vr
  vrf = vr >> 8
 end

 cpu_write_flag(0, vrf ~= 0)
 cpu_write_flag(11, vrf ~= 0)
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
 if carry and ((CPU_FLAGS & (1<<(0))) ~= 0) then
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

local function cpu_cmp(v2, v1, opcode)
 local vr = v2 - v1
 _cpu_uf_co_sub(v1, v2, 0, vr, opcode)
 _cpu_uf_zsp(vr, opcode)
end

local function cpu_cmp_mrm(mrm, opcode)
 cpu_cmp(cpu_read_rm(mrm, mrm.dst), cpu_read_rm(mrm, mrm.src), opcode)
end

local function cpu_sub(mrm, opcode, borrow)
 local w = (opcode & 0x01)
 local v1 = cpu_read_rm(mrm, mrm.src)
 local v2 = cpu_read_rm(mrm, mrm.dst)
 local vb = 0
 if borrow and ((CPU_FLAGS & (1<<(0))) ~= 0) then
  vb = 1
 end
 local vr = v2 - v1 - vb
 _cpu_uf_co_sub(v1, v2, vb, vr, opcode)
 if w == 1 then
  vr = vr & 0xFFFF
 else
  vr = vr & 0xFF
 end
 cpu_write_rm(mrm, mrm.dst, vr)
 _cpu_uf_zsp(vr, opcode)
end

local function cpu_xor(mrm, opc)
 local v1 = cpu_read_rm(mrm, mrm.src)
 local v2 = cpu_read_rm(mrm, mrm.dst)
 local vr = v1 ~ v2
 cpu_write_rm(mrm, mrm.dst, vr)
 _cpu_uf_bit(vr, opc)
end

local function cpu_and(mrm, opc)
 local v1 = cpu_read_rm(mrm, mrm.src)
 local v2 = cpu_read_rm(mrm, mrm.dst)
 local vr = v1 & v2
 cpu_write_rm(mrm, mrm.dst, vr)
 _cpu_uf_bit(vr, opc)
end

local function cpu_test(mrm, opc)
 local v1 = cpu_read_rm(mrm, mrm.src)
 local v2 = cpu_read_rm(mrm, mrm.dst)
 local vr = v1 & v2
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
 cpu_print(string.format("AX:%04X CX:%04X DX:%04X BX:%04X SP:%04X BP:%04X SI:%04X DI:%04X | %04X %04X %04X %04X | %04X",
  CPU_REGS[1], CPU_REGS[2], CPU_REGS[3], CPU_REGS[4], CPU_REGS[5], CPU_REGS[6], CPU_REGS[7], CPU_REGS[8],
  CPU_SEGMENTS[1], CPU_SEGMENTS[2], CPU_SEGMENTS[3], CPU_SEGMENTS[4], CPU_FLAGS))
-- cpu_print(string.format("%02X (flags %04X, seg ES %04X CS %04X SS %04X DS %04X)", opcode, CPU_FLAGS, CPU_SEGMENTS[1], CPU_SEGMENTS[2], CPU_SEGMENTS[3], CPU_SEGMENTS[4]))
-- cpu_print(string.format("AX %04X CX %04X DX %04X BX %04X SP %04X BP %04X SI %04X DI %04X", CPU_REGS[1], CPU_REGS[2], CPU_REGS[3], CPU_REGS[4], CPU_REGS[5], CPU_REGS[6], CPU_REGS[7], CPU_REGS[8]))
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
-- local opc_c = opcode & 0xFE
-- if opc_c ~= 0x6C and opc_c ~= 0x6E and opc_c ~= 0xA4 and opc_c ~= 0xA6 and opc_c ~= 0xAA and opc_c ~= 0xAC
-- and opc_c ~= 0xAE then
-- CPU_IP = old_ip
-- return true
-- end

 -- if length zero, skip
 -- TODO: this relies on the opcode after REP always being 1 long...
 if (CPU_REGS[2] == 0) then
  return true
 end

 CPU_IP = old_ip

 local pr_state = true
 local skipConds = opcode ~= 0xA6 and opcode ~= 0xA7 and opcode ~= 0xAE and opcode ~= 0xAF

 while CPU_REGS[2] ~= 0 do
  local r = run_one(true, pr_state)

  if not r then return false
  elseif r == "block" then platform_error("this should never see 'block'") end

  CPU_REGS[2] = (CPU_REGS[2] - 1) & 0xFFFF
  if CPU_REGS[2] == 0 then break end

  local condResult = skipConds
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



 if p == nil then



  return 0xFF
 elseif type(p) == "function" then return p(cond)
 else return p end
end

local function cpu_out(cond, val)
 local p = io_ports[cond+1]



 if type(p) == "function" then p(cond,val)
 elseif p ~= nil then io_ports[cond+1] = val
 else



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

-- emu_debug(1, string.format("unknown interrupt: %02X AX=%04X", cond, ax))
end

local function cpu_int(cond)
 local addr = RAM:r16(cond * 4)
 local seg = RAM:r16(cond * 4 + 2)



 cpu_push16(CPU_FLAGS)
 cpu_push16(CPU_SEGMENTS[2])
 cpu_push16(CPU_IP)

 CPU_SEGMENTS[2]=seg
 CPU_IP=addr
 CPU_HALTED=false

 CPU_FLAGS = CPU_FLAGS & (~(1 << (9)))
end

local rel_jmp_conds = {
 function() return ((CPU_FLAGS & (1<<(11))) ~= 0) end,
 function() return not ((CPU_FLAGS & (1<<(11))) ~= 0) end,
 function() return ((CPU_FLAGS & (1<<(0))) ~= 0) end,
 function() return not ((CPU_FLAGS & (1<<(0))) ~= 0) end,
 function() return ((CPU_FLAGS & (1<<(6))) ~= 0) end,
 function() return not ((CPU_FLAGS & (1<<(6))) ~= 0) end,
 function() return ((CPU_FLAGS & (1<<(0))) ~= 0) or ((CPU_FLAGS & (1<<(6))) ~= 0) end,
 function() return not (((CPU_FLAGS & (1<<(0))) ~= 0) or ((CPU_FLAGS & (1<<(6))) ~= 0)) end,
 function() return ((CPU_FLAGS & (1<<(7))) ~= 0) end,
 function() return not ((CPU_FLAGS & (1<<(7))) ~= 0) end,
 function() return ((CPU_FLAGS & (1<<(2))) ~= 0) end,
 function() return not ((CPU_FLAGS & (1<<(2))) ~= 0) end,
 function() return ((CPU_FLAGS & (1<<(11))) ~= 0) ~= ((CPU_FLAGS & (1<<(7))) ~= 0) end,
 function() return ((CPU_FLAGS & (1<<(11))) ~= 0) == ((CPU_FLAGS & (1<<(7))) ~= 0) end,
 function() return (((CPU_FLAGS & (1<<(11))) ~= 0) ~= ((CPU_FLAGS & (1<<(7))) ~= 0)) or ((CPU_FLAGS & (1<<(6))) ~= 0) end,
 function() return not ((((CPU_FLAGS & (1<<(11))) ~= 0) ~= ((CPU_FLAGS & (1<<(7))) ~= 0)) or ((CPU_FLAGS & (1<<(6))) ~= 0)) end
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
opcode_map[0x00] = function(opcode) cpu_add(mrm6_table[((opcode) & 0x7)]((opcode)), opcode) end
for i=0x01,0x05 do opcode_map[i] = opcode_map[0x00] end

-- PUSH/POP ES
opcode_map[0x06] = function(opcode) cpu_push16(CPU_SEGMENTS[1]) end
opcode_map[0x07] = function(opcode) CPU_SEGMENTS[1] = cpu_pop16() end

-- OR
opcode_map[0x08] = function(opcode) cpu_or(mrm6_table[((opcode) & 0x7)]((opcode)), opcode) end
for i=0x09,0x0D do opcode_map[i] = opcode_map[0x08] end

-- PUSH CS
opcode_map[0x0E] = function(opcode) cpu_push16(CPU_SEGMENTS[2]) end

-- POP CS (8086)
if cpu_arch == "8086" then
 opcode_map[0x0F] = function(opcode) CPU_SEGMENTS[2] = cpu_pop16() end
end

-- ADC
opcode_map[0x10] = function(opcode) cpu_add(mrm6_table[((opcode) & 0x7)]((opcode)), opcode, true) end
for i=0x11,0x15 do opcode_map[i] = opcode_map[0x10] end

-- PUSH/POP SS
opcode_map[0x16] = function(opcode) cpu_push16(CPU_SEGMENTS[3]) end
opcode_map[0x17] = function(opcode) CPU_SEGMENTS[3] = cpu_pop16() end

-- SBB
opcode_map[0x18] = function(opcode) cpu_sub(mrm6_table[((opcode) & 0x7)]((opcode)), opcode, true) end
for i=0x19,0x1D do opcode_map[i] = opcode_map[0x18] end

-- PUSH/POP DS
opcode_map[0x1E] = function(opcode) cpu_push16(CPU_SEGMENTS[4]) end
opcode_map[0x1F] = function(opcode) CPU_SEGMENTS[4] = cpu_pop16() end

-- AND
opcode_map[0x20] = function(opcode) cpu_and(mrm6_table[((opcode) & 0x7)]((opcode)), opcode) end
for i=0x21,0x25 do opcode_map[i] = opcode_map[0x20] end

-- ES:
opcode_map[0x26] = function(opcode)
 CPU_SEGMOD = 1
 local r = run_one(true, true)
 CPU_SEGMOD = nil
 return r
end

-- DAA
opcode_map[0x27] = function(opcode)
 local al = CPU_REGS[1] & 0xFF
 local old_al = al
 local old_cf = ((CPU_FLAGS & (1<<(0))) ~= 0)
 if ((old_al & 0x0F) > 0x9) or ((CPU_FLAGS & (1<<(4))) ~= 0) then
  al = al + 0x6
  cpu_write_flag(0, old_cf or (al > 0xFF))
  CPU_FLAGS = CPU_FLAGS | (1 << (4))
 else
  CPU_FLAGS = CPU_FLAGS & (~(1 << (4)))
 end
 if (old_al > 0x99) or old_cf then
  al = al + 0x60
  CPU_FLAGS = CPU_FLAGS | (1 << (0))
 end
 CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | (al & 0xFF)
 _cpu_uf_zsp(al, 0)
end

-- SUB
opcode_map[0x28] = function(opcode) cpu_sub(mrm6_table[((opcode) & 0x7)]((opcode)), opcode) end
for i=0x29,0x2D do opcode_map[i] = opcode_map[0x28] end

-- CS:
opcode_map[0x2E] = function(opcode)
 CPU_SEGMOD = 2
 local r = run_one(true, true)
 CPU_SEGMOD = nil
 return r
end

-- DAS
opcode_map[0x2F] = function(opcode)
 local al = CPU_REGS[1] & 0xFF
 local old_al = al
 local old_cf = ((CPU_FLAGS & (1<<(0))) ~= 0)
 if ((al & 0x0F) > 0x9) or ((CPU_FLAGS & (1<<(4))) ~= 0) then
  al = al - 0x6
  cpu_write_flag(0, old_cf or (al < 0))
  CPU_FLAGS = CPU_FLAGS | (1 << (4))
 else
  CPU_FLAGS = CPU_FLAGS & (~(1 << (4)))
 end
 if ((al) > 0x99) or old_cf then
  al = al - 0x60
  CPU_FLAGS = CPU_FLAGS | (1 << (0))
 else
  CPU_FLAGS = CPU_FLAGS & (~(1 << (0)))
 end
 CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | (al & 0xFF)
 _cpu_uf_zsp(al, 0)
end

-- XOR
opcode_map[0x30] = function(opcode) cpu_xor(mrm6_table[((opcode) & 0x7)]((opcode)), opcode) end
for i=0x31,0x35 do opcode_map[i] = opcode_map[0x30] end

-- SS:
opcode_map[0x36] = function(opcode)
 CPU_SEGMOD = 3
 local r = run_one(true, true)
 CPU_SEGMOD = nil
 return r
end

-- AAA
opcode_map[0x37] = function(opcode)
 local al = CPU_REGS[1] & 0xFF
 if ((al & 0x0F) >= 0x9) or ((CPU_FLAGS & (1<<(4))) ~= 0) then
  CPU_REGS[1] = (CPU_REGS[1] + 0x106) & 0xFFFF
  CPU_FLAGS = CPU_FLAGS | (1 << (0))
  CPU_FLAGS = CPU_FLAGS | (1 << (4))
 else
  CPU_FLAGS = CPU_FLAGS & (~(1 << (0)))
  CPU_FLAGS = CPU_FLAGS & (~(1 << (4)))
 end
 CPU_REGS[1] = (CPU_REGS[1] & 0xFF0F)
end

-- CMP
opcode_map[0x38] = function(opcode) cpu_cmp_mrm(mrm6_table[((opcode) & 0x7)]((opcode)), opcode) end
for i=0x39,0x3D do opcode_map[i] = opcode_map[0x38] end

-- DS:
opcode_map[0x3E] = function(opcode)
 CPU_SEGMOD = 4
 local r = run_one(true, true)
 CPU_SEGMOD = nil
 return r
end

-- AAS
opcode_map[0x3F] = function(opcode)
 local al = CPU_REGS[1] & 0xFF
 if ((al & 0x0F) >= 0x9) or ((CPU_FLAGS & (1<<(4))) ~= 0) then
  CPU_REGS[1] = (CPU_REGS[1] - 0x006) & 0xFFFF
  local ah = (CPU_REGS[1] & 0xFF00) >> 8
  ah = (ah - 1) & 0xFF
  CPU_REGS[1] = (CPU_REGS[1] & 0xFF) | (ah << 8)
  CPU_FLAGS = CPU_FLAGS | (1 << (0))
  CPU_FLAGS = CPU_FLAGS | (1 << (4))
 else
  CPU_FLAGS = CPU_FLAGS & (~(1 << (0)))
  CPU_FLAGS = CPU_FLAGS & (~(1 << (4)))
 end
 CPU_REGS[1] = (CPU_REGS[1] & 0xFF0F)
end

-- INC

opcode_map[(0x40)] = function(opcode) local v = CPU_REGS[(1)]; v = (v + 1) & 0xFFFF; CPU_REGS[(1)] = v; _cpu_uf_inc(v, 1) end
opcode_map[(0x41)] = function(opcode) local v = CPU_REGS[(2)]; v = (v + 1) & 0xFFFF; CPU_REGS[(2)] = v; _cpu_uf_inc(v, 1) end
opcode_map[(0x42)] = function(opcode) local v = CPU_REGS[(3)]; v = (v + 1) & 0xFFFF; CPU_REGS[(3)] = v; _cpu_uf_inc(v, 1) end
opcode_map[(0x43)] = function(opcode) local v = CPU_REGS[(4)]; v = (v + 1) & 0xFFFF; CPU_REGS[(4)] = v; _cpu_uf_inc(v, 1) end
opcode_map[(0x44)] = function(opcode) local v = CPU_REGS[(5)]; v = (v + 1) & 0xFFFF; CPU_REGS[(5)] = v; _cpu_uf_inc(v, 1) end
opcode_map[(0x45)] = function(opcode) local v = CPU_REGS[(6)]; v = (v + 1) & 0xFFFF; CPU_REGS[(6)] = v; _cpu_uf_inc(v, 1) end
opcode_map[(0x46)] = function(opcode) local v = CPU_REGS[(7)]; v = (v + 1) & 0xFFFF; CPU_REGS[(7)] = v; _cpu_uf_inc(v, 1) end
opcode_map[(0x47)] = function(opcode) local v = CPU_REGS[(8)]; v = (v + 1) & 0xFFFF; CPU_REGS[(8)] = v; _cpu_uf_inc(v, 1) end

-- DEC

opcode_map[(0x48)] = function(opcode) local v = CPU_REGS[(1)]; v = (v - 1) & 0xFFFF; CPU_REGS[(1)] = v; _cpu_uf_dec(v, 1) end
opcode_map[(0x49)] = function(opcode) local v = CPU_REGS[(2)]; v = (v - 1) & 0xFFFF; CPU_REGS[(2)] = v; _cpu_uf_dec(v, 1) end
opcode_map[(0x4A)] = function(opcode) local v = CPU_REGS[(3)]; v = (v - 1) & 0xFFFF; CPU_REGS[(3)] = v; _cpu_uf_dec(v, 1) end
opcode_map[(0x4B)] = function(opcode) local v = CPU_REGS[(4)]; v = (v - 1) & 0xFFFF; CPU_REGS[(4)] = v; _cpu_uf_dec(v, 1) end
opcode_map[(0x4C)] = function(opcode) local v = CPU_REGS[(5)]; v = (v - 1) & 0xFFFF; CPU_REGS[(5)] = v; _cpu_uf_dec(v, 1) end
opcode_map[(0x4D)] = function(opcode) local v = CPU_REGS[(6)]; v = (v - 1) & 0xFFFF; CPU_REGS[(6)] = v; _cpu_uf_dec(v, 1) end
opcode_map[(0x4E)] = function(opcode) local v = CPU_REGS[(7)]; v = (v - 1) & 0xFFFF; CPU_REGS[(7)] = v; _cpu_uf_dec(v, 1) end
opcode_map[(0x4F)] = function(opcode) local v = CPU_REGS[(8)]; v = (v - 1) & 0xFFFF; CPU_REGS[(8)] = v; _cpu_uf_dec(v, 1) end

-- PUSH/POP
opcode_map[0x50] = function(opcode) cpu_push16(CPU_REGS[1]) end
opcode_map[0x51] = function(opcode) cpu_push16(CPU_REGS[2]) end
opcode_map[0x52] = function(opcode) cpu_push16(CPU_REGS[3]) end
opcode_map[0x53] = function(opcode) cpu_push16(CPU_REGS[4]) end
opcode_map[0x54] = function(opcode) cpu_push16(CPU_REGS[5]) end
opcode_map[0x55] = function(opcode) cpu_push16(CPU_REGS[6]) end
opcode_map[0x56] = function(opcode) cpu_push16(CPU_REGS[7]) end
opcode_map[0x57] = function(opcode) cpu_push16(CPU_REGS[8]) end
opcode_map[0x58] = function(opcode) CPU_REGS[1] = cpu_pop16() end
opcode_map[0x59] = function(opcode) CPU_REGS[2] = cpu_pop16() end
opcode_map[0x5A] = function(opcode) CPU_REGS[3] = cpu_pop16() end
opcode_map[0x5B] = function(opcode) CPU_REGS[4] = cpu_pop16() end
opcode_map[0x5C] = function(opcode) CPU_REGS[5] = cpu_pop16() end
opcode_map[0x5D] = function(opcode) CPU_REGS[6] = cpu_pop16() end
opcode_map[0x5E] = function(opcode) CPU_REGS[7] = cpu_pop16() end
opcode_map[0x5F] = function(opcode) CPU_REGS[8] = cpu_pop16() end

if cpu_arch == "8086" or cpu_arch == "80186" then
 -- PUSH SP (8086/80186 bug reproduction)
 opcode_map[0x54] = function(opcode) cpu_push16(CPU_REGS[5] - 2) end
end

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

if cpu_arch == "8086" then
 for i=0,15 do
  opcode_map[0x60 + i] = opcode_map[0x70 + i]
 end
else
-- 80186+ opcodes
-- TODO: IMUL(69/6B), BOUND

-- PUSHA
opcode_map[0x60] = function(opcode)
 local tmp = CPU_REGS[5]
 cpu_push16(CPU_REGS[1])
 cpu_push16(CPU_REGS[2])
 cpu_push16(CPU_REGS[3])
 cpu_push16(CPU_REGS[4])
 cpu_push16(tmp)
 cpu_push16(CPU_REGS[6])
 cpu_push16(CPU_REGS[7])
 cpu_push16(CPU_REGS[8])
end

-- POPA
opcode_map[0x61] = function(opcode)
 CPU_REGS[8] = cpu_pop16()
 CPU_REGS[7] = cpu_pop16()
 CPU_REGS[6] = cpu_pop16()
 cpu_pop16()
 CPU_REGS[4] = cpu_pop16()
 CPU_REGS[3] = cpu_pop16()
 CPU_REGS[2] = cpu_pop16()
 CPU_REGS[1] = cpu_pop16()
end

-- PUSH imm16/imm8
opcode_map[0x68] = function(opcode)
 cpu_push16(cpu_advance_ip16())
end
opcode_map[0x6A] = function(opcode)
 cpu_push16(cpu_advance_ip())
end

-- ENTER (TESTME)
opcode_map[0xC8] = function(opcode)
 local nb = cpu_advance_ip16()
 local level = cpu_advance_ip() & 0x1F
 cpu_push16(CPU_REGS[6])
 local frame_ptr = CPU_REGS[5]
 if level > 0 then
  for i=1,level-1 do
   CPU_REGS[6] = CPU_REGS[6] - 2
   cpu_push16(CPU_REGS[6])
  end
  cpu_push16(frame_ptr)
 end
 CPU_REGS[6] = frame_ptr
 CPU_REGS[5] = CPU_REGS[5] - nb
end

-- LEAVE
opcode_map[0xC9] = function(opcode)
 CPU_REGS[5] = CPU_REGS[6]
 CPU_REGS[6] = cpu_pop16()
end

opcode_map[0xA4] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 RAM[addrDst] = RAM[addrSrc]
 cpu_incdec_dir(7, 1)
 cpu_incdec_dir(8, 1)
end
opcode_map[0xA5] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 RAM:w16(addrDst, RAM:r16(addrSrc))
 cpu_incdec_dir(7, 2)
 cpu_incdec_dir(8, 2)
end

-- INS Ib
opcode_map[0x6C] = function(opcode)
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 RAM[addrDst] = cpu_in(CPU_REGS[3]) & 0xFF
 cpu_incdec_dir(8, 1)
end

-- INS Iw
opcode_map[0x6D] = function(opcode)
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 RAM:w16(addrDst, cpu_in(CPU_REGS[3]) & 0xFFFF)
 cpu_incdec_dir(8, 2)
end

-- OUTS Ib
opcode_map[0x6E] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 cpu_out(CPU_REGS[3], RAM[addrSrc])
 cpu_incdec_dir(7, 1)
end

-- OUTS Iw
opcode_map[0x6F] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 cpu_out(CPU_REGS[3], RAM:r16(addrSrc))
 cpu_incdec_dir(7, 2)
end

end -- (80186+ opcodes)

local grp1_table = {
 [0]=cpu_add,
 [1]=cpu_or,
 [2]=function(a,b) cpu_add(a,b,true) end,
 [3]=function(a,b) cpu_sub(a,b,true) end,
 [4]=cpu_and,
 [5]=function(a,b) cpu_sub(a,b,false) end,
 [6]=cpu_xor,
 [7]=function(a,b) cpu_cmp_mrm(a,b) end
}

-- GRP1
opcode_map[0x80] = function(opcode)
 local mrm = cpu_mrm_copy(cpu_mod_rm(0))
 local v = mrm.src & 0x07
 mrm.src = 40
 mrm.imm = cpu_advance_ip()
 grp1_table[v](mrm, opcode)
end
opcode_map[0x81] = function(opcode)
 local mrm = cpu_mrm_copy(cpu_mod_rm(1))
 local v = mrm.src & 0x07
 mrm.src = 41
 mrm.imm = cpu_advance_ip16()
 grp1_table[v](mrm, opcode)
end
opcode_map[0x82] = function(opcode)
 local mrm = cpu_mrm_copy(cpu_mod_rm(0))
 local v = mrm.src & 0x07
 mrm.src = 40
 mrm.imm = cpu_advance_ip()
 grp1_table[v](mrm, opcode)
end
opcode_map[0x83] = function(opcode)
 local mrm = cpu_mrm_copy(cpu_mod_rm(1))
 local v = mrm.src & 0x07
 mrm.src = 41
 mrm.imm = to_s8(cpu_advance_ip()) & 0xFFFF
 grp1_table[v](mrm, opcode)
end

-- TEST
opcode_map[0x84] = function(opcode) cpu_test(cpu_mod_rm(0), opcode) end
opcode_map[0x85] = function(opcode) cpu_test(cpu_mod_rm(1), opcode) end

-- XCHG
opcode_map[0x86] = function(opcode)
 local mrm = cpu_mod_rm(opcode & 0x01)
 local t = cpu_read_rm(mrm, mrm.src)
 cpu_write_rm(mrm, mrm.src, cpu_read_rm(mrm, mrm.dst))
 cpu_write_rm(mrm, mrm.dst, t)
end
opcode_map[0x87] = opcode_map[0x86]

-- MOV mod/rm
opcode_map[0x88] = function(opcode) cpu_mov(cpu_mod_rm(0)) end
opcode_map[0x89] = function(opcode) cpu_mov(cpu_mod_rm(1)) end
opcode_map[0x8A] = function(opcode) cpu_mov(cpu_mod_rm(2)) end
opcode_map[0x8B] = function(opcode) cpu_mov(cpu_mod_rm(3)) end

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







opcode_map[(0x91)] = function(opcode) local v = CPU_REGS[(2)]; CPU_REGS[(2)] = CPU_REGS[1]; CPU_REGS[1] = v; end
opcode_map[(0x92)] = function(opcode) local v = CPU_REGS[(3)]; CPU_REGS[(3)] = CPU_REGS[1]; CPU_REGS[1] = v; end
opcode_map[(0x93)] = function(opcode) local v = CPU_REGS[(4)]; CPU_REGS[(4)] = CPU_REGS[1]; CPU_REGS[1] = v; end
opcode_map[(0x94)] = function(opcode) local v = CPU_REGS[(5)]; CPU_REGS[(5)] = CPU_REGS[1]; CPU_REGS[1] = v; end
opcode_map[(0x95)] = function(opcode) local v = CPU_REGS[(6)]; CPU_REGS[(6)] = CPU_REGS[1]; CPU_REGS[1] = v; end
opcode_map[(0x96)] = function(opcode) local v = CPU_REGS[(7)]; CPU_REGS[(7)] = CPU_REGS[1]; CPU_REGS[1] = v; end
opcode_map[(0x97)] = function(opcode) local v = CPU_REGS[(8)]; CPU_REGS[(8)] = CPU_REGS[1]; CPU_REGS[1] = v; end

-- CBW
opcode_map[0x98] = function(opcode)
 local v = CPU_REGS[1] & 0xFF
 if v >= 0x80 then v = v | 0xFF00 end
 CPU_REGS[1] = v
end

-- CWD
opcode_map[0x99] = function(opcode)
 if CPU_REGS[1] >= 0x8000 then
  CPU_REGS[3] = 0xFFFF
 else
  CPU_REGS[3] = 0x0000
 end
end

-- CALL far
opcode_map[0x9A] = function(opcode)
 local newIp = cpu_advance_ip16()
 local newCs = cpu_advance_ip16()
 cpu_push16(CPU_SEGMENTS[2])
 cpu_push16(CPU_IP)
 CPU_IP = newIp
 CPU_SEGMENTS[2] = newCs
end

-- PUSHF/POPF
opcode_map[0x9C] = function(opcode) cpu_push16(CPU_FLAGS) end

-- ARCHNOTE: The 286 clears bits 12-15 in real mode.
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
 CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | RAM[((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (addr))]
end

-- MOV offs->AX
opcode_map[0xA1] = function(opcode)
 local addr = cpu_advance_ip16()
 CPU_REGS[1] = RAM:r16(((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (addr)))
end

-- MOV AL->offs
opcode_map[0xA2] = function(opcode)
 local addr = cpu_advance_ip16()
 RAM[((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (addr))] = CPU_REGS[1] & 0xFF
end

-- MOV AX->offs
opcode_map[0xA3] = function(opcode)
 local addr = cpu_advance_ip16()
 RAM:w16(((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (addr)), CPU_REGS[1])
end

-- MOVSB/MOVSW
opcode_map[0xA4] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 RAM[addrDst] = RAM[addrSrc]
 cpu_incdec_dir(7, 1)
 cpu_incdec_dir(8, 1)
end
opcode_map[0xA5] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 RAM:w16(addrDst, RAM:r16(addrSrc))
 cpu_incdec_dir(7, 2)
 cpu_incdec_dir(8, 2)
end

-- CMPSB/CMPSW
opcode_map[0xA6] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 cpu_cmp(RAM[addrSrc], RAM[addrDst], opcode)
 cpu_incdec_dir(7, 1)
 cpu_incdec_dir(8, 1)
end
opcode_map[0xA7] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 cpu_cmp(RAM:r16(addrSrc), RAM:r16(addrDst), opcode)
 cpu_incdec_dir(7, 2)
 cpu_incdec_dir(8, 2)
end

-- TEST AL, imm8
opcode_map[0xA8] = function(opcode)
 _cpu_uf_bit((CPU_REGS[1] & cpu_advance_ip()) & 0xFF, 0)
end
-- TEST AX, imm16
opcode_map[0xA9] = function(opcode)
 _cpu_uf_bit((CPU_REGS[1] & cpu_advance_ip16()), 1)
end

-- STOSB/STOSW
opcode_map[0xAA] = function(opcode)
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 RAM[addrDst] = CPU_REGS[1] & 0xFF
 cpu_incdec_dir(8, 1)
end
opcode_map[0xAB] = function(opcode)
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 RAM:w16(addrDst, CPU_REGS[1])
 cpu_incdec_dir(8, 2)
end

-- LODSB/LODSW
opcode_map[0xAC] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | RAM[addrSrc]
 cpu_incdec_dir(7, 1)
end
opcode_map[0xAD] = function(opcode)
 local addrSrc = ((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (CPU_REGS[7]))
 CPU_REGS[1] = RAM:r16(addrSrc)
 cpu_incdec_dir(7, 2)
end

-- SCASB/SCASW
opcode_map[0xAE] = function(opcode)
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 cpu_cmp(CPU_REGS[1] & 0xFF, RAM[addrDst], opcode)
 cpu_incdec_dir(8, 1)
end
opcode_map[0xAF] = function(opcode)
 local addrDst = ((CPU_SEGMENTS[(SEG_ES)+1]<<4)+(CPU_REGS[8]))
 cpu_cmp(CPU_REGS[1], RAM:r16(addrDst), opcode)
 cpu_incdec_dir(8, 2)
end

-- MOV imm8
opcode_map[0xB0] = function(opcode) CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | (cpu_advance_ip()) end
opcode_map[0xB1] = function(opcode) CPU_REGS[2] = (CPU_REGS[2] & 0xFF00) | (cpu_advance_ip()) end
opcode_map[0xB2] = function(opcode) CPU_REGS[3] = (CPU_REGS[3] & 0xFF00) | (cpu_advance_ip()) end
opcode_map[0xB3] = function(opcode) CPU_REGS[4] = (CPU_REGS[4] & 0xFF00) | (cpu_advance_ip()) end
opcode_map[0xB4] = function(opcode) CPU_REGS[1] = (CPU_REGS[1] & 0xFF) | (cpu_advance_ip() << 8) end
opcode_map[0xB5] = function(opcode) CPU_REGS[2] = (CPU_REGS[2] & 0xFF) | (cpu_advance_ip() << 8) end
opcode_map[0xB6] = function(opcode) CPU_REGS[3] = (CPU_REGS[3] & 0xFF) | (cpu_advance_ip() << 8) end
opcode_map[0xB7] = function(opcode) CPU_REGS[4] = (CPU_REGS[4] & 0xFF) | (cpu_advance_ip() << 8) end

-- MOV imm16
opcode_map[0xB8] = function(opcode) CPU_REGS[1] = cpu_advance_ip16() end
opcode_map[0xB9] = function(opcode) CPU_REGS[2] = cpu_advance_ip16() end
opcode_map[0xBA] = function(opcode) CPU_REGS[3] = cpu_advance_ip16() end
opcode_map[0xBB] = function(opcode) CPU_REGS[4] = cpu_advance_ip16() end
opcode_map[0xBC] = function(opcode) CPU_REGS[5] = cpu_advance_ip16() end
opcode_map[0xBD] = function(opcode) CPU_REGS[6] = cpu_advance_ip16() end
opcode_map[0xBE] = function(opcode) CPU_REGS[7] = cpu_advance_ip16() end
opcode_map[0xBF] = function(opcode) CPU_REGS[8] = cpu_advance_ip16() end

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
 cpu_write_rm(mrm, mrm.dst, RAM:r16(((CPU_SEGMENTS[(CPU_SEGMOD or (defseg+1))]<<4) + (addr))))
 if opcode == 0xC5 then
  CPU_SEGMENTS[4] = RAM:r16(((CPU_SEGMENTS[(CPU_SEGMOD or (defseg+1))]<<4) + (addr + 2)))
 else
  CPU_SEGMENTS[1] = RAM:r16(((CPU_SEGMENTS[(CPU_SEGMOD or (defseg+1))]<<4) + (addr + 2)))
 end
end
opcode_map[0xC5] = opcode_map[0xC4]

-- MOV imm(rm)
opcode_map[0xC6] = function(opcode)
 local mrm = cpu_mod_rm(0)
 cpu_write_rm(mrm, mrm.dst, cpu_advance_ip())
end
opcode_map[0xC7] = function(opcode)
 local mrm = cpu_mod_rm(1)
 cpu_write_rm(mrm, mrm.dst, cpu_advance_ip16())
end

-- RET far + pop
opcode_map[0xCA] = function(opcode)
 local btp = cpu_advance_ip16()
 CPU_IP = cpu_pop16()
 CPU_SEGMENTS[2] = cpu_pop16()
 CPU_REGS[5] = CPU_REGS[5] + btp
end

-- RET far
opcode_map[0xCB] = function(opcode)
 CPU_IP = cpu_pop16()
 CPU_SEGMENTS[2] = cpu_pop16()
end

-- INT 3
opcode_map[0xCC] = function(opcode)
-- cpu_int(3)
 dump_memory()
 platform_error("breakpoint")
end

-- INT imm
opcode_map[0xCD] = function(opcode) cpu_int(cpu_advance_ip()) end

-- INTO
opcode_map[0xCE] = function(opcode)
 if ((CPU_FLAGS & (1<<(11))) ~= 0) then
  cpu_int(4)
 end
end

-- IRET far
opcode_map[0xCF] = function(opcode)
 CPU_IP = cpu_pop16()
 CPU_SEGMENTS[2] = cpu_pop16()
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

-- GRP2
opcode_map[0xD0] = function(opcode)
 local mrm = cpu_mrm_copy(cpu_mod_rm(opcode & 0x01))
 local v = (mrm.src & 0x07) + 1
 mrm.src = 40
 mrm.imm = 1
 grp2_table[v](mrm,opcode)
end
opcode_map[0xD2] = function(opcode)
 local mrm = cpu_mrm_copy(cpu_mod_rm(opcode & 0x01))
 local v = (mrm.src & 0x07) + 1
 mrm.src = 17
 grp2_table[v](mrm,opcode)
end

if cpu_arch ~= "8086" then
 opcode_map[0xC0] = function(opcode)
  local mrm = cpu_mrm_copy(cpu_mod_rm(opcode & 0x01))
  local v = (mrm.src & 0x07) + 1
  mrm.src = 40
  mrm.imm = cpu_advance_ip()
  grp2_table[v](mrm,opcode)
 end
 opcode_map[0xC1] = opcode_map[0xD0]
end

opcode_map[0xD1] = opcode_map[0xD0]
opcode_map[0xD3] = opcode_map[0xD2]

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
 if ((CPU_FLAGS & (1<<(0))) ~= 0) then
  CPU_REGS[1] = CPU_REGS[1] | 0xFF
 else
  CPU_REGS[1] = CPU_REGS[1] & 0xFF00
 end
end

-- XLAT
opcode_map[0xD7] = function(opcode)
 local addr = (CPU_REGS[4] + (CPU_REGS[1] & 0xFF)) & 0xFFFF
 CPU_REGS[1] = (CPU_REGS[1] & 0xFF00) | RAM[((CPU_SEGMENTS[(CPU_SEGMOD or (4))]<<4) + (addr))]
end

-- LOOPNZ r8
opcode_map[0xE0] = function(opcode)
 local offset = to_s8(cpu_advance_ip())
 CPU_REGS[2] = CPU_REGS[2] - 1
 if CPU_REGS[2] ~= 0 and not ((CPU_FLAGS & (1<<(6))) ~= 0) then
  CPU_IP = (CPU_IP + offset) & 0xFFFF
 end
end
-- LOOPZ r8
opcode_map[0xE1] = function(opcode)
 local offset = to_s8(cpu_advance_ip())
 CPU_REGS[2] = CPU_REGS[2] - 1
 if CPU_REGS[2] ~= 0 and ((CPU_FLAGS & (1<<(6))) ~= 0) then
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
 CPU_SEGMENTS[2] = newCs
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
 if not cpu_rep(function() return not ((CPU_FLAGS & (1<<(6))) ~= 0) end) then return false end
end

-- REPZ
opcode_map[0xF3] = function(opcode)
 if not cpu_rep(function() return ((CPU_FLAGS & (1<<(6))) ~= 0) end) then return false end
end

-- HLT
opcode_map[0xF4] = function(opcode)
 CPU_HALTED = true
 return "block"
end

-- CMC
opcode_map[0xF5] = function(opcode) CPU_FLAGS = CPU_FLAGS ~ (1 << (0)) end

-- GRP3
local grp3_table = {}
grp3_table[0] = function(mrm, opcode)
 mrm = cpu_mrm_copy(mrm)
 if opcode == 0xF7 then
  mrm.src = 41
  mrm.imm = cpu_advance_ip16()
 else
  mrm.src = 40
  mrm.imm = cpu_advance_ip()
 end
 cpu_test(mrm, opcode)
end
grp3_table[1] = function()
 platform_error("invalid opcode: GRP3/1")
end

-- GRP3/NOT
grp3_table[2] = function(mrm, opcode)
 if opcode == 0xF7 then
  cpu_write_rm(mrm, mrm.dst, cpu_read_rm(mrm, mrm.dst) ~ 0xFFFF)
 else
  cpu_write_rm(mrm, mrm.dst, cpu_read_rm(mrm, mrm.dst) ~ 0xFF)
 end
end

-- GRP3/NEG
grp3_table[3] = function(mrm, opcode)
 local src = cpu_read_rm(mrm, mrm.dst)
 local result = 0
 if opcode == 0xF7 then
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
end

grp3_table[4] = cpu_mul
grp3_table[5] = cpu_imul
grp3_table[6] = cpu_div
grp3_table[7] = cpu_idiv

opcode_map[0xF6] = function(opcode)
 local mrm = cpu_mod_rm(opcode & 0x01)
 local v = mrm.src & 0x07
 if v >= 4 then
  mrm = cpu_mrm_copy(mrm)
  if opcode == 0xF7 then mrm.src = 0 else mrm.src = 16 end
 end
 grp3_table[v](mrm, opcode)
end
opcode_map[0xF7] = opcode_map[0xF6]

-- flag setters
opcode_map[0xF8] = function(opcode) CPU_FLAGS = CPU_FLAGS & (~(1 << (0))) end
opcode_map[0xF9] = function(opcode) CPU_FLAGS = CPU_FLAGS | (1 << (0)) end
opcode_map[0xFA] = function(opcode) CPU_FLAGS = CPU_FLAGS & (~(1 << (9))) end
opcode_map[0xFB] = function(opcode) CPU_FLAGS = CPU_FLAGS | (1 << (9)) end
opcode_map[0xFC] = function(opcode) CPU_FLAGS = CPU_FLAGS & (~(1 << (10))) end
opcode_map[0xFD] = function(opcode) CPU_FLAGS = CPU_FLAGS | (1 << (10)) end

-- GRP4
opcode_map[0xFE] = function(opcode)
 local mrm = cpu_mod_rm(0)
 local v = mrm.src & 0x07
-- emu_debug(0, "GRP4/"..v)

 if v == 0 then -- INC
  local v = (cpu_read_rm(mrm,mrm.dst) + 1) & 0xFF
  cpu_write_rm(mrm,mrm.dst,v)
  _cpu_uf_inc(v, 0)
 elseif v == 1 then -- DEC
  local v = (cpu_read_rm(mrm,mrm.dst) - 1) & 0xFF
  cpu_write_rm(mrm,mrm.dst,v)
  _cpu_uf_dec(v, 0)
 else platform_error("GRP4 todo: " .. v) end
end

-- GRP5
opcode_map[0xFF] = function(opcode)
 local mrm = cpu_mod_rm(1)
 local v = mrm.src & 0x07
-- emu_debug(0,"GRP5/"..v)

 if v == 0 then -- INC
  local v = (cpu_read_rm(mrm,mrm.dst) + 1) & 0xFFFF
  cpu_write_rm(mrm,mrm.dst,v)
  _cpu_uf_inc(v, 1)
 elseif v == 1 then -- DEC
  local v = (cpu_read_rm(mrm,mrm.dst) - 1) & 0xFFFF
  cpu_write_rm(mrm,mrm.dst,v)
  _cpu_uf_dec(v, 1)
 elseif v == 2 then -- CALL near abs
  local newIp = cpu_read_rm(mrm,mrm.dst)
  cpu_push16(CPU_IP)
  CPU_IP = newIp
 elseif v == 3 then -- CALL far abs
  local addr = ((CPU_SEGMENTS[(CPU_SEGMOD or (cpu_seg_rm(mrm,mrm.dst)+1))]<<4) + (cpu_addr_rm(mrm,mrm.dst)))
  local newIp = RAM:r16(addr)
  local newCs = RAM:r16(addr+2)
  cpu_push16(CPU_SEGMENTS[2])
  cpu_push16(CPU_IP)
  CPU_IP = newIp
  CPU_SEGMENTS[2] = newCs
 elseif v == 4 then -- JMP near abs
  CPU_IP = cpu_read_rm(mrm,mrm.dst)
 elseif v == 5 then -- JMP far
  local addr = ((CPU_SEGMENTS[(CPU_SEGMOD or (cpu_seg_rm(mrm,mrm.dst)+1))]<<4) + (cpu_addr_rm(mrm,mrm.dst)))
  local newIp = RAM:r16(addr)
  local newCs = RAM:r16(addr+2)
  CPU_IP = newIp
  CPU_SEGMENTS[2] = newCs
 elseif v == 6 then cpu_push16(cpu_read_rm(mrm,mrm.dst))
 else platform_error("GRP5 todo: " .. v) end
end

-- 8087 FPU stubs
opcode_map[0x9B] = function(opcode) end
for i=0xD8,0xDF do
 opcode_map[i] = function(opcode) cpu_mod_rm(1) end
end

if cpu_arch == "8086" then
 -- 8086 0xCX opcode aliases
 opcode_map[0xC0] = opcode_map[0xC2]
 opcode_map[0xC1] = opcode_map[0xC3]
 opcode_map[0xC8] = opcode_map[0xCA]
 opcode_map[0xC9] = opcode_map[0xCB]
end






run_one = function(no_interrupting, pr_state)
 if CPU_HASINT and not no_interrupting then
-- local intr = table.remove(CPU_INTQUEUE, 1)
  local intr = CPU_INTQUEUE[1]
  if intr ~= nil then
   if intr >= 256 or ((CPU_FLAGS & (1<<(9))) ~= 0) then
    table.remove(CPU_INTQUEUE, 1)
    cpu_int(intr & 0xFF)
    if #CPU_INTQUEUE == 0 then CPU_HASINT = false end
   end
  end
 end

 if ((CPU_IP & 0xFF00) == 0x1100) and (CPU_SEGMENTS[2] == 0xF000) then
  CPU_FLAGS = CPU_FLAGS | (1 << (9)) -- enable interrupts during (after!) fake handlers
  local intr = cpu_int_fake(CPU_IP & 0xFF)
  if intr ~= "block" then
   CPU_IP = cpu_pop16()
   CPU_SEGMENTS[2] = cpu_pop16()
   local old_flags = cpu_pop16()
   local old_flag_mask = 0x0200
   CPU_FLAGS = (CPU_FLAGS & (~old_flag_mask)) | (old_flags & old_flag_mask)
   return true
  else
   return "block"
  end
 end

 local opcode = cpu_advance_ip()




 local om = opcode_map[opcode]




 if om ~= nil then
  local result = om(opcode)
  if result ~= nil then
   return result
  else
   return true
  end
 else
  emu_debug(2, "unknown opcode: " .. string.format("%02X", opcode))
  cpu_print_state(opcode)
  cpu_emit_interrupt(6, false)
  return true
 end
end

function cpu_emit_interrupt(v, nmi)
 nmi = (v == 2)
 if nmi then
  table.insert(CPU_INTQUEUE, v + 256)
 else
  table.insert(CPU_INTQUEUE, v)
 end
 CPU_HASINT = true
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

 -- handle video
 video_update()
 keyboard_update()
 pit_tick(clock)
 -- handle OC waits
 cv = os.clock()
 if (cv - clock) < 0.05 then
  oc_tick_i = oc_tick_i + 1
-- if (oc_tick_i % 3) == 0 then
   platform_sleep(0)
-- end
 end
 --cv = os.clock()
 -- handle pc speaker
 if (kbd_get_spkr_latch() & 0x03) == 0x03 then
  local pch = pit_channel(3)
  if (pch.mode == 2 or pch.mode == 3 or pch.mode == 6 or pch.mode == 7) and (pch.reload_set_lo and pch.reload_set_hi) then
   local freq = (pit_osc_freq * 1000000) / pch.reload
   platform_beep(freq, 0.05)
  end
 end
 clock = cv
end

local function cpu_execute()
 if CPU_HALTED and not CPU_HASINT then return "block" end

 execute = true
 while execute == true do
  execute = run_one(false, true)
  if execute == "block" then
   upd_tick(os.clock())
   execute = true
  elseif ((opc & 0x1FF) == 0) and (os.clock() - clock) >= 0.05 then
   upd_tick(os.clock())
  end
  opc = opc + 1
 end
 platform_error("execution stopped\n")
end

function emu_execute()
 CPU_FLAGS = 0x0202
 sysconf_init()

 -- set IVT locations to NOP or IRET
 for i=0,255 do
  if interrupt_handlers[i+1] then
   RAM[0xF1100+i] = 0x90
  else
   RAM[0xF1100+i] = 0xCF
  end
 end

 cpu_execute()
end
