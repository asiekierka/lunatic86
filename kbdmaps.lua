map_char_to_key = {}
map_char_to_key[258] = 0x50 -- down
map_char_to_key[260] = 0x4B -- left
map_char_to_key[259] = 0x48 -- up
map_char_to_key[261] = 0x4D -- right
map_char_to_key[13] = 0x1C -- enter
map_char_to_key[0] = 0
map_char_to_key[0x1C] = 1 -- esc
for i=1,9 do map_char_to_key[i + 48] = i + 1 end
local chrs = "!@#$%^&*()"
for i=1,#chrs do map_char_to_key[string.byte(chrs, i, i)] = i + 1 end
map_char_to_key[48] = 11 -- numbers
map_char_to_key[45] = 12 -- -
map_char_to_key[95] = 12 -- _
map_char_to_key[61] = 13 -- =
map_char_to_key[43] = 12 -- +
map_char_to_key[8] = 14 -- backspace
map_char_to_key[9] = 15 -- tab
local chrs = "qwertyuiop[]"
for i=1,#chrs do map_char_to_key[string.byte(chrs,i,i)] = 15 + i end
local chrs = "QWERTYUIOP{}"
for i=1,#chrs do map_char_to_key[string.byte(chrs,i,i)] = 15 + i end
map_char_to_key[13] = 28 -- enter
-- 29?
local chrs = "asdfghjkl;'"
for i=1,#chrs do map_char_to_key[string.byte(chrs,i,i)] = 29 + i end
local chrs = "ASDFGHJKL:\""
for i=1,#chrs do map_char_to_key[string.byte(chrs,i,i)] = 29 + i end
map_char_to_key[96] = 41 -- `
map_char_to_key[126] = 41 -- ~
-- 42?
map_char_to_key[92] = 43 -- \
map_char_to_key[124] = 43 -- |
local chrs = "zxcvbnm,./"
for i=1,#chrs do map_char_to_key[string.byte(chrs,i,i)] = 43 + i end
local chrs = "ZXCVBNM<>?"
for i=1,#chrs do map_char_to_key[string.byte(chrs,i,i)] = 43 + i end
-- 54?
map_char_to_key[42] = 55 -- *
-- 56?
map_char_to_key[32] = 57 -- space
