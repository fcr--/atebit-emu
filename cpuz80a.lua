local Cpu = {}
Cpu.__index = Cpu
local band, bor, lshift, rshift, bxor, arshift = bit.band, bit.bor, bit.lshift, bit.rshift, bit.bxor, bit.arshift

function Cpu:new(params)
    return setmetatable({
        a = 0, f = 0, b = 0, c = 0, d = 0, e = 0, h = 0, l = 0,
        a_= 0, f_= 0, b_= 0, c_= 0, d_= 0, e_= 0, h_= 0, l_= 0,
        i = 0, r = 0,
        ix = 0,
        iy = 0,
        sp = 0,
        pc = 0,
        cycles = 0,
        memory = params.memory,
        irqCount = 0,
        irqEnabled = false,
        irqMode = 0,
    }, self)
end

local FLAG_S, FLAG_Z, FLAG_H, FLAG_V, FLAG_N, FLAG_C = 0x80, 0x40, 0x10, 0x4, 0x2, 0x1
local FLAG_NS, FLAG_NZ, FLAG_NH, FLAG_NV, FLAG_NN, FLAG_NC = 0x7F, 0xBF, 0xEF, 0xFB, 0xFD, 0xFE

local function word(high, low)
    return bor(lshift(high, 8), low)
end

local function updateHalfCarry8(val1, val2, f)
    return band(band(val1, 0xF) + band(val2, 0xF), 0x10) ~= 0 and bor(f, FLAG_H) or band(f, FLAG_NH)
end
local function updateOverflow8(extendedVal, f)
    return band(0x100, bxor(extendedVal, rshift(extendedVal, 1))) ~= 0 and bor(f, FLAG_V) or band(f, FLAG_NV)
end
local function updateCarry8(extendedVal, f)
    return band(extendedVal, 0x100) ~= 0 and bor(f, FLAG_C) or band(f, FLAG_NC)
end

local function genFlagUpdater(behaviour, params)
    assert(#behaviour == 8)
    params = params or {}
    behaviour = behaviour:gsub('N', params.substraction and '1' or '0')
    local function bitMask(b)
        local val, i = 0, behaviour:find(b, 1)
        while i do
            val = bor(val, lshift(1, 8 - i))
            i = behaviour:find('1', i)
        end
        return val
    end
    local res, keep
    if behaviour:find'-' then
        keep = true
        res = {'local a, b, e, f = ...'}
        if behaviour:find'1' then res[#res] = string.format('f = bit.bor(f, %d)', bitMask'1') end
        if behaviour:find'0' then res[#res] = string.format('f = bit.band(f, %d)', bxor(bitMask'0', 0xFF)) end
    else
        keep = false
        res = {'local a, b, e = ...',
            string.format('local f = %d', bitMask'1')}
    end
    local s53 = false -- optimization for the common case when S, 5, 3 appears in and only in "S.5.3..."
    if behaviour:find '^S[^S53]5[^S53]3[^S53]+$' then
        res[#res+1] = string.format('f = bit.bor(bit.band(e, 0xC8), %s)', keep and 'bit.band(f, 0x37)' or 'f')
        s53 = true
    end
    if behaviour:find 'P' then
        res[#res+1] = [[local parityEven = bit.bxor(e, bit.rshift(e, 4))
            parityEven = bit.bxor(parityEven, bit.rshift(parityEven, 2))
            parityEven = bit.band(bit.bxor(parityEven, bit.rshift(parityEven, 1)), 1) == 0]]
    end
    for bit = 7, 0, -1 do
        local op = behaviour:sub(8 - bit, 8 - bit)
        local mask, nMask = lshift(1, bit), bxor(lshift(1, bit), 0xFF)
        local function s53Copy(idx)
            if not s53 then
                if keep then
                    res[#res+1] = string.format('f = bit.band(e, %d)~=0 and bit.bor(f, %d) or bit.band(f, %d)', lshift(1, idx), mask, nMask)
                elseif bit == idx then
                    res[#res+1] = string.format('f = bit.bor(bit.band(e, %d), f)', lshift(1, idx))
                else
                    res[#res+1] = string.format('f = bit.band(e, %d)~=0 and bit.bor(f, %d) or f', lshift(1, idx), mask)
                end
            end
        end
        if op:find'[?01-]' then
            -- if "undefined, reset, set or not affected" there's nothing else to do
        elseif op == 'S' then
            s53Copy(7)
        elseif op == '5' then
            s53Copy(5)
        elseif op == '3' then
            s53Copy(3)
        elseif op == 'Z' then
            res[#res+1] = string.format('f = bit.band(e, 0xFF)==0 and bit.bor(f, %d) or bit.band(f, %d)', mask, nMask)
        elseif op == 'H' then
            res[#res+1] = string.format('f = bit.band(bit.band(a, 0xF) + bit.band(b, 0xF), 0x10)~=0 '..
                'and bit.bor(f, %d) or bit.band(f, %d)', mask, nMask)
        elseif op == 'V' then
            -- overflow happens when (sign(a)!=sign(e)) and (sign(b)!=sign(e))
            -- this can be written as well as: (sign(a)==sign(b)) and (sign(a)!=sign(e))
            res[#res+1] = string.format('f = bit.band(bit.bxor(a, e), bit.bxor(b, e), 0x80)~=0 '..
                'and bit.bor(f, %d) or bit.band(f, %d)', mask, nMask)
        elseif op == 'P' then
            res[#res+1] = string.format('f = parityEven and bit.bor(f, %d) or bit.band(f, %d)', mask, nMask)
        elseif op == 'C' then
            res[#res+1] = string.format('f = bit.band(e, 0x100)~=0 and bit.bor(f, %d) or bit.band(f, %d)', mask, nMask)
        else
            error('invalid flag \''..op..'\' in behaviour')
        end
    end
    for _, line in pairs(params.exceptions or {}) do res[#res+1] = line end
    res[#res+1] = 'return f'
    return loadstring(table.concat(res, '\n'))
end

local opcodes8 = { -- opcode -> {code, bytes, cycles}
    [0x00] = {function(cpu) -- NOP
    end, 1, 4},
    [0x01] = {function(cpu, high, low) -- LD BC, nn
        cpu.b, cpu.c = high, low
    end, 3, 10},
    [0x02] = {function(cpu, high, low) -- LD (BC), A
        cpu.memory:write(word(cpu.b, cpu.c), cpu.a)
    end, 1, 7},
    [0x03] = {function(cpu) -- INC BC
        cpu.c = band(cpu.c + 1, 0xFF)
        if cpu.c == 0 then cpu.b = bit.band(cpu.b + 1, 0xFF) end
    end, 1, 6},
    [0x04] = {(function(fu) return function(cpu) -- INC B
        local b, e = cpu.b, cpu.b+1
        cpu.b = band(b, 0xFF)
        cpu.f = fu(b, 1, e, cpu.f)
    end end)(genFlagUpdater'SZ5H3VN-'), 1, 4},
    [0x05] = {(function(fu) return function(cpu) -- DEC B
        local b, e = cpu.b, cpu.b+0xFF
        cpu.b = band(b, 0xFF)
        cpu.f = fu(b, 0xFF, e, cpu.f)
    end end)(genFlagUpdater'SZ5H3VN-'), 1, 4},
    [0x06] = {function(cpu, byte) -- LD B, n
        cpu.b = byte
    end, 2, 7},
    [0x0A] = {function(cpu, high, low) -- LD A, (BC)
        cpu.a = cpu.memory:read(word(cpu.b, cpu.c))
    end, 1, 7},
    [0x0E] = {function(cpu, byte) -- LD C, n
        cpu.c = byte
    end, 2, 7},
    [0x11] = {function(cpu, high, low) -- LD DE, nn
        cpu.d, cpu.e = high, low
    end, 3, 10},
    [0x12] = {function(cpu, high, low) -- LD (DE), A
        cpu.memory:write(word(cpu.d, cpu.e), cpu.a)
    end, 1, 2},
    [0x13] = {function(cpu) -- INC DE
        cpu.e = band(cpu.e + 1, 0xFF)
        if cpu.e == 0 then cpu.d = band(cpu.d + 1, 0xFF) end
    end, 1, 6},
    [0x14] = {(function(fu) return function(cpu) -- INC D
        local d, e = cpu.d, cpu.d+1
        cpu.d = band(d, 0xFF)
        cpu.f = fu(d, 1, e, cpu.f)
    end end)(genFlagUpdater'SZ5H3VN-'), 1, 4},
    [0x16] = {function(cpu, byte) -- LD D, n
        cpu.d = byte
    end, 2, 7},
    [0x18] = {function(cpu, byte) -- JR e
        cpu.pc = cpu.pc + arshift(lshift(byte, 24), 24) -- sign extend byte
    end, 2, 12},
    [0x1A] = {function(cpu, high, low) -- LD A, (DE)
        cpu.a = cpu.memory:read(word(cpu.d, cpu.e))
    end, 1, 7},
    [0x1E] = {function(cpu, byte) -- LD E, n
        cpu.e = byte
    end, 2, 7},
    [0x20] = {function(cpu, byte) -- JR NZ, e
        if band(cpu.f, FLAG_Z) == 0 then
            cpu.pc = cpu.pc + arshift(lshift(byte, 24), 24) -- sign extend byte
            cpu.cycles = cpu.cycles + 5
        end
    end, 2, 7},
    [0x21] = {function(cpu, high, low) -- LD HL, nn
        cpu.h, cpu.l = high, low
    end, 3, 10},
    [0x23] = {function(cpu) -- INC HL
        cpu.l = band(cpu.l + 1, 0xFF)
        if cpu.l == 0 then cpu.h = band(cpu.h + 1, 0xFF) end
    end, 1, 6},
    [0x24] = {(function(fu) return function(cpu) -- INC H
        local h, e = cpu.h, cpu.h+1
        cpu.h = band(h, 0xFF)
        cpu.f = fu(h, 1, e, cpu.f)
    end end)(genFlagUpdater'SZ5H3VN-'), 1, 4},
    [0x26] = {function(cpu, byte) -- LD H, n
        cpu.h = byte
    end, 2, 7},
    [0x28] = {function(cpu, byte) -- JR Z, e
        if band(cpu.f, FLAG_Z) ~= 0 then
            cpu.pc = cpu.pc + arshift(lshift(byte, 24), 24) -- sign extend byte
            cpu.cycles = cpu.cycles + 5
        end
    end, 2, 7},
    [0x2E] = {function(cpu, byte) -- LD L, n
        cpu.l = byte
    end, 2, 7},
    [0x30] = {function(cpu, byte) -- JR NC, e
        if band(cpu.f, FLAG_C) == 0 then
            cpu.pc = cpu.pc + arshift(lshift(byte, 24), 24) -- sign extend byte
            cpu.cycles = cpu.cycles + 5
        end
    end, 2, 7},
    [0x31] = {function(cpu, high, low) -- LD SP, nn
        cpu.sp = word(high, low)
    end, 3, 10},
    [0x32] = {function(cpu, high, low) -- LD (nn), A
        cpu.memory:write(word(high, low), cpu.a)
    end, 3, 13},
    [0x33] = {function(cpu) -- INC SP
        cpu.sp = band(cpu.sp + 1, 0xFFFF)
    end, 1, 6},
    [0x38] = {function(cpu, byte) -- JR C, e
        if band(cpu.f, FLAG_C) ~= 0 then
            cpu.pc = cpu.pc + arshift(lshift(byte, 24), 24) -- sign extend byte
            cpu.cycles = cpu.cycles + 5
        end
    end, 2, 7},
    [0x3E] = {function(cpu, byte) -- LD A, n
        cpu.a = byte
    end, 2, 7},
    [0x40] = {function(cpu) -- LD B, B
    end, 1, 4},
    [0x41] = {function(cpu) -- LD B, C
        cpu.b = cpu.c
    end, 1, 4},
    [0x42] = {function(cpu) -- LD B, D
        cpu.b = cpu.d
    end, 1, 4},
    [0x43] = {function(cpu) -- LD B, E
        cpu.b = cpu.e
    end, 1, 4},
    [0x44] = {function(cpu) -- LD B, H
        cpu.b = cpu.h
    end, 1, 4},
    [0x45] = {function(cpu) -- LD B, L
        cpu.b = cpu.l
    end, 1, 4},
    [0x47] = {function(cpu) -- LD B, A
        cpu.b = cpu.a
    end, 1, 4},
    [0x48] = {function(cpu) -- LD C, B
        cpu.c = cpu.b
    end, 1, 4},
    [0x49] = {function(cpu) -- LD C, C
    end, 1, 4},
    [0x4A] = {function(cpu) -- LD C, D
        cpu.c = cpu.d
    end, 1, 4},
    [0x4B] = {function(cpu) -- LD C, E
        cpu.c = cpu.e
    end, 1, 4},
    [0x4C] = {function(cpu) -- LD C, H
        cpu.c = cpu.h
    end, 1, 4},
    [0x4D] = {function(cpu) -- LD C, L
        cpu.c = cpu.l
    end, 1, 4},
    [0x4F] = {function(cpu) -- LD C, A
        cpu.c = cpu.a
    end, 1, 4},
    [0x50] = {function(cpu) -- LD D, B
        cpu.d = cpu.b
    end, 1, 4},
    [0x51] = {function(cpu) -- LD D, C
        cpu.d = cpu.c
    end, 1, 4},
    [0x52] = {function(cpu) -- LD D, D
    end, 1, 4},
    [0x53] = {function(cpu) -- LD D, E
        cpu.d = cpu.e
    end, 1, 4},
    [0x54] = {function(cpu) -- LD D, H
        cpu.d = cpu.h
    end, 1, 4},
    [0x55] = {function(cpu) -- LD D, L
        cpu.d = cpu.l
    end, 1, 4},
    [0x57] = {function(cpu) -- LD D, A
        cpu.d = cpu.a
    end, 1, 4},
    [0x58] = {function(cpu) -- LD E, B
        cpu.e = cpu.b
    end, 1, 4},
    [0x59] = {function(cpu) -- LD E, C
        cpu.e = cpu.c
    end, 1, 4},
    [0x5A] = {function(cpu) -- LD E, D
        cpu.e = cpu.d
    end, 1, 4},
    [0x5B] = {function(cpu) -- LD E, E
    end, 1, 4},
    [0x5C] = {function(cpu) -- LD E, H
        cpu.e = cpu.h
    end, 1, 4},
    [0x5D] = {function(cpu) -- LD E, L
        cpu.e = cpu.l
    end, 1, 4},
    [0x5F] = {function(cpu) -- LD E, A
        cpu.e = cpu.a
    end, 1, 4},
    [0x60] = {function(cpu) -- LD H, B
        cpu.h = cpu.b
    end, 1, 4},
    [0x61] = {function(cpu) -- LD H, C
        cpu.h = cpu.c
    end, 1, 4},
    [0x62] = {function(cpu) -- LD H, D
        cpu.h = cpu.d
    end, 1, 4},
    [0x63] = {function(cpu) -- LD H, E
        cpu.h = cpu.e
    end, 1, 4},
    [0x64] = {function(cpu) -- LD H, H
    end, 1, 4},
    [0x65] = {function(cpu) -- LD H, L
        cpu.h = cpu.l
    end, 1, 4},
    [0x67] = {function(cpu) -- LD H, A
        cpu.h = cpu.a
    end, 1, 4},
    [0x68] = {function(cpu) -- LD L, B
        cpu.l = cpu.b
    end, 1, 4},
    [0x69] = {function(cpu) -- LD L, C
        cpu.l = cpu.c
    end, 1, 4},
    [0x6A] = {function(cpu) -- LD L, D
        cpu.l = cpu.d
    end, 1, 4},
    [0x6B] = {function(cpu) -- LD L, E
        cpu.l = cpu.e
    end, 1, 4},
    [0x6C] = {function(cpu) -- LD L, H
        cpu.l = cpu.h
    end, 1, 4},
    [0x6D] = {function(cpu) -- LD L, L
    end, 1, 4},
    [0x6F] = {function(cpu) -- LD L, A
        cpu.l = cpu.a
    end, 1, 4},
    [0x68] = {function(cpu) -- LD A, B
        cpu.a = cpu.b
    end, 1, 4},
    [0x69] = {function(cpu) -- LD A, C
        cpu.a = cpu.c
    end, 1, 4},
    [0x6A] = {function(cpu) -- LD A, D
        cpu.a = cpu.d
    end, 1, 4},
    [0x6B] = {function(cpu) -- LD A, E
        cpu.a = cpu.e
    end, 1, 4},
    [0x6C] = {function(cpu) -- LD A, H
        cpu.a = cpu.h
    end, 1, 4},
    [0x6D] = {function(cpu) -- LD A, L
        cpu.a = cpu.l
    end, 1, 4},
    [0x6F] = {function(cpu) -- LD A, A
    end, 1, 4},
    [0xB0] = {(function(fu) return function(cpu) -- OR B
        local a, b = cpu.a, cpu.b
        cpu.a = bor(cpu.a, cpu.b)
        cpu.f = fu(a, b, cpu.a)
    end end)(genFlagUpdater 'SZ503P00'), 1, 4},
    [0xB1] = {(function(fu) return function(cpu) -- OR C
        local a, c = cpu.a, cpu.c
        cpu.a = bor(cpu.a, cpu.c)
        cpu.f = fu(a, c, cpu.a)
    end end)(genFlagUpdater 'SZ503P00'), 1, 4},
    [0xB2] = {(function(fu) return function(cpu) -- OR D
        local a, d = cpu.a, cpu.d
        cpu.a = bor(cpu.a, cpu.d)
        cpu.f = fu(a, d, cpu.a)
    end end)(genFlagUpdater 'SZ503P00'), 1, 4},
    [0xB3] = {(function(fu) return function(cpu) -- OR E
        local a, e = cpu.a, cpu.e
        cpu.a = bor(cpu.a, cpu.e)
        cpu.f = fu(a, e, cpu.a)
    end end)(genFlagUpdater 'SZ503P00'), 1, 4},
    [0xB4] = {(function(fu) return function(cpu) -- OR H
        local a, h = cpu.a, cpu.h
        cpu.a = bor(cpu.a, cpu.h)
        cpu.f = fu(a, h, cpu.a)
    end end)(genFlagUpdater 'SZ503P00'), 1, 4},
    [0xB5] = {(function(fu) return function(cpu) -- OR L
        local a, l = cpu.a, cpu.l
        cpu.a = bor(cpu.a, cpu.l)
        cpu.f = fu(a, l, cpu.a)
    end end)(genFlagUpdater 'SZ503P00'), 1, 4},
    [0xB7] = {(function(fu) return function(cpu) -- OR A
        cpu.f = fu(cpu.a, cpu.a, cpu.a)
    end end)(genFlagUpdater 'SZ503P00'), 1, 4},
    [0xCD] = {function(cpu, high, low) -- CALL nn
        
        cpu.pc = word(high, low)
    end, 3, 17}
    [0xEF] = {function(cpu) -- RST $28
        local sp = cpu.sp
        cpu.memory:write(band(sp-1, 0xFFFF), rshift(cpu.pc, 8))
        cpu.memory:write(band(sp-2, 0xFFFF), band(cpu.pc, 0xFF))
        cpu.pc = 0x28
        cpu.sp = sp - 2
    end, 1, 11},
    [0xF3] = {function(cpu) -- DI
        cpu.irqEnabled = true
    end, 1, 4},
    [0xF7] = {function(cpu) -- RST $30
        local sp = cpu.sp
        cpu.memory:write(band(sp-1, 0xFFFF), rshift(cpu.pc, 8))
        cpu.memory:write(band(sp-2, 0xFFFF), band(cpu.pc, 0xFF))
        cpu.pc = 0x30
        cpu.sp = sp - 2
    end, 1, 11},
    [0xFB] = {function(cpu) -- EI
        cpu.irqEnabled = nil -- will be enabled after next instruction
    end, 1, 4},
    [0xFF] = {function(cpu) -- RST $38
        local sp = cpu.sp
        cpu.memory:write(band(sp-1, 0xFFFF), rshift(cpu.pc, 8))
        cpu.memory:write(band(sp-2, 0xFFFF), band(cpu.pc, 0xFF))
        cpu.pc = 0x38
        cpu.sp = sp - 2
    end, 1, 11},
}

function Cpu:dumpRegs(includeShadow)
    local res = {}
    res[#res+1] = string.format('A = $%02x, F = $%02x, BC = $%02x%02x, DE = $%02x%02x, HL = $%02x%02x',
        self.a, self.f, self.b, self.c, self.d, self.e, self.h, self.l)
    if includeShadow then
        res[#res+1] = string.format("A'= $%02x, F'= $%02x, BC'= $%02x%02x, DE'= $%02x%02x, HL'= $%02x%02x",
            self.a_, self.f_, self.b_, self.c_, self.d_, self.e_, self.h_, self.l_)
    end
    res[#res+1] = string.format('I = $%02x, R = $%02x, IX = $%04x, IY = $%04x, SP = $%04x, PC = $%04x',
        self.i, self.r, self.ix, self.iy, self.sp, self.pc)
    return table.concat(res, '\n')
end

function Cpu:step()
    if self.irqCount > 0 and self.irqEnabled then
        self.irqEnabled = false
        if self.irqMode == 0 then
            --
        end
    elseif self:halted() then -- run NOP:
        local code, _, cycles = opcodes8[0]
        self.cycles = self.cycles + cycles
        code(self)
    else
        local instr = self.memory:read(self.pc)
        -- EI doesn't enables interrupt, but it sets a flip-flop that enables interrupts after
        -- its following instruction is loaded. This was designed this way so that EI, RETI does
        -- not generate stack overflows on persisting interrupts. This intermediate state is
        -- indicated by a nil irqEnabled.
        if self.irqEnabled == nil then self.irqEnabled = true end
        if not opcodes8[instr] then
            error(string.format('unsupported instruction $%02x\n%s', instr, self:dumpRegs()))
        end
        local code, bytes, cycles = unpack(opcodes8[instr])
        self.pc = self.pc + bytes
        self.cycles = self.cycles + cycles
        if bytes == 1 then
            code(self)
        elseif bytes == 2 then
            code(self, self.memory:read(self.pc - 1))
        else
            code(self, self.memory:read(self.pc - 1), self.memory:read(self.pc - 2))
        end
    end
end

local IrqLine = {}
IrqLine.__index = IrqLine

function IrqLine:set(state)
    if state and not self.state then
        self.cpu.irqCount = self.cpu.irqCount + 1
    else
        self.cpu.irqCount = self.cpu.irqCount - 1
    end
    self.state = state
end

function Cpu:newIrqLine()
    return setmetatable({
        state = false,
        cpu = self
    }, IrqLine)
end

function Cpu:halted()
    return band(self.f, FLAG_H) ~= 0
end

return Cpu
