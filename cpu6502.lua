local Cpu = {}
Cpu.__index = Cpu

local unpack = _G.unpack or table.unpack
local bit32 = _G.bit32 or require 'bit32'

local FLAG_N, FLAG_V, FLAG_B, FLAG_D, FLAG_I, FLAG_Z, FLAG_C = 0x80, 0x40, 0x10, 0x8, 0x4, 0x2, 0x1
local FLAG_NN, FLAG_NV, FLAG_NB, FLAG_ND, FLAG_NI, FLAG_NZ, FLAG_NC = 0x7F, 0xBF, 0xEF, 0xF7, 0xFB, 0xFD, 0xFE

local function swab(x) return bit32.bor(bit32.rshift(x, 8), bit32.lshift(bit32.band(x, 0xFF), 8)) end
local function page(x) return bit32.band(bit32.rshift(x, 8), 0xFF) end
local function updateZero(val, p)
    return val == 0 and bit32.bor(p, FLAG_Z) or bit32.band(p, FLAG_NZ)
end
local function updateNegative(val, p)
    return bit32.band(val, 0x80) ~= 0 and bit32.bor(p, FLAG_N) or bit32.band(p, FLAG_NN)
end
local function updateOverflow(val, p)
    return (val < -128 or val > 127) and bit32.bor(p, FLAG_V) or bit32.band(p, FLAG_NV)
end
local function updateCarry(val, p)
    return bit32.band(val, 0x100) ~= 0 and bit32.bor(p, FLAG_C) or bit32.band(p, FLAG_NC)
end

local function read16(memory, addr)
    return bit32.bor(memory:read(addr), bit32.lshift(memory:read(addr + 1), 8))
end

local function readWrap16(memory, addr, byte)
    if bit32.band(addr, 0xFF) == 0xFF then
        return bit32.bor(memory:read(addr), bit32.lshift(memory:read(addr - 0xFF), 8))
    end
    return bit32.bor(memory:read(addr), bit32.lshift(memory:read(addr + 1), 8))
end

local signExtendByte
if bit and bit.arshift then -- luajit
    signExtendByte = function(n) return bit.arshift(bit.lshift(n, 24), 24) end
else -- everything else:
    signExtendByte = function(n)
        n = bit32.band(n, 0xFF)
        return bit32.band(n, 0x80) ~= 0 and n - 256 or n
    end
end

local function adcImplementation(cpu, src)
    local a, p = 0, cpu.p
    -- decimal mode is a b*tch, sources:
    -- http://nesdev.com/6502.txt
    -- http://6502.org/tutorials/decimal_mode.html
    if bit32.band(p, FLAG_D) == FLAG_D then
        a = cpu.a + src + bit32.band(p, 1) -- plus carry
        if bit32.band(cpu.a, 0xf) + bit32.band(src, 0xf) + bit32.band(p, 1) > 9 then
            a = a + 6
        end
        p = updateOverflow(a, updateNegative(a, p))
        if a > 0x99 then
            a = a + 96
            p = bit32.bor(p, FLAG_C)
        else
            p = bit32.band(p, FLAG_NC)
        end
    else
        -- addition is carried on sign extended operands
        a = signExtendByte(cpu.a) + signExtendByte(src) + bit32.band(p, 1) -- plus carry
        p = updateOverflow(a, updateNegative(a, updateCarry(cpu.a + src + bit32.band(p, 1), p)))
    end
    cpu.a = bit32.band(a, 0xFF)
    cpu.p = updateZero(cpu.a, p)
end

local function branchGen(flag, state)
    local value = state and flag or 0
    return function(cpu, data)
        if bit32.band(cpu.p, flag) == value then
            local pc = bit32.band(cpu.pc + signExtendByte(data), 0xFFFF)
            cpu.cycles = cpu.cycles + (page(cpu.pc) ~= page(pc) and 2 or 1)
            cpu.pc = pc
        end
    end
end

function Cpu:new(params)
    return setmetatable({
        memory = params.memory,
        a = 0,
        x = 0,
        y = 0,
        s = 0xFF, -- stack pointer
        pc = read16(params.memory, 0xFFFC),
        p = 0x20, -- flags
        cycles = 0
    }, self)
end

function fmt(fmt, mut)
    return function(cpu, data)
        if mut == 'signExtendByte' then
            data = signExtendByte(data)
        end
        return string.format(fmt, data)
    end
end

local decodeRom = { -- opcode->{code, bytes, cycles}
    [0x00] = {fmt = fmt 'BRK', function(cpu)
        cpu.p = bit32.bor(cpu.p, FLAG_B)
        cpu.pc = read16(cpu.memory, 0xFFFE)
    end, 1, 7},
    [0x01] = {fmt = fmt 'ORA ($%02x, X) ; pre indexed indirect', function(cpu, data)
        cpu.a = bit32.bor(cpu.a, cpu.memory:read(readWrap16(cpu.memory, bit32.band(cpu.x + data, 0xFF))))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
        if bit32.band(data, 0xFF) + cpu.x >= 0x100 then cpu.cycles = cpu.cycles + 1 end
    end, 2, 6},
    -- [0x02] invalid
    -- [0x03] invalid
    -- [0x04] invalid
    [0x05] = {fmt = fmt 'ORA $%02x ; zp', function(cpu, data)
        cpu.a = bit32.bor(cpu.a, cpu.memory:read(data))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 2, 3},
    [0x06] = {fmt = fmt 'ASL $%02x ; zp', function(cpu, data)
        local m, p = cpu.memory:read(data), cpu.p
        p = bit32.band(m, 0x80) ~= 0 and bit32.bor(p, FLAG_C) or bit32.band(p, FLAG_NC)
        m = bit32.band(bit32.lshift(m, 1), 0xFF)
        cpu.memory:write(data, m)
        cpu.p = updateNegative(m, updateZero(m, p))
    end, 2, 5},
    -- [0x07] invalid
    [0x08] = {fmt = fmt 'PHP', function(cpu)
        cpu.memory:write(bit32.bor(0x100, cpu.s), cpu.p)
        cpu.s = bit32.band(cpu.s - 1, 0xFF)
    end, 1, 3},
    [0x09] = {fmt = fmt 'ORA #$%02x ; imm', function(cpu, data)
        cpu.a = bit32.bor(cpu.a, data)
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 2, 2},
    [0x0A] = {fmt = fmt 'ASL', function(cpu, data)
        local a, p = cpu.a, cpu.p
        p = bit32.band(a, 0x80) ~= 0 and bit32.bor(p, FLAG_C) or bit32.band(p, FLAG_NC)
        a = bit32.band(bit32.lshift(a, 1), 0xFF)
        cpu.a = a
        cpu.p = updateNegative(a, updateZero(a, p))
    end, 1, 2},
    -- [0x0B] invalid
    -- [0x0C] invalid
    [0x0D] = {fmt = fmt 'ORA $%04x ; absolute', function(cpu, data)
        cpu.a = bit32.bor(cpu.a, cpu.memory:read(data))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 3, 4},
    [0x0E] = {fmt = fmt 'ASL $%04x ; absolute', function(cpu, data)
        local m, p = cpu.memory:read(data), cpu.p
        p = bit32.band(m, 0x80) ~= 0 and bit32.bor(p, FLAG_C) or bit32.band(p, FLAG_NC)
        m = bit32.band(bit32.lshift(m, 1), 0xFF)
        cpu.memory:write(data, m)
        cpu.p = updateNegative(m, updateZero(m, p))
    end, 3, 6},
    -- [0x0F] invalid
    [0x10] = {fmt = fmt('BPL %d ; relative', 'signExtendByte'), branchGen(FLAG_N, false), 2, 2},
    [0x11] = {fmt = fmt 'ORA ($%02x), Y ; post indexed indirect', function(cpu, data)
        local base = readWrap16(cpu.memory, data)
        cpu.a = bit32.bor(cpu.a, cpu.memory:read(bit32.band(base + cpu.y, 0xFFFF)))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
        if bit32.band(base, 0xFF) + cpu.y >= 0x100 then cpu.cycles = cpu.cycles + 1 end
    end, 2, 5},
    -- [0x12] invalid
    -- [0x13] invalid
    -- [0x14] invalid
    [0x15] = {fmt = fmt 'ORA $%02x, X ; zp', function(cpu, data)
        cpu.a = bit32.bor(cpu.a, cpu.memory:read(bit32.band(data + cpu.x, 0xFF)))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 2, 4},
    [0x16] = {fmt = fmt 'ASL $%02x, X ; zp', function(cpu, data)
        local addr = bit32.band(data + cpu.x, 0xFF)
        local m, p = cpu.memory:read(addr), cpu.p
        p = bit32.band(m, 0x80) ~= 0 and bit32.bor(p, FLAG_C) or bit32.band(p, FLAG_NC)
        m = bit32.band(bit32.lshift(m, 1), 0xFF)
        cpu.memory:write(addr, m)
        cpu.p = updateNegative(m, updateZero(m, p))
    end, 2, 5},
    -- [0x17] invalid
    [0x18] = {fmt = fmt 'CLC', function(cpu)
        cpu.p = bit32.band(cpu.p, FLAG_NC)
    end, 1, 2},
    [0x19] = {fmt = fmt 'ORA $%04x, Y ; absolute', function(cpu, data)
        cpu.a = bit32.bor(cpu.a, cpu.memory:read(bit32.band(data + cpu.y, 0xffff)))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
        if bit32.band(data, 0xFF) + cpu.y >= 0x100 then cpu.cycles = cpu.cycles + 1 end
    end, 3, 4},
    -- [0x1A] invalid
    -- [0x1B] invalid
    -- [0x1C] invalid
    [0x1D] = {fmt = fmt 'ORA %04x, X ; absolute', function(cpu, data)
        cpu.a = bit32.bor(cpu.a, cpu.memory:read(bit32.band(data + cpu.x, 0xffff)))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
        if bit32.band(data, 0xFF) + cpu.x >= 0x100 then cpu.cycles = cpu.cycles + 1 end
    end, 3, 4},
    [0x1E] = {fmt = fmt 'ASL $%04x, X ; absolute', function(cpu, data)
        local addr = bit32.band(data + cpu.x, 0xFFFF)
        local m, p = cpu.memory:read(addr), cpu.p
        p = bit32.band(m, 0x80) ~= 0 and bit32.bor(p, FLAG_C) or bit32.band(p, FLAG_NC)
        m = bit32.band(bit32.lshift(m, 1), 0xFF)
        cpu.memory:write(addr, m)
        cpu.p = updateNegative(m, updateZero(m, p))
        if bit32.band(data, 0xFF) + cpu.x >= 0x100 then cpu.cycles = cpu.cycles + 1 end
    end, 3, 7},
    -- [0x1F] invalid
    [0x20] = {fmt = fmt 'JSR $%04x ; absolute', function(cpu, data)
	local pc = cpu.pc - 1
	cpu.memory:write(bit32.bor(0x100, cpu.s), bit32.rshift(pc, 8))
	cpu.memory:write(bit32.bor(0x100, bit32.band(cpu.s-1, 0xFF)), bit32.band(pc, 0xFF))
	cpu.s = bit32.band(cpu.s - 2, 0xFF)
	cpu.pc = data
    end, 3, 6},
    [0x21] = {fmt = fmt 'AND ($%02x, X) ; pre indexed indirect', function(cpu, data)
        cpu.a = bit32.band(cpu.a, cpu.memory:read(readWrap16(cpu.memory, bit32.band(cpu.x + data, 0xFF))))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
        if bit32.band(data, 0xFF) + cpu.x >= 0x100 then cpu.cycles = cpu.cycles + 1 end
    end, 2, 6},
    -- [0x22] invalid
    -- [0x23] invalid
    [0x24] = {fmt = fmt 'BIT $%02x ; zp', function(cpu, data)
        local m = cpu.memory:read(data)
        local MASK = bit32.bor(FLAG_N, FLAG_V)
        local NMASK = bit32.bxor(0xFF, MASK)
        cpu.p = updateNegative(bit32.band(cpu.a, m), bit32.bor(bit32.band(m, MASK), bit32.band(cpu.p, NMASK)))
    end, 2, 3},
    [0x25] = {fmt = fmt 'AND $%02x ; zp', function(cpu, data)
        cpu.a = bit32.band(cpu.a, cpu.memory:read(data))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 2, 3},
    [0x26] = {fmt = fmt 'ROL $%02x ; zp', function(cpu, data)
        local m = bit32.bor(bit32.lshift(cpu.memory:read(data), 1), bit32.band(cpu.p, FLAG_C))
        cpu.p = bit32.bor(bit32.band(cpu.p, FLAG_NC), bit32.rshift(m, 8))
        m = bit32.band(m, 0xFF)
        cpu.p = updateNegative(m, updateZero(m, cpu.p))
        cpu.memory:write(data, m)
    end, 2, 5},
    -- [0x27] invalid
    [0x28] = {fmt = fmt 'PLP', function(cpu)
        cpu.s = bit32.band(cpu.s + 1, 0xFF)
        cpu.p = cpu.memory:read(bit32.bor(0x100, cpu.s))
    end, 1, 4},
    [0x29] = {fmt = fmt 'AND #$%02x ; imm', function(cpu, data)
        cpu.a = bit32.band(cpu.a, data)
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 2, 2},
    [0x2A] = {fmt = fmt 'ROL', function(cpu)
        local a = bit32.bor(bit32.lshift(cpu.a, 1), bit32.band(cpu.p, FLAG_C))
        cpu.p = bit32.bor(bit32.band(cpu.p, FLAG_NC), bit32.rshift(a, 8))
        cpu.a = bit32.band(a, 0xFF)
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 1, 2},
    -- [0x2B] invalid
    [0x2C] = {fmt = fmt 'BIT $%04x ; absolute', function(cpu, data)
        local m = cpu.memory:read(data)
        local MASK = bit32.bor(FLAG_N, FLAG_V)
        local NMASK = bit32.bxor(0xFF, MASK)
        cpu.p = updateNegative(bit32.band(cpu.a, m), bit32.bor(bit32.band(m, MASK), bit32.band(cpu.p, NMASK)))
    end, 3, 4},
    [0x2D] = {fmt = fmt 'AND $%04x ; absolute', function(cpu, data)
        cpu.a = bit32.band(cpu.a, cpu.memory:read(data))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 3, 4},
    [0x2E] = {fmt = fmt 'ROL $%04x ; absolute', function(cpu, data)
        local m = bit32.bor(bit32.lshift(cpu.memory:read(data), 1), bit32.band(cpu.p, FLAG_C))
        local p = bit32.bor(bit32.band(cpu.p, FLAG_NC), bit32.rshift(m, 8))
        m = bit32.band(m, 0xFF)
        cpu.p = updateNegative(m, updateZero(m, p))
        cpu.memory:write(data, m)
    end, 3, 6},
    -- [0x2F] invalid
    [0x30] = {fmt = fmt('BMI %d ; relative', 'signExtendByte'), branchGen(FLAG_N, true), 2, 2},
    [0x45] = {fmt = fmt 'EOR $%02x ; zp', function(cpu, data)
        cpu.a = bit32.bxor(cpu.a, cpu.memory:read(data))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 2, 3},
    [0x50] = {fmt = fmt('BVC %d ; relative', 'signExtendByte'), branchGen(FLAG_V, false), 2, 2},
    [0x58] = {fmt = fmt 'CLI', function(cpu)
        cpu.p = bit32.band(cpu.p, FLAG_NI)
    end, 1, 2},
    [0x60] = {fmt = fmt 'RTS', function(cpu)
        cpu.s = bit32.band(cpu.s + 2, 0xFF)
        cpu.pc = bit32.band(readWrap16(cpu.memory, bit32.bor(0x100, bit32.band(cpu.s-1, 0xFF))) + 1, 0xFFFF)
    end, 1, 6},
    [0x61] = {fmt = fmt 'ADC ($%02x, X) ; pre indexed indirect', function(cpu, data)
        return adcImplementation(cpu, cpu.memory:read(readWrap16(cpu.memory, bit32.band(cpu.x + data, 0xFF))))
    end, 2, 6},
    [0x65] = {fmt = fmt 'ADC $%02x ; zp', function(cpu, data)
        return adcImplementation(cpu, cpu.memory:read(data))
    end, 2, 3},
    [0x66] = {fmt = fmt 'ROR $%02x ; zp', function(cpu, data)
        local oldcarry = bit32.band(cpu.p, 1)
        local m = cpu.memory:read(data)
        cpu.p = bit32.bor(bit32.band(cpu.p, FLAG_NC), bit32.band(m, 1))
        m = bit32.bor(bit32.rshift(m, 1), bit32.lshift(oldcarry, 7))
        cpu.p = updateNegative(m, updateZero(m, cpu.p))
        cpu.memory:write(data, m)
    end, 2, 5},
    [0x68] = {fmt = fmt 'PLA', function(cpu)
        cpu.s = bit32.band(cpu.s + 1, 0xFF)
        cpu.a = cpu.memory:read(bit32.bor(0x100, cpu.s))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 1, 4},
    [0x69] = {fmt = fmt 'ADC #$%02x ; imm', adcImplementation, 2, 2},
    [0x6A] = {fmt = fmt 'ROR', function(cpu)
        local oldcarry = bit32.band(cpu.p, 1)
        cpu.p = bit32.bor(bit32.band(cpu.p, FLAG_NC), bit32.band(cpu.a, 1))
        cpu.a = bit32.bor(bit32.rshift(cpu.a, 1), bit32.lshift(oldcarry, 7))
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 1, 2},
    [0x6D] = {fmt = fmt 'ADC $%04x ; absolute', function(cpu, data)
        return adcImplementation(cpu, cpu.memory:read(data))
    end, 3, 4},
    [0x70] = {fmt = fmt('BVS %d ; relative', 'signExtendByte'), branchGen(FLAG_V, true), 2, 2},
    [0x71] = {fmt = fmt 'ADC ($%02x), Y ; post indexed indirect', function(cpu, data)
        local base = readWrap16(cpu.memory, data)
        if bit32.band(base, 0xFF) + cpu.y >= 0x100 then cpu.cycles = cpu.cycles + 1 end
        return adcImplementation(cpu, cpu.memory:read(bit32.band(base + cpu.y, 0xFFFF)))
    end, 2, 5},
    [0x75] = {fmt = fmt 'ADC $%02x, X ; x in zp', function(cpu, data)
        return adcImplementation(cpu, cpu.memory:read(bit32.band(cpu.x + data, 0xFF)))
    end, 2, 4},
    [0x79] = {fmt = fmt 'ADC $%02x, Y ; absolute + y', function(cpu, data)
        if bit32.band(data, 0xFF) + cpu.y >= 0x100 then cpu.cycles = cpu.cycles + 1 end
        return adcImplementation(cpu, cpu.memory:read(bit32.band(cpu.y + data, 0xFFFF)))
    end, 3, 4},
    [0x7D] = {fmt = fmt 'ADC $%02x, X ; absolute + x', function(cpu, data)
        if bit32.band(data, 0xFF) + cpu.x >= 0x100 then cpu.cycles = cpu.cycles + 1 end
        return adcImplementation(cpu, cpu.memory:read(bit32.band(cpu.x + data, 0xFFFF)))
    end, 3, 4},
    [0x85] = {fmt = fmt 'STA $%02x ; zp', function(cpu, data)
        cpu.memory:write(data, cpu.a)
    end, 2, 3},
    [0x86] = {fmt = fmt 'STX $%02x ; zp', function(cpu, data)
        cpu.memory:write(data, cpu.x)
    end, 2, 3},
    [0x8A] = {fmt = fmt 'TXA', function(cpu)
        cpu.a = cpu.x
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 1, 2},
    [0x90] = {fmt = fmt('BCS %d ; relative', 'signExtendByte'), branchGen(FLAG_C, false), 2, 2},
    [0x98] = {fmt = fmt 'TYA', function(cpu)
        cpu.a = cpu.y
        cpu.p = updateNegative(cpu.a, updateZero(cpu.a, cpu.p))
    end, 1, 2},
    [0xA2] = {fmt = fmt 'LDX #$%02x ; imm', function(cpu, data)
        cpu.x = data
        cpu.p = updateNegative(data, updateZero(data, cpu.p))
    end, 2, 2},
    [0xA8] = {fmt = fmt 'TAY', function(cpu)
        cpu.y = cpu.a
        cpu.p = updateNegative(cpu.y, updateZero(cpu.y, cpu.p))
    end, 1, 2},
    [0xA9] = {fmt = fmt 'LDA #$%02x ; imm', function(cpu, data)
        cpu.a = data
        cpu.p = updateNegative(data, updateZero(data, cpu.p))
    end, 2, 2},
    [0xB0] = {fmt = fmt('BCS %d ; relative', 'signExtendByte'), branchGen(FLAG_C, true), 2, 2},
    [0xB8] = {fmt = fmt 'CLV', function(cpu)
        cpu.p = bit32.band(cpu.p, FLAG_NV)
    end, 1, 2},
    [0xC8] = {fmt = fmt 'INY', function(cpu)
        cpu.y = bit32.band(cpu.y + 1, 0xFF)
        cpu.p = updateNegative(cpu.y, updateZero(cpu.y, cpu.p))
    end, 1, 2},
    [0xCA] = {fmt = fmt 'DEX', function(cpu)
        cpu.x = bit32.band(cpu.x - 1, 0xFF)
        cpu.p = updateNegative(cpu.x, updateZero(cpu.x, cpu.p))
    end, 1, 2},
    [0xD0] = {fmt = fmt('BNE %d ; relative', 'signExtendByte'), branchGen(FLAG_Z, false), 2, 2},
    [0xD8] = {fmt = fmt 'CLD', function(cpu)
        cpu.p = bit32.band(cpu.p, FLAG_ND)
    end, 1, 2},
    [0xE8] = {fmt = fmt 'INX', function(cpu)
        cpu.x = bit32.band(cpu.x + 1, 0xFF)
        cpu.p = updateNegative(cpu.x, updateZero(cpu.x, cpu.p))
    end, 1, 2},
    [0xEA] = {fmt = fmt 'NOP', function(cpu)
    end, 1, 2},
    [0xF0] = {fmt = fmt('BEQ %d ; relative', 'signExtendByte'), branchGen(FLAG_Z, true), 2, 2},
    [0xF8] = {fmt = fmt 'SED', function(cpu)
        cpu.p = bit32.bor(cpu.p, FLAG_D)
    end, 1, 2},
}

function Cpu:step()
    local instr = self.memory:read(self.pc)
    if not decodeRom[instr] then
        error(('invalid opcode $%02x at PC=$%04x'):format(instr, self.pc))
    end
    local code, bytes, cycles = unpack(decodeRom[instr])
    self.pc = self.pc + bytes
    self.cycles = self.cycles + cycles
    if bytes == 1 then
        code(self)
    elseif bytes == 2 then
        code(self, self.memory:read(self.pc - 1))
    else
        code(self, read16(self.memory, self.pc - 2))
    end
end

function Cpu:dissassemble(addr)
    addr = addr or self.pc
    local instr = self.memory:read(addr)
    if not decodeRom[instr] then
        return ('unknown opcode $%02x at $%04x'):format(instr, self.pc)
    end
    local bytes = decodeRom[instr][2]
    if bytes == 1 then
        return decodeRom[instr].fmt(self)
    elseif bytes == 2 then
        return decodeRom[instr].fmt(self, self.memory:read(bit32.band(addr + 1, 0xFFFF)))
    else
        return decodeRom[instr].fmt(self, read16(self.memory, bit32.band(addr + 1, 0xFFFF)))
    end
end

return Cpu
-- vi: et sw=4
