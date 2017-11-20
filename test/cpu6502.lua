local Cpu = require 'cpu6502'
local Memory = require 'memory'
local IoChips = require 'iochips'

local CpuTest = {}

local bit32 = _G.bit32 or require 'bit32'

local function assemble(code)
    local file = assert(io.popen('/bin/mktemp', 'r'))
    local outfile = file:read'*l'
    assert(file:close())
    local ok, ret = xpcall(function()
        file = assert(io.open(outfile .. '.asm', 'w'))
        file:write(code..'\n')
        file:close()
	file = assert(io.popen(('xa %s -o %s 2>&1'):format(outfile..'.asm', outfile), 'r'))
	local text = file:read('*a')
	assert(text == '', 'error compiling:\n'..text)
	file:close()
	file = assert(io.open(outfile, 'rb'))
	local ret = file:read('*a')
	file:close()
	return ret
    end, debug.traceback)
    os.remove(outfile)
    os.remove(outfile .. '.asm')
    if not ok then error(ret) end
    return ret
end

function CpuTest.run()
    -- this computer has 16 pages of $1000 bytes (4kB) each,
    local rom = Memory.Rom:new(assemble[[
    * = $e000
        cld
    test1: .( // test 1: simple load & store
            lda #1
            sta $2a
    .)
    test2: .( // test 2: testing inx, iny
            ldx #0
            inx
            txa
            tay
            iny
            tya
            sta $2a
    .)
    test3: .( // test 3: testing flags
            ldx #0
            cld
            sed
            php : pla ; loading flags in a
            and #$08 ; testing for bcD bit
            beq off
            inx
        off:
            cld
            php : pla : and #$08
            bne on ; bcD bit should be 0 after cld
            inx
        on: inx ; should be 3
            stx $2a
    .)
    test4: .(
        num1 = $0
        num2 = $1
            lda #$ce
            ldx #$f7
            sta num1
            stx num2
            jsr mult
            eor num1 ; xoring low and high bytes should give 4
            sta $2a
            bne test5 ; branch always
        mult: // a:num1 = (num1 * num2)
              // ref: http://codebase64.org/doku.php?id=base:short_8bit_multiplication_16bit_product
            lda #0
            ldx #8
            clc
        m0: bcc m1
            clc
            adc num2
        m1: ror
            ror num1
            dex
            bpl m0
            rts
    .)
    test5:
    .dsb $fffc - *
    .word test1, test1
    ]]) -- rom will be located at $e000-$ffff and
    local ram = Memory.Ram:new(8192) -- ram at $0000 - $1fff
    local out = {}
    local completedTest = nil
    -- overriding ram metatable:
    local TracedRam = setmetatable({}, getmetatable(ram))
    TracedRam.__index = TracedRam
    setmetatable(ram, TracedRam)
    function TracedRam:write(addr, byte)
        if addr == 0x2a then completedTest = byte end
        return Memory.Ram.write(self, addr, byte)
    end
    local cpu = Cpu:new{ memory = Memory.Mapper:new(8192, {[0]={ram}, [7]={rom}})}
    local nextTest = 1
    local tests = {
        function() end, -- nothing to check in first test
        function() end, -- nothing to check in inc test
        function() end, -- nothing to check in flags test
        function() end, -- nothing to check in multiplication test
    }
    while nextTest <= #tests do
        -- print(('A:%02x, X:%02x, Y:%02x, S:%02x, PC:%04x, P:%02x, ram:[%02x %02x]')
        --         :format(cpu.a, cpu.x, cpu.y, cpu.s, cpu.pc, cpu.p, cpu.memory:read(0), cpu.memory:read(1)),
        --     cpu:dissassemble())
        cpu:step()
        if completedTest then
            assert(completedTest == bit32.band(nextTest, 0xff),
                ('test must be run in order, got $%02x expecting $%02x'):format(completedTest, nextTest))
            tests[completedTest]()
            nextTest = nextTest + 1
            completedTest = nil
        end
    end
end

return CpuTest
-- vi: et sw=4
