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
    .dsb $fffe - *
    .word test1
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
        function() end, -- nothing to check in inc test
    }
    while nextTest <= #tests do
        --print(('$%04x'):format(cpu.pc), cpu:dissassemble())
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
