local Cpu = require 'cpuz80a'
local Memory = require 'memory'
local IoChips = require 'iochips'

local CpuTest = {}

function CpuTest.assemble(code, debugDisasm)
    local file = assert(io.popen('/bin/mktemp', 'r'))
    local tmpBase = file:read'*a':gmatch'[^\n]+'()
    assert(file:close())
    local ok, ret = xpcall(function()
        file = assert(io.open(tmpBase .. '.S', 'w'))
        file:write(code..'\n')
        file:close()
        file = assert(io.popen([[
            z80-unknown-coff-as ]]..tmpBase..[[.S -o ]]..tmpBase..[[.o;
            z80-unknown-coff-ld ]]..tmpBase..[[.o -o ]]..tmpBase..[[ 2>&1 ]], 'r'))
        local text = file:read('*a')
        assert(text == '', 'error compiling:\n'..text)
        file:close()
        file = assert(io.open(tmpBase, 'rb'))
        local ret = file:read('*a')
        file:close()
        if debugDisasm then
            os.execute([[z80-unknown-coff-objdump -bbinary -mz80 -D ]]..tmpBase)
        end
        return ret
    end, debug.traceback)
    os.remove(tmpBase)
    os.remove(tmpBase .. '.S')
    os.remove(tmpBase .. '.o')
    if not ok then error(ret) end
    return ret
end

function CpuTest.run()
    local rombin = assert(CpuTest.assemble([[
            ORG $0000
            Data8251 = $4000
            Command8251 = $4001

        Init8251Macro: MACRO
                LD A, %01001101; 8N1 1x
                LD Command8251, A
                LD A, %00110111; transmit&receive
                LD Command8251, A
        ENDM

        PutCharMacro: MACRO
                PUSH AF
            .checkReady:
                LD A, (Command8251)
                BIT 0, A
                JR Z, .checkReady
                POP AF
                LD Data8251, A
        ENDM

        MainMacro: MACRO
                LD DE, .hello
            .nextChar:
                LD A, (DE)
                OR A
                JR Z, .stringEnd
                CALL PutChar
                JR .nextChar
            .stringEnd:
                HALT
            .hello:
                DB "Hello, world!", 0
        ENDM

        ZERO:
            LD SP, $2000
            EI
            Init8251Macro
            JR Main
            DS $38 - (. - ZERO)
        Isr: EI
            RETI
        Main:
            MainMacro
            HALT
        PutChar:
            PutCharMacro
            RET]], true))
    local rom = Memory.Rom:new(rombin)
    local ram = Memory.Ram:new(8192)
    local serialBuffer = {}
    local usart = IoChips.Intel8251:new{ callback = function(b) table.insert(serialBuffer, string.char(b)) end }
    local cpu = Cpu:new{ memory = Memory.Mapper:new(8192, {[0]={rom}, {ram}, {usart}}) }
    usart:setIrqLine(cpu:newIrqLine())
    while not cpu:halted() do
        cpu:step()
    end
    assert(table.concat(serialBuffer, '') == 'Hello, world!')
end

return CpuTest

