local Adt = require 'adt'

local IoChips = {}

------------------------- INTEL 8251

-- Mode Word: async = [ S2 | S1 | EP | PEN | L2 | L1 | B2 | B1 ], sync = [ SCS | ESD | EP | PEN | L2 | L1 | 0 | 0 ]
--   Number of Stop Bits: (S2, S1): (0, 0) = invalid, (0, 1) = 1bit, (1, 0) = 1½bits, (1, 1) = 2bits.
--   Even Parity (EP): (0) = odd, (1) = even.
--   Parity Enable (PEN): (0) = disable, (1) = enable.
--   Character Length (L2, L1): (0, 0) = 5bits, (0, 1) = 6bits, (1, 0) = 7bits, (1,1) = 8bits.
--   Baud Rate (B2, B1): (0, 1) = 1x, (1, 0) = 16x, (1, 1) = 64x.
--   Single Character Sync: (SCS): (0) = double sync character, (1) = single sync character.
--   External Sync Detect: (ESD): (0) = SYNDET is an output, (1) = SYNDET is an input.
-- Command Word: [ EH | IR | RTS | ER | SBRK | RxE | DTR | TxEN ]
--   Enter Hunt mode (EH): (1) = enable search for Sync Character.
--   Internal Reset (IR): "high" returns 8251 to Mode Instruction Format.
--   Request to Send (RTS): "high" will force \hat{RTS} output to zero.
--   Error Reset (ER): (1) = reset error flags FE, OE, PE.
--   Send Break Character (SBRK): (0) = normal operation, (1) = forces TxD "low".
--   Receive Enable (RxE): (0) = disabel, (1) = enable.
--   Data Terminal Ready (DTR): "high" will force \hat{DTR} output to zero.
--   Transmit Enable (TxEN): (0) = disable, (1) = enable.
-- Status Word: [ DSR | SYNDET | FE | OE | PE | TxE | RxRDY | TxRDY ]
--   Data Set Ready (DSR): copy of DSR pin input value
--   Sync Detect (SYNDET): When set for internal sync detect indicates that character
--                         sync has been achieved and 8251 is ready for data.
--   Framing Error (FE): (1) = Stop bit not detected. Does not inhibit operation.
--   Overrun Error (OE): (1) = A character was received before the previous one was read.
--   Parity Error (PE): (1) = Invalid parity value detected in received character.
--   Transmitter Empty (TxE): (1) = No character is queued nor being sent.
--   Receive Ready (RxRDY): (1) = Input character queued. Flag cleared on read.
--   Transmitter Ready (TxRDY): (1) = USART is ready to accept a data character or command.

IoChips.Intel8251 = {}
IoChips.Intel8251.__index = IoChips.Intel8251

local dummyIrqLine = { set = function()end }

function IoChips.Intel8251:new(params)
    local res = setmetatable({
        buffer = Adt.MutableQueue:new(),
        irqLine = params.irqLine or dummyIrqLine,
        callback = params.callback,
        receiveEnabled = false,
        transmitEnabled = false,
        lastByte = 0
    }, self)
    res:reset()
    return res
end

function IoChips.Intel8251:setIrqLine(line)
    self.irqLine = line
    self.irqLine:set(self.transmitEnabled or self.receiveEnabled and #self.buffer > 0)
end

function IoChips.Intel8251:reset()
    self.state = 'ExpectingMode'
    self.errors = 0
    if self.irqLine then self.irqLine:set(false) end
end

function IoChips.Intel8251:read(addr)
    if bit.band(addr, 1) then --status word
        local word = 0x04 -- TxE is always "high"
        if self.receiveEnabled and self.state == 'ExpectingCommand' and #self.buffer > 0 then
            word = bit.bor(word, 2) -- RxRDY "high"
        end
        if self.transmitEnabled then bit.bor(word, 1) end
        return word
    else
        if #self.buffer > 0 then
            self.lastByte = self.buffer:pop()
            self.irqLine:set(self.transmitEnabled or self.receiveEnabled and #self.buffer > 0)
        end
        return self.lastByte
    end
end

function IoChips.Intel8251:write(addr, byte)
    if bit.band(addr, 1) then -- control word
        if self.state == 'ExpectingMode' and bit.band(byte, 3) == 0 then -- sync
            self.mode = {word=byte, async=false, scs=bit.band(byte, 0x80)==0x80, esd=bit.band(byte, 0x40)==0x40,
                ep=bit.band(byte, 0x20)==0x20, pen=bit.band(byte, 0x10)==0x10, charSize=({[0]=5,6,7,8})[bit.band(bit.rshift(byte, 2))]}
            self.state = 'ExpectingSyncChar1'
        elseif self.state == 'ExpectingMode' then -- async
            self.mode = {word=byte, async=true, stopBits=({1, 1.5, 2})[bit.band(3, bit.rshift(byte, 6))],
                ep=bit.band(byte, 0x20)==0x20, pen=bit.band(byte, 0x10)==0x10, charSize=({[0]=5,6,7,8})[bit.band(3, bit.rshift(byte, 2))],
                baudRate=({1,16,64})[bit.band(byte,3)]}
            self.state = 'ExpectingCommand'
        elseif self.state == 'ExpectingSyncChar1' then
            self.mode.syncChar1 = byte
            self.state = self.mode.scs and 'ExpectingCommand' or 'ExpectingSyncChar2'
        elseif self.state == 'ExpectingSyncChar2' then
            self.mode.syncChar2 = byte
            self.state = 'ExpectingCommand'
        elseif self.state == 'ExpectingCommand' then -- EH, RTS, SBRK, DTR are ignored
            if bit.band(byte, 0x40) ~= 0 then return self:reset() end -- internal reset
            if bit.band(byte, 0x10) ~= 0 then self.errors = 0 end
            self.receiveEnabled = bit.band(byte, 4) ~= 0
            self.transmitEnabled = bit.band(byte, 1) ~= 0
            self.irqLine:set(self.transmitEnabled or self.receiveEnabled and #self.buffer > 0)
        end
    else -- data:
        self.callback(byte)
    end
end

function IoChips.Intel8251:send(byteOrText)
    if type(byteOrText) == 'string' then
        for i = 1, #byteOrText do self.buffer:push(byteOrText:byte(i)) end
    elseif type(byteOrText) == 'number' then
        self.buffer:push(bit.band(byteOrText, 0xFF))
    else
        error'byteOrText must be a number or a string'
    end
    self.irqLine:set(self.transmitEnabled or self.receiveEnabled and #self.buffer > 0)
end



------------------------- INTEL 8255

IoChips.Intel8255 = {}
IoChips.Intel8255.__index = IoChips.Intel8255

function IoChips.Intel8255:new()
    return setmetatable({controlRegister = 0, a = 0, b = 0, c = 0}, self)
end

function IoChips.Intel8255:write(addr, byte)
    addr = bit.band(addr, 0x3)
    if addr == 0 then
        self.a = byte
        self:writeA(byte)
    end
end

function IoChips.Intel8255:read(addr, byte)
end

-- override the following function to interface to the chip
function IoChips.Intel8255:writeA(byte) end
function IoChips.Intel8255:writeB(byte) end
function IoChips.Intel8255:writeC(byte) end
function IoChips.Intel8255:readA() return 0 end
function IoChips.Intel8255:readB() return 0 end
function IoChips.Intel8255:readC() return 0 end




return IoChips