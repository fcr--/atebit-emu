local Memory = require 'memory'
local IoChips = require 'iochips'

function createComputer(romFileName)
    local fd = assert(open(romFileName, 'rb'))
    local rom = Memory.Rom:new(fd:read('*a'), 8192)
    fd:close()
    local mapper = Memory.Mapper(8192, {
        [0] = Memory.Ram:new(8192),
        [6] = io,
        [7] = rom
    })
end