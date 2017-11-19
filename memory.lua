local Memory = {}

Memory.Ram = {}
Memory.Ram.__index = Memory.Ram

function Memory.Ram:new(size)
    assert(size == math.floor(size) and math.frexp(size) == 0.5, 'size must be an integer power of two')
    local bytes = {}
    for i = 0, size-1 do
        bytes[i] = 0
    end
    return setmetatable({bytes = bytes, mask = bit.tobit(size - 1)}, self)
end

function Memory.Ram:read(addr)
    return self.bytes[bit.band(addr, self.mask)]
end

function Memory.Ram:write(addr, byte)
    self.bytes[bit.band(addr, self.mask)] = byte
end

function Memory.Ram:ipairs(from, to) -- optionals
    from = from or 0
    to = to or self.mask
    assert(from >= 0 and to <= self.mask, ('ilegal memory range (%d, %d)'):format(from, to))
    return function()
        if from <= to then
            local byte = self:read(byte)
            from = from + 1
            return byte
        end
    end
end



Memory.Rom = {}
Memory.Rom.__index = Memory.Rom
-- usage:
-- local fd = assert(open(filename, 'rb'))
-- local rom = Memory.Rom:new(fd:read('*a'))
-- fd:close()
function Memory.Rom:new(bytes, size)
    size = size or (#bytes == 0 and 0 or 2^select(2, math.frexp(#bytes-1)))
    assert(size == math.floor(size) and math.frexp(size) == 0.5, 'size must be an integer power of two')
    return setmetatable({bytes = bytes, mask = bit.tobit(size - 1)}, self)
end

function Memory.Rom:read(addr)
    return self.bytes:byte(bit.band(addr, self.mask)+1) or 0
end

function Memory.Rom.write() end



Memory.Mapper = {}
Memory.Mapper.__index = Memory.Mapper

-- usage:
-- Memory.Mapper:new(pageSize, {
--   [pageNumber] = {memoryObject, baseOffset}
--   ...
-- })
function Memory.Mapper:new(pageSize, mapping)
    assert(pageSize == math.floor(pageSize) and math.frexp(pageSize) == 0.5, 'pageSize must be an integer power of two')
    return setmetatable({
        pageSizeBits = select(2, math.frexp(pageSize)) - 1,
        addrMask = bit.tobit(pageSize - 1),
        mapping = mapping
    }, self)
end

function Memory.Mapper:read(addr)
    local handler = self.mapping[bit.rshift(addr, self.pageSizeBits)]
    if handler then
        local memoryObject, baseOffset = unpack(handler)
        return memoryObject:read(bit.band(addr, self.addrMask) + (baseOffset or 0))
    end
    return 0
end

function Memory.Mapper:write(addr, byte)
    local handler = self.mapping[bit.rshift(addr, self.pageSizeBits)]
    if handler then
        local memoryObject, baseOffset = unpack(handler)
        memoryObject:write(bit.band(addr, self.addrMask) + (baseOffset or 0), byte)
    end
end

return Memory