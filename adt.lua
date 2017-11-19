local Adt = {}

Adt.MutableQueue = {}
Adt.MutableQueue.__index = Adt.MutableQueue

function Adt.MutableQueue:new(list)
    return setmetatable({
        q = list or {},
        first = 1,
        last = list and #list or 0
    }, self)
end

function Adt.MutableQueue:push(item)
    self.last = self.last + 1
    self.q[self.last] = item
    return self
end

function Adt.MutableQueue:pop()
    if self.first <= self.last then
        local value = self.q[self.first]
        self.q[self.first] = nil
        self.first = self.first + 1
        return value
    end
end

function Adt.MutableQueue:consume()
    return function() return self:pop() end
end

function Adt.MutableQueue:__len()
    return self.last - self.first+1
end

return Adt
-- vi: et sw=4
