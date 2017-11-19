--local luacovDefaults = require 'luacov.defaults'
local luacov = require('luacov.runner')(setmetatable({ runreport = true, deletestats = true }, {__index=luacovDefaults}))
local CpuTest = require 'test.cpu6502'
CpuTest.run()
