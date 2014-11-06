local x
local function y() print(x()) end
local function x() print(5) end
-- x = function () print(5) end
y()
