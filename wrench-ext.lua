local is_useful_notification
local should_use_internal_pop
local private_config = {}

local M = {}

local file_exists = function(name)
   local f=io.open(name,"r")
   if f ~= nil then
      io.close(f)
      return true
   else
      return false
   end
end

private_ext_file = os.getenv("HOME") .. "/src/github/private-config/etc/wrench-ext.lua"

if file_exists(private_ext_file) then
   dofile_res, private_config = pcall(dofile, private_ext_file)
   if not dofile_res then
      private_config = {}
   end
end

is_useful_notification = function(key, pkg, title, text)
   if private_config.is_useful_notification then
      return private_config.is_useful_notification(key, pkg, title, text)
   end
   return 1;
end

should_use_internal_pop = function()
   if private_config.should_use_internal_pop then
      return private_config.should_use_internal_pop()
   end
   return 1;
end

M.is_useful_notification = is_useful_notification
M.should_use_internal_pop = should_use_internal_pop

return M
