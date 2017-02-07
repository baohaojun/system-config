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
   if private_config.is_useful_notification and
   private_config.is_useful_notification(key, pkg, title, text) == 0 then
      return 0
   end

   if title == "no title" or text == "no text" then
      return 0
   end

   if pkg == "com.github.shadowsocks" and title == "Default" then
      return 0
   end

   if pkg == "android" or pkg == "com.bhj.setclip" or pkg == "com.android.systemui" then
      return 0
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
