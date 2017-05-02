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

if os.getenv("HOME") then
   private_ext_file = os.getenv("HOME") .. "/src/github/private-config/etc/wrench-ext.lua"
else
   private_ext_file = ""
end

if file_exists(private_ext_file) then
   dofile_res, private_config = pcall(dofile, private_ext_file)
   if not dofile_res then
      private_config = {}
   end
end

local ignored_pkgs = {
   "com.google.android.apps.maps",
   "android",
   "com.bhj.setclip",
   "com.android.systemui",
   "com.github.shadowsocks",
}

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

   for _, p in ipairs(ignored_pkgs) do
      if pkg == p then
         return 0
      end
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

M.configs = {
   ["phone-width"] = 1080,
   ["phone-height"] = 1920,
   ["wheel-scale"] = 1,
   ["vnc-server-command"] = "/data/data/com.android.shell/androidvncserver",
}

local dofile_res = nil
dofile_res, vpn_mode = pcall(dofile, "vpn-mode.lua")
if not dofile_res then
   vpn_mode = "横屏高清"
end

if vpn_mode ~= "演示模式" then
   M.configs["vnc-server-command"] = "/data/data/com.android.shell/androidvncserver -s 100"
   M.configs["allow-vnc-resize"] = "true"
   if vpn_mode == "横屏高清" then
      M.configs["phone-width"] = 1920
      M.configs["phone-height"] = 1080
   elseif vpn_mode == "竖屏高清" then
      M.configs["phone-width"] = 1080
      M.configs["phone-height"] = 1920
   end
end

M.configs['vpn_mode'] = vpn_mode

M.getConfig = function(config)
   -- if true then return "" end
   if private_config.configs and private_config.configs[config] then
      return private_config.configs[config]
   end
   return M.configs[config] or ""
end

return M
