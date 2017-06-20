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

local dofile_res = nil
local configDir = os.getenv("WRENCH_CONFIG_DIR")

private_ext_file = configDir .. package.config:sub(1, 1) .. "my-wrench-ext.lua"

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

function is_dup(s1, s2)
   if s2:len() > s1:len() then
      s1, s2 = s2, s1
   end

   if s1:sub(1, s2:len()) == s2 or s1:sub(-s2:len()) == s2 then
      return true
   end

   return false
end

M.rewrite_notification_text = function(key, pkg, title, text, ticker)
   if pkg == "com.tencent.mobileqq" then
      return ticker
   end

   if pkg == "com.android.mms" and title:match("通の新しいメッセージ") then
      return ticker
   end
   if ticker ~= "" and not is_dup(ticker, text) then
      return ("%s ticker(%s)"):format(text, ticker)
   end
   return text
end

is_useful_notification = function(key, pkg, title, text, ticker)
   if private_config.is_useful_notification and
   private_config.is_useful_notification(key, pkg, title, text) == 0 then
      return 0
   end

   if title == "" or (text == "" and ticker == "") then
      return 0
   end

   if text == "" and title == ticker and title:match("件の新しいメール") then
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

dofile_res, vnc_mode = pcall(dofile, configDir .. package.config:sub(1, 1) .. "vnc-mode.lua")
if not dofile_res then
   vnc_mode = "演示模式"
end

if vnc_mode ~= "演示模式" then
   M.configs["vnc-server-command"] = "/data/data/com.android.shell/androidvncserver -s 100"
   M.configs["allow-vnc-resize"] = "true"
   if vnc_mode == "横屏高清" then
      M.configs["phone-width"] = 1920
      M.configs["phone-height"] = 1080
   elseif vnc_mode == "竖屏高清" then
      M.configs["phone-width"] = 1080
      M.configs["phone-height"] = 1920
   end
end

M.configs['vnc_mode'] = vnc_mode

M.getConfig = function(config)
   -- if true then return "" end
   if private_config.configs and private_config.configs[config] then
      return private_config.configs[config]
   end
   return M.configs[config] or ""
end

return M
