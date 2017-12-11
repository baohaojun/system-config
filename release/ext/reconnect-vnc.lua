#!/usr/bin/env Wrench
-- 重置手机 VNC 连接模式（演示、竖屏高清、横屏高清）

-- [[file:~/system-config/knuth-mode/reconnect-vnc.lua.org::*%E6%9C%80%E7%BB%88%E7%9A%84%E7%89%88%E6%9C%AC%EF%BC%9A][the-ultimate-script]]

local mode
if M.ext_args then
   mode = M.ext_args[1]
else
   mode = select_args{"你希望采用什么模式连接手机VNC？", "演示模式", "高清模式"}
end

log("mode is %s", mode)
M.update_screen_size()
local configDir = os.getenv("WRENCH_CONFIG_DIR")
local mode_file = io.open(configDir .. package.config:sub(1, 1) .. "vnc-mode.lua", "w")
local vnc_mode_str = [[
local M = {}
M.mode = "%s"
M.width = %d
M.height = %d
return M
]]

mode_file:write(vnc_mode_str:format(mode, M.real_width, M.real_height))
mode_file:close()
kill_android_vnc()
gotUiTask{"start-vnc"}
-- Local Variables: --
-- eval: (read-only-mode 1) --
-- End: --

-- the-ultimate-script ends here
