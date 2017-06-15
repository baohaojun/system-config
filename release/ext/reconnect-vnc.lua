-- 重置手机 VNC 连接模式（演示、竖屏高清、横屏高清）

local mode = select_args{"你希望采用什么模式连接手机VNC？", "演示模式", "竖屏高清", "横屏高清"}
log("mode is %s", mode)
local configDir = os.getenv("WRENCH_CONFIG_DIR")

local mode_file = io.open(configDir .. package.config:sub(1, 1) .. "vnc-mode.lua", "w")
mode_file:write("return [[" .. mode .. "]]")
mode_file:close()
kill_android_vnc()
