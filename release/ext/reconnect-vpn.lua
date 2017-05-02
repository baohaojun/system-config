-- 重置手机 VPN 连接模式（演示、竖屏高清、横屏高清）

local mode = select_args{"你希望采用什么模式连接手机VPN？", "演示模式", "竖屏高清", "横屏高清"}
log("mode is %s", mode)

local mode_file = io.open("vpn-mode.lua", "w")
mode_file:write("return [[" .. mode .. "]]")
mode_file:close()
kill_android_vnc()
