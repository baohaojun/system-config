#!/usr/bin/lua

-- 清除当前窗口的发送按钮设置
local top_window = adb_top_window()
log("change window %s from %s to empty", top_window, window_post_button_map[top_window])
window_post_button_map[top_window] = nil
save_window_types()
