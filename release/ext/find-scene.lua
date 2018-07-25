#!/usr/bin/env Wrench
-- 在手机屏幕上搜索某个场景

-- 所谓的场景，是一个预先准备的截图，存放在 Wrench 代码的
--  release/scenes 目录下。

-- 系统里必须使用 nodejs 的 look-same 模块、以及拥有我的
--  system-config 项目的 gcode/playground/opencv 目录里编译出来的
--  match_image 程序，所以基本上必须在我的 system-config 环境下才能使
-- 用此功能。


local scene_name
if M.ext_args then
   scene_name = M.ext_args[1]
else
   mode = select_args{"你希望找到那个场景截图？", "blank-1", "blanch-2"}
end
