#!/usr/bin/env Wrench.sh
-- 打开Bilibili客户端扫描二维码功能

function open_bili_scan()
   start_app("tv.danmaku.bili")
   wait_top_activity_n_ok(5, [[tv.danmaku.bili/tv.danmaku.bili.MainActivity]])
   adb_event"adb-tap 1002 317" -- click discovery
   wait_top_activity_n_ok(5, [[tv.danmaku.bili/tv.danmaku.bili.MainActivity]])
   adb_event"sleep .5 adb-tap 969 457" -- click scan
   if wait_top_activity_n_ok(5, [[tv.danmaku.bili-19]]) then
      adb_event"adb-tap 752 1264" -- allow camera
   end

   if not wait_top_activity_n_ok(5, [[tv.danmaku.bili/tv.danmaku.bili.ui.qrcode.QRcodeCaptureActivity]]) then
      if yes_or_no_p("还没有打开Bilibili，再试一次？") then
         open_bili_scan()
      end
   end
end

open_bili_scan()
