#!/usr/bin/env Wrench
-- 打开微博扫码功能
local qr_button = "adb-tap 871 257"
if real_height == 2160 then
   qr_button = "adb-tap 863 202"
end

for i = 1, 20 do
   start_app("com.sina.weibo")
   wait_top_activity_n(5, [[com.sina.weibo/com.sina.weibo.MainTabActivity]])
   adb_event"adb-tap 1028 145" -- 
   wait_top_activity_n(5, [[com.sina.weibo/com.sina.weibo.feed.HomeActivity]])

   adb_event("sleep .5 " .. qr_button) -- click qr code
   if wait_top_activity_n_ok(5, [[com.sina.weibo/com.sina.weibo.qrcode.CaptureActivity]]) then
      break
   else
      adb_event"key back sleep .1 key back sleep .1 key back"
   end
end
