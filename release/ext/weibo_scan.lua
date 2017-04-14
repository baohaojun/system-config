-- 打开微博扫码功能
for i = 1, 20 do
   adb_start_activity([[com.sina.weibo/com.sina.weibo.MainTabActivity]])
   wait_top_activity_n(5, [[com.sina.weibo/com.sina.weibo.MainTabActivity]])
   adb_event"adb-tap 1028 145" -- 
   wait_top_activity_n(5, [[com.sina.weibo/com.sina.weibo.feed.HomeListActivity]])
   adb_event"adb-tap 937 382" -- click qr code
   if wait_top_activity_n_ok(5, [[com.sina.weibo/com.sina.weibo.qrcode.CaptureActivity]]) then
      break
   else
      adb_event"key back sleep .1 key back sleep .1 key back"
   end
end
