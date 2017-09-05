#!/usr/bin/env Wrench.sh
-- 在微博上关注小扳手作者包昊军（不一定成功）

local function wrench_follow_me()
   check_phone()
   -- http://weibo.com/u/1611427581 (baohaojun)
   -- http://weibo.com/u/1809968333 (beagrep)
   adb_am("am start sinaweibo://userinfo?uid=1611427581")
   wait_top_activity_match("com.sina.weibo/com.sina.weibo.page.")
   adb_event("sleep 1 adb-tap 187 1884")
   log("Follow baohaojun on weibo")
   for n = 1, 10 do
      sleep(.2)
      if adb_top_window() == "com.sina.weibo" then
         sleep(.5)
         adb_event("key back")
         break
      end
   end
end

wrench_follow_me()
