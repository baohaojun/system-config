#!/usr/bin/lua

-- 通过微博、微信朋友圈传播小扳手（不一定成功）
local function wrench_spread_it()
   check_phone()
   -- http://weibo.com/1611427581/Bviui9tzF
   -- http://weibo.com/1611427581/BvnNk2PwH?from=page_1005051611427581_profile&wvr=6&mod=weibotime&type=comment
   -- http://m.weibo.cn/1809968333/3774599487375417

   spread_text = "#小扳手真好用# 💑💓💕💖💗💘💙💚💛💜💝💞💟😍😻♥❤"
   wrench_share_to_weixin(spread_text)
   if 1 then
      return
   end
   adb_am("am start sinaweibo://userinfo?uid=1611427581")
   wait_top_activity_match("com.sina.weibo/com.sina.weibo.page.")
   log_to_ui("top_window is %s", adb_top_window())

   for i = 1, 10 do
      adb_event("adb-tap 584 1087 sleep .5")
      if adb_top_window() == "com.sina.weibo/com.sina.weibo.feed.DetailWeiboActivity" then
         break
      end
      log("top_window is %s at %d", adb_top_window(), i)
   end
   adb_event("adb-tap 911 1863")
   sleep(.2)
   adb_event("sleep .2 adb-tap 527 1911")
   wait_input_target("com.sina.weibo/com.sina.weibo.composerinde.CommentComposerActivity")
   adb_event("adb-tap 99 932")
   wrench_post(spread_text)
   adb_event("sleep .5 adb-key back sleep .5")
end

wrench_spread_it()
