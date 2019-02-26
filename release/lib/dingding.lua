M.dd_pkg = "com.alibaba.android.rimet/"
M.dd_activity = "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.home.activity.HomeActivity"

M.open_dd = function()
   local top_window, i
   for i = 1, 5 do
      if not top_activity_match(M.dd_pkg) then
         adb_start_app(M.dd_activity)
      end
      sleep(.3)

      local found_grey = find_scene("dd-xiaoxi-weixuanzhong")
      local found_duihua = find_scene("dd-xiaoxi-xuanzhong")
      if not found_grey and not found_duihua then
         adb_event("key back")
         sleep(.2)
      elseif found_grey then
         jump_from_to("dd-xiaoxi-weixuanzhong", "dd-xiaoxi-xuanzhong")
         found_duihua = 1
      end

      if found_duihua then
         jump_from_to("dd-xiaoxi-dd-biaoti", "dd-xiaoxi-sousuo", {click_times = 2})
         return
      end
   end
end

M.wrench_find_dingding_contact = function(friend_name)
   putclip_nowait(friend_name)
   open_dd()
   jump_from_to("dd-xiaoxi-sousuo", "dd-xiaoxi-sousuozhong")
   adb_event"key scroll_lock key enter sleep .8"
   if yes_or_no_p("第一个人就是你要找的人吗？") then
      click_scene("dd-NiKeNengXiangZhao", {y = 200, x = 100})
   end
end

M.dingding_open_homepage = function()
   local dingding_splash = "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.SplashActivity"
   local dingding_home = "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.home.activity.HomeActivity"
   adb_am("am start -n " .. dingding_splash)
   wait_top_activity_match("com.alibaba.android.rimet/")
   for i = 1, 20 do
      sleep(.1)
      local window = adb_top_window()
      if window then
         log("dd: window is %s at %d", window, i)
      end
      if window and window ~= dingding_splash and window ~= dingding_home then
         if window == "com.alibaba.android.rimet/com.alibaba.android.user.login.SignUpWithPwdActivity" then
            log("You need to sign in dingding")
            break
         end
         log("dd: window is %s at %d", window, i)
         adb_event"key back sleep .1"
         sleep(.1)
         adb_am("am start -n " .. dingding_splash)
         wait_top_activity_match("com.alibaba.android.rimet/")
      elseif window == dingding_splash then
         adb_event"adb-tap 863 222"
      elseif window == dingding_home then
         break
      end

   end
end

M.picture_to_dingding_chat = function(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   adb_shell(
      [[
            for x in /sdcard/Wrench-DingDing/*; do
               if test -e "$x"; then
                  rm -rf "$x";
                  am startservice --user 0 -n com.bhj.setclip/.PutClipService --es picture "$x";
               fi;
            done;
            mkdir /sdcard/Wrench-DingDing/;
            cp /sdcard/DCIM/Camera/000-wrench-* /sdcard/Wrench-DingDing/;
            for x in /sdcard/Wrench-DingDing/000-wrench-*; do
                if test -e "$x"; then
                    am startservice --user 0 -n com.bhj.setclip/.PutClipService --es picture "$x";
                fi;
            done

   ]])

   if not click_to_album_wx_chat_style("adb-tap 173 1359", "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.im.activities.AlbumActivity") then
      return
   end

   local pic_share_buttons = {
      "adb-tap 587 314", "adb-tap 1007 348", "adb-tap 284 712",
      "adb-tap 649 666", "adb-tap 969 676", "adb-tap 292 1053",
      "adb-tap 598 1029", "adb-tap 980 1027", "adb-tap 260 1407"
   }
   for i = 1, #pics do
      local target = pics[i]
      local button = pic_share_buttons[i]
      if i == 1 then
         for n = 1, 10 do
            local window = adb_top_window()
            if window == "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.im.activities.AlbumActivity" then
               adb_event"sleep .2"
               adb_event(button .. " sleep .2 adb-tap 907 1860")
            elseif window == "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.im.activities.AlbumPreviewActivity" then
               adb_event"sleep .5 adb-tap 859 1850 sleep .2 adb-tap 85 169 sleep .3"
               break
            end
         end
      end
      adb_event("sleep .2 " .. button .. " sleep .1")
   end
   adb_event"adb-tap 930 1876 sleep .2 adb-tap 96 1860 sleep .2"
   if yes_or_no_p"Confirm to send these pictures to dingding" then
      adb_event"adb-tap 888 128 sleep .2"
   end
   if wait_top_activity_n(2, old_top_window) ~= old_top_window then
      log"Can't get old dd chat window"
   end
end

M.get_out_of_windows = function(windows, ...)
   if type(windows) ~= "table" then
      windows = {windows, ...}
   end

   for i = 1, 20 do
      if not member(adb_top_window(), windows) then
         return
      end
      sleep(.2)
   end

   prompt_user("试了很多次，还是在 %s 里，小扳手的自动化脚本可能有问题，请检查一下", adb_top_window())
   error("Failed to get out of %s", join(", ", windows))
end

M.need_confirm = function(fmt, ...)
   if yes_or_no_p(fmt, ...) then
      return true
   else
      return false
   end
end

M.wrench_picture = function (...)
   local pics = upload_pics(...)
   local window = adb_top_window()
   if window == W.weixinLauncherActivity and yes_or_no_p("发送给当前微信聊天窗口") then
      return picture_to_weixin_chat(pics)
   elseif window == "com.tencent.mm/com.tencent.mm.plugin.scanner.ui.BaseScanUI" then
      return M.picture_to_weixin_scan(pics)
   elseif window:match("^com.tencent.mm/com.tencent.mm.ui.chatting") then
      if yes_or_no_p("发送给当前微信聊天窗口") then
         return picture_to_weixin_chat(pics)
      end
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity" then
      if yes_or_no_p("发送给当前 QQ 聊天窗口") then
         return picture_to_qq_chat(pics)
      end
   elseif window == "com.alibaba.android.rimet/com.alibaba.android.dingtalkim.activities.ChatMsgActivity" then
      if yes_or_no_p("发送给当前钉钉聊天窗口") then
         return picture_to_dingding_chat(pics)
      end
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.SplashActivity" then
      if yes_or_no_p("发送给当前 QQ 聊天窗口") then
         return picture_to_qq_chat(pics)
      end
   elseif window == "com.sina.weibo/com.sina.weibo.weiyou.DMSingleChatActivity" then
      if yes_or_no_p("发送给当前微博私信聊天窗口") then
         return picture_to_weibo_chat(pics)
      end
   elseif window == W.weiboCommentActivity or window == W.weiboForwardActivity then
      if yes_or_no_p("发送给当前微博回复窗口") then
         return picture_to_weibo_comment(pics)
      end
   end
   if yes_or_no_p("不知道此窗口（" .. window .. "）下如何分享图片，如需继续上传，请点击确认后选择把图片发给谁，否则请点取消") then
      local how_to_send = M.select_args_with_history("how-to-send-pic",
         "请输入微信、QQ 联系人搜索方式或选择如何分享",
         "再试一下发送给新的当前窗口",
         "分享到微信朋友圈",
         "分享到微博",
         "输入 XXX@@wx 并回车发给微信联系人 XXX",
         "输入 XXX@@qq 并回车发给 QQ 联系人 XXX",
         "输入 XXX@YYY@@qq 并回车发给 QQ 群 YYY 里的联系人 XXX"
         )

      if how_to_send == "分享到微信朋友圈" then
         picture_to_weixin_share()
         return
      elseif how_to_send == "分享到微博" then
         picture_to_weibo_share()
         return
      elseif how_to_send == "再试一下发送给新的当前窗口" then
         return wrench_picture()
      else
         wrench_call(how_to_send)
         sleep(1)
         return wrench_picture()
      end
   end
   return #pics .. " pictures sent"
end


M.press_dial_key = function()
   if not where_is_dial_key then
      where_is_dial_key = phone_info_map[android_serial .. ":拨号键位置"]
      if not where_is_dial_key then
         where_is_dial_key = select_args{"Where is the dial button？", "Middle", "First from left", "Second from left"}
         phone_info_map[android_serial .. ":拨号键位置"] = where_is_dial_key
         save_phone_info()
      end
   end
   debugging("where_is_dial_key is %s", where_is_dial_key)
   if where_is_dial_key == "Middle" then
      adb_event("adb-tap 554 1668")
   elseif where_is_dial_key == "First from left" then
      adb_event"adb-tap 156 1633"
   elseif where_is_dial_key == "Second from left" then
      adb_event"adb-tap 420 1634"
   else
      adb_event("adb-tap 554 1668")
      log("Error: unknown where_is_dial_key: %s, must be one of Middle, First from left, Second from left.\n\nPlease update %s", where_is_dial_key, M.configDirFile("phone_info.lua"))
      where_is_dial_key = nil
   end
end

M.wrench_call = function(number)
   if number:match("@@") then
      number = string_strip(number)
      local names = split("@@", number, true)
      local who, where = names[1] or "", names[2] or ""
      if where == "qq" then
         wrench_find_qq_contact(who)
      elseif where == "wx" then
         wrench_find_weixin_contact(who)
      elseif where == "zd" then
         find_zidanduanxin_friend(who)
      elseif where == "dd" then
         wrench_find_dingding_contact(who)
      elseif where == "coffee" then
         get_coffee(who)
      elseif where == "mail" then
         search_mail(who)
      elseif where == "wb" or where == "weibo" then
         find_weibo_friend(who)
      elseif where == "sms" then
         search_sms(who)
      elseif where == "ext" then
         wrench_run("ext" .. package.config:sub(1, 1) .. who .. ".lua")
      elseif where == "eval" then
         local func = loadstring(who)
         wrench_eval(func, who)
      else
         prompt_user("Don't know how to do it: " .. where)
      end
      return
   end

   adb_am("am start -a android.intent.action.DIAL tel:" .. number)
   adb_event("sleep .5")
   if not yes_or_no_p("Phone number correct and dial it?") then
       return
   end
   press_dial_key()
   adb_event("sleep 1")
   if adb_top_window() == "com.android.contacts/com.android.contacts.activities.DialtactsActivity" then
      press_dial_key()
   end
end

