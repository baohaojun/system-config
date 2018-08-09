M.click_scene = function (scene)
   if not scenes_map[scene] then
      find_scene(scene)
   end

   if scenes_map[scene] then
      local xy = scenes_map[scene]
      xy = split(" ", xy)
      adb_tap_XY(xy[1], xy[2])
   end
end

M.qq_find_friend = function(friend_name)
   putclip_nowait(friend_name)

   log("qq find friend: %s", friend_name)
   for i = 1, 5 do
      qq_open_search()
      local top_window = wait_input_target_n(5, W.qqChatActivity2, W.qqGroupSearch)
      adb_event("key comma sleep .1")

      click_scene('qq-close-search')
      adb_tap_1080x2160(857, 91, 804, 167) -- click the clear search button "x"
      adb_event"sleep .1 key scroll_lock sleep .5" -- clear the search first by input "x " and click ⓧ.
      if top_window and top_window:match(W.qqGroupSearch) then
         for click_search_res = 1, 3 do
            log"Found W.qqGroupSearch"
            adb_tap_1080x2160(415, 331, 544, 558)
            sleep(.2)

            if adb_top_window() ~= W.qqGroupSearch then
               log("Found the qq friend %s", friend_name)
               return
            elseif click_search_res == 3 then
               yes_or_no_p("小扳手好像没有找到你想找的 QQ 好友（%s），请自己手动点一下...", friend_name)
               return
            end
         end
      else
         if i > 1 and not yes_or_no_p("没有找到 QQ 好友 %s，再试一遍？", friend_name) then
            return
         end
         log("Got stuck in W.qqChatActivity2, ime stuck?: %s at %d", top_window, i)
         if i == 5 then
            error("Can't get to W.qqGroupSearch in the end")
         end
         adb_event"key back adb-tap 303 291"
      end
   end
end

M.qq_find_group_friend = function(friend_name)
   putclip_nowait(friend_name)
   log("qq find group friend: %s", friend_name)
   local window = adb_top_window()
   if window ~= W.qqChatActivity and window ~= W.qqChatActivity2 then
      log("qq window is not chat: %s", window)
      return
   end
   tap_top_right()
   local chatSetting = "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatSettingForTroop"
   window = wait_top_activity(chatSetting)
   if window ~= chatSetting then
      log("did not get chatSetting: %s", window)
      return
   end

   adb_event("sleep 1 ")
   adb_tap_1080x2160(404, 639, 426, 1540) -- 点击进入群成员列表
   local troopList = "com.tencent.mobileqq/com.tencent.mobileqq.activity.TroopMemberListActivity"
   window = wait_top_activity(troopList)
   if window ~= troopList then
      prompt_user("没有点到 QQ 群成员列表页面，可能是小扳手的座标出了点问题，请用录屏功能调整一下座标")
      return
   end

   local troop_list_search = "adb-tap 663 252"
   if real_height >= 2160 then
      troop_list_search = "adb-tap 373 212"
   end

   for i = 1, 40 do
         adb_event("sleep .1 " .. troop_list_search)
         if wait_input_target_n(3, troopList) ~= "" then
            break
         else
            log("wait for troopList input: %d", i)
         end
   end

   the_1st_member_click = "adb-tap 326 229"
   if real_height >= 2160 then
      the_1st_member_click = "adb-tap 500 204"
   end

   adb_event("key scroll_lock key space key DEL sleep .5 " .. the_1st_member_click)
   local troopMember = "com.tencent.mobileqq/com.tencent.mobileqq.activity.TroopMemberCardActivity"
   for i = 1, 10 do
      window = wait_top_activity_n(2, troopMember)
      if window == troopMember then
         break
      else
         log("Did not get into troopMember, try " .. i)
         adb_event("key space sleep .5 key DEL sleep .5 " .. the_1st_member_click)
         if i == 5 then
            log("Giving up...")
            return
         end
      end
   end
   local send_msg_button = "adb-tap 857 1800"
   if real_height >= 2160 then
      send_msg_button = "adb-tap 894 1872"
   end
   adb_event("sleep " .. .5 * i .. send_msg_button)
end

M.qq_open_search = function ()
   local max_qq_try = 5

   search_bar_y = 251

   for qq_try = 1, max_qq_try do
      adb_start_activity(W.qqChatActivity2)
      adb_event"adb-tap 186 1809 sleep .1 adb-tap 186 1809" -- click first tab button, but maybe also click edit
      if qq_try > 1 then
         adb_event"adb-swipe-100 433 701 433 751 sleep .1"
      end
      adb_event(" sleep .3 adb-tap 539 " .. search_bar_y .. " adb-tap 539 " .. search_bar_y) -- double click the search bar
      local ime_active, height, ime_connected

      if wait_input_target_n_ok(4 + math.floor(qq_try / 2), W.qqGroupSearch) then
         return
      elseif qq_try == max_qq_try then
         prompt_user("在等 %s 的输入，但最后找到的是 %s，请检查 wrench.lua 脚本", W.qqGroupSearch, top_window)
         error("Wrench.lua 脚本可能有问题了，这个自动化操作需要更新脚本")
      end

      if ime_active then
         log("got ime active in %s with ime height: %d, try: %d", top_window, height, qq_try)
         if height ~= 0 then
            adb_event("key back sleep .1")
         end
         adb_event("key back sleep .1")
      end

      debugging("qq @ %s when try = %d", top_window, qq_try)
      for findHomePageTry = 1, 10 do
         if not (qq_try < 3 and top_window == W.qqSplashActivity and ime_active) then
            adb_event"key back sleep .2"
         end
         top_window = adb_top_window()
         if top_window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.QQSettingSettingActivity" then
            log("进入了 QQ 设置页面")
            adb_event("key back sleep .2")
         end
         if not top_window:match("^com.tencent.mobileqq/") then
            log("got out of qq (%s) when try = %d", top_window, qq_try)
            break
         end

         if top_window == W.qqSplashActivity then
            break
         end
      end
   end
   if yes_or_no_p("没法打开 QQ 主页窗口，放弃？（会重启 Lua 后台）") then
      error("用户放弃操作")
   end
end

M.clickForQqMoney = function(title, text)
   log("Click for QQ money")
   for i = 1, 50 do
      top_window = adb_top_window()
      if top_window and not top_window:match("^com.tencent.mobileqq/") then
         sleep(.1)
      else
         if title == "QQ" then
            adb_event"adb-tap 641 405 sleep .5" -- assume it's the first chat:D
         end
         break
      end
   end
   -- adb-tap 306 1398 adb-tap 179 1587 adb-tap 951 1762
   for i = 1, 50 do
      adb_event"adb-tap 351 1398 adb-tap 179 1587 adb-tap 951 1762"
      adb_event "sleep .1"
      if adb_top_window() == "com.tencent.mobileqq/cooperation.qwallet.plugin.QWalletPluginProxyActivity" then
         sayThankYouForLuckyMoney()
         break
      elseif i == 50 then
         log("Can't get to QWalletPluginProxyActivity after %d times", i)
      end
   end
   adb_event"sleep .3 adb-key home"
end

M.picture_to_qq_chat = function (pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = close_ime()
   local chatWindow
   local image_button = ('206 %d'):format(1920 - ime_height - 50)
   local post_button = ('159 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local target = pics[i]
      if i == 1 then
         for n = 1,10 do
            local window = adb_top_window()
            if window == W.qqChatActivity or window == W.qqChatActivity2 then
               chatWindow = window
               adb_event(image_button .. " sleep 2 adb-tap 70 1873")
               local top_window = wait_top_activity(W.qqAlbumList, W.qqCameraFlow, W.qqNewCameraFlow)

               if top_window == W.qqCameraFlow or top_window == W.qqNewCameraFlow then
                  log("get W.qqCameraFlow")
                  get_out_of_windows(W.qqCameraFlow, W.qqNewCameraFlow)
                  image_button = ('380 %d'):format(1920 - ime_height - 50)
               elseif top_window ~= W.qqAlbumList then
                  log("Wait for W.qqAlbumList failed")
                  prompt_user("小扳手自动化脚本没有点到 QQ 像册界面，无法继续执行发图片功能")
                  return
               else -- qqAlbumList
                  sleep(.5)
                  adb_tap_1080x2160(692, 243)
                  log("注意：QQ 上传图片的功能可能没法用了，因为你要上传的图片在相册列表的第几行、第几张现在是不固定的")
                  log("小扳手帮你点了最上面的相册里的前 %d 张，但有可能是错误的。有时候你可能要自己把一直占着前几个位置的照片删掉。。。", #pics)
               end
            elseif window == W.qqPhoteList then
               adb_event("adb-tap 171 427")
            elseif window == W.qqPhotoPreview then
               adb_event("adb-key back")
               if wait_top_activity(W.qqPhoteList) == W.qqPhoteList then
                  break
               elseif adb_top_window() == W.qqPhotoPreview then
                   adb_event("adb-key back")
               end
            end
            sleep(.1)
         end
      end
      local pic_share_buttons = {
         "adb-tap 271 285", "adb-tap 621 267", "adb-tap 968 291",
         "adb-tap 285 664", "adb-tap 625 644", "adb-tap 978 653",
         "adb-tap 284 989", "adb-tap 621 1024", "adb-tap 988 1019"
      }

      if real_height >= 2160 then
         pic_share_buttons = {
            "adb-tap 317 218", "adb-tap 656 190", "adb-tap 1009 204",
            "adb-tap 301 516", "adb-tap 681 537", "adb-tap 1015 536",
            "adb-tap 304 889", "adb-tap 654 874", "adb-tap 975 858",
         }
      end

      local i_button = pic_share_buttons[i]
      sleep(.1)
      adb_event(i_button)
   end

   local original_pic_button = 'adb-tap 477 1835'
   if real_height >= 2160 then
      original_pic_button = 'adb-tap 522 1859'
   end

   adb_event("sleep .1 " .. original_pic_button)
   if need_confirm("请确认是不是要发送这些图片？") then
      adb_event"adb-tap 898 1840"
      wait_top_activity(chatWindow)
      adb_event("key back")
   end
end

M.wrench_share_to_qq = function(text)
   share_text_to_app("com.qzone", "com.qzonex.module.operation.ui.QZonePublishMoodActivity",  text)
   wait_input_target(W.qqShareActivity)
   if yes_or_no_p("分享到 QQ 空间？") then
      wrench_send_action()
   end
end

M.picture_to_qq_share = function(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
   log("share to qq")
   share_pics_to_app("com.qzone", "com.qzonex.module.operation.ui.QZonePublishMoodActivity", pics)
   wait_top_activity(W.qqShareActivity)
   adb_event("adb-tap 228 401")
   wait_input_target(W.qqShareActivity)
end

M.wrench_find_qq_contact = function(number)
   local contact_type
   if number == "" then
      number = string_strip(M.select_args_with_history("qq-contact", "请输入 QQ_FRIEND_NAME 或 QQ_USER@QQ_GROUP", "", " "))
      if number == "" then
         prompt_user("没有输入你想查找的 QQ 联系人，无法查找")
         return
      end
   end

   if (number:match("@qq.com")) then
      number = number:gsub("@qq.com", "")
      contact_type = 0
   elseif (number:match("@QQ.com")) then
      number = number:gsub("@QQ.com", "")
      contact_type = 1
   elseif (number:match("@")) then
      local names = split("@", number)
      local who, where = names[1] or "", names[2] or ""
      qq_find_friend(where)
      sleep(.5)
      qq_find_group_friend(who)
      return
   else
      qq_find_friend(number)
      return
   end
   if using_adb_root then
      adb_am(("am start --user 0 -n com.tencent.mobileqq/.activity.ChatActivity --es uin %d --ei uintype %d"):format(number, contact_type))
   else
      wrench_find_qq_contact(number)
   end
end

M.emoji_for_qq = function(text)
   return emoji_rewrite(text, qq_emojis_remap)
end

dofile_res, M.qq_emojis_remap = pcall(dofile, "qq-emojis-remap.lua")
if not dofile_res then
   M.qq_emojis_remap = {}
end

