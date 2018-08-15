W.weixinPackage = "com.tencent.mm/"
W.weixinAlbumPreviewActivity = "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.AlbumPreviewUI"
W.weixinChatActivity = "com.tencent.mm/com.tencent.mm.ui.chatting.ChattingUI"
W.weixinLauncherActivity = "com.tencent.mm/com.tencent.mm.ui.LauncherUI"
W.weixinSearchActivity = "com.tencent.mm/com.tencent.mm.plugin.fts.ui.FTSMainUI"
W.weixinSnsUploadActivity = "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI"
W.weixinImagePreviewActivity = "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.ImagePreviewUI"

M.weixin_open_homepage = function()
   weixin_open_search()
   exit_ime()
end

M.weixin_open_search = function(depth)
   if not depth then depth = 0 end
   for i = 1, 10 do
      adb_am("am start -n " .. W.weixinLauncherActivity)
      wait_top_activity_match("com.tencent.mm/")

      log("touch the search button " .. i)
      local click_weixin_search_button = "adb-tap 801 132"

      local top_window
      for i_search = 1, 4 do
         adb_event(click_weixin_search_button)
         sleep(.1)
         top_window = adb_top_window()
         if top_window == W.weixinSearchActivity then
            if wait_input_target_n_ok(1, W.weixinSearchActivity) then
               adb_event("key space sleep .1")
               click_scene('wx-close-search', 10)
               adb_event"sleep .1"
               if wait_input_target_n_ok(5, W.weixinSearchActivity) then
                  return
               end
            end

            log("exit from search by key back: %d %s ", i_search, top_window)
            if not wait_input_target_n_ok(5, W.weixinSearchActivity) then
               if i_search == 4 then
                  if yes_or_no_p("本次（第 %d 次）打开微信首页的自动操作没有点出微信搜索框，再试一次？", depth + 1)
                  then
                     return weixin_open_search(depth + 1)
                  else
                     error("用户取消了操作")
                  end
               end
            end
         elseif top_window ~= '' and top_window ~= W.weixinLauncherActivity then
            log("exit the current '%s' by back key %d", top_window, i)
            for launcher_or_search = 1, 10 do
               if top_window ~= '' then
                  adb_event("key back")
               end
               adb_event"sleep .1"
               top_window = adb_top_window()
               log("got %s", top_window)
               if top_window == W.weixinLauncherActivity or
                  (top_window ~= '' and not top_window:match(W.weixinPackage))
               then
                  break
               elseif top_window == W.weixinSearchActivity then
                  if (depth < 5) then
                     return weixin_open_search(depth + 1)
                  end
               end
            end
            break
         elseif top_window == W.weixinLauncherActivity then
            log("We are in %s when %d@%d", top_window, i_search, i)
            break
         end
      end
      log("exit the current '%s' by touching back botton %d", top_window, i)
      adb_event"88 125 sleep .1 88 125 sleep .1"
      sleep(.1)
   end
end

M.weixin_find_friend = function(friend_name, depth)
   if not depth then depth = 0 end
   if friend_name == "" then
      friend_name = string_strip(M.select_args_with_history("weixin-friends", "请输入想找的微信联系人名字", "", " ")):gsub("@@wx$", "")
      if friend_name == "" then
         prompt_user("没有输入你想查找的微信联系人，无法查找")
         return
      end
   end

   local need_confirm
   if friend_name:match("%?$") then
      friend_name = friend_name:gsub("%?$", "")
      need_confirm = true
   end

   putclip_nowait(friend_name)

   weixin_open_search()
   adb_event"sleep .2 key scroll_lock sleep .5"
   if need_confirm then
      prompt_user("请确认哪个是你要找的联系人")
      return
   end
   for i = 1, 10 do
      adb_event"adb-tap 245 382 sleep .2"
      if adb_top_window() == W.weixinSearchActivity then
         log("still at weixin search at %d", i)
         if i == 10 then
            prompt_user("Can't get out of weixinSearchActivity")
         end
      else

         break
      end
   end
end

M.adb_start_weixin_share = function(text_or_image)
   if using_adb_root then
      if text_or_image == 'text' then
         adb_am("am start -n com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsCommentUI --ei sns_comment_type 1")
      elseif text_or_image == 'image' then
         adb_am("am start -n com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI")
      else
         error("Can only do image or text")
      end
      return
   end

   local click = "adb-tap"
   if text_or_image == 'text' then
      click = "adb-long-press-800"
   elseif text_or_image ~= 'image' then
      error("Can only do image or text")
   end

   weixin_open_homepage()
   adb_event("adb-tap 654 1850 sleep .5 adb-tap 332 358")
   if (wait_top_activity_match("com.tencent.mm/com.tencent.mm.plugin.sns.ui.")):match"com.tencent.mm/com.tencent.mm.plugin.sns.ui." then
      adb_event("sleep 1 " .. click .. " 977 132")
   else
      log("Can't switch to the Friend Zone page.")
   end
   if text_or_image == 'image' then
      adb_event("adb-tap 213 929") -- choose picture
   end
   adb_event("adb-tap 143 264")
   wait_input_target("com.tencent.mm/com.tencent.mm.plugin.sns.ui.")
   log("start weixin share complete")
end

M.picture_to_weixin_scan = function(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   for i = 1, 5 do
      M.tap_top_right()
      sleep(.2)
      M.tap_bottom_center()
      sleep(.5)
      if adb_top_window() == "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.AlbumPreviewUI" then
         break
      elseif i == 5 then
         prompt_user("无法进入扫描手机相册预览界面")
         return
      end
   end

   for i = 1, 5 do
      adb_event"adb-tap 567 425 sleep .5"
      if adb_top_window() ~= "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.AlbumPreviewUI" then
         break
      elseif i == 5 then
         prompt_user("无法退出扫描手机相册预览界面")
         return
      end
   end
end

M.clickForWeixinMoney = function()

   log("Click for weixin money")

   for i = 1, 50 do
      top_window = adb_top_window()
      if top_window and not top_window:match("^com.tencent.mm/") then
         sleep(.1)
      else
         break
      end
   end

   for i = 1, 50 do
      adb_event"adb-tap 406 1660"
      if i > 2 then
         adb_event"adb-tap 327 1395"
      end
      adb_event "sleep .1"
      if isWeixinLuckyMoneyReceiver(adb_top_window()) then
         break
      elseif i == 50 then
         log("Can't get get the money after %d times", i)
      end
   end

   for i = 1, 20 do
      adb_event"adb-tap 535 1197 adb-tap 507 1382 sleep .1"
      top_window = adb_top_window()
      if top_window == "com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.LuckyMoneyDetailUI" then
         break
      elseif top_window and not isWeixinLuckyMoneyReceiver(top_window) then
         log("We have got to %s when click weixin money", top_window)
         break
      end
   end

   sayThankYouForLuckyMoney()
   adb_event"sleep .5 adb-key home"
end

M.wrench_share_to_weixin = function(text)
   debug("share to weixin: %s", text)
   weixinShareActivity = "com.tencent.mm/com.tencent.mm.plugin.sns.ui"
   if text then
      text = text:gsub("\n", "​\n")
      putclip_nowait(emoji_for_weixin(text))
   end
   adb_start_weixin_share('text')
   wrench_post(nil, 'top-right', "分享到微信朋友圈？")
end

M.weixin_text_share = function(window, text)
   if text then
      text = text:gsub("\n", "​\n")
      putclip(text)
   end
   adb_event("adb-key scroll_lock sleep .2")
   if yes_or_no_p("Share to wechat?") then
      adb_event(" adb-tap 1016 131")
   end
end

M.picture_to_weixin_share = function(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
   share_pics_to_app("com.tencent.mm", "com.tencent.mm.ui.tools.ShareToTimeLineUI", pics)

   local wait_chooser = true
   local wait_input = true


   for try = 1, 10 do
      log("pic to wx: %d", try)
      if wait_chooser and M.wait_top_activity_n_ok(1, "smartisanos/smartisanos.app.DoppelgangerChooseActivity") then
         log("got app chooser @%d", try)
         adb_event("adb-tap 341 1739 sleep .1")
         wait_chooser = false
      elseif wait_input and adb_top_window():match"^com.tencent.mm/com.tencent.mm.plugin.sns.ui" then
         adb_event"adb-tap 228 401"
         if not M.wait_input_target_n_ok(10, "^com.tencent.mm/com.tencent.mm.plugin.sns.ui") then
            prompt_user("图片分享到微信朋友圈时出错：输入法没有在朋友圈分享页面激活")
         else
            break
         end
      end
   end
end

M.picture_to_weixin_chat = function(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = close_ime()
   local post_button = ('%d %d'):format(right_button_x, 1920 - 50)
   local chatWindow = adb_top_window()
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]
      if i == 1 then
         for n = 1,10 do
            local window = adb_top_window()
            log("doing first picture for n = %d, window is %s", n, window)
            if window == chatWindow then
               chatWindow = window
               adb_event(post_button .. " sleep .3")
               if adb_top_window():match("PopupWindow") then -- weixin may suggest with popup window a picture just created
                  debug("got popup window?")
                  adb_event("key back sleep .1 adb-tap 123 1853")
                  wait_top_activity(chatWindow)
               end
               adb_event("adb-tap 203 1430") -- click the "album" button

               debug("chatWindow: clicked")
               wait_top_activity(W.weixinAlbumPreviewActivity)
            elseif window == W.weixinAlbumPreviewActivity then
               log("got into album preview for n = %d", n)
               adb_event("adb-tap 521 398") -- to get into image preview
               sleep(.2)
            elseif window == W.weixinImagePreviewActivity then
               log("got into image preview for n = %d", n)
               adb_event("sleep .1 adb-key back")
               if wait_top_activity(W.weixinAlbumPreviewActivity) == W.weixinAlbumPreviewActivity then
                  sleep(.2)
                  log("get back into album preview from image preview")
                  break
               elseif adb_top_window() == W.weixinImagePreviewActivity then
                  log("still in image preview at %d", n)
                  adb_event("key back")
               else
                  log("got into %s at n = %d", adb_top_window(), n)
               end
            end
         end
      end
      local pic_share_buttons = {
         "adb-tap 316 260", "adb-tap 614 281", "adb-tap 1000 260",
         "adb-tap 268 629", "adb-tap 652 645", "adb-tap 1004 632",
         "adb-tap 301 1008", "adb-tap 612 996", "adb-tap 1006 992",
      }

      if real_height >= 2160 and not M.pic_to_weixin_chat_share_buttons then
         pic_share_buttons = {
            ['cols'] = 4, ['rows'] = 3,
            ['first'] = "adb-tap 239 218", -- (1, 1)
            ['last'] = "adb-tap 1050 742", -- (3, 4)
            ['wanted'] = 9, ['start'] = 1,
         }
         M.pic_to_weixin_chat_share_buttons = calc_buttons(pic_share_buttons)
      end
      if M.pic_to_weixin_chat_share_buttons then
         pic_share_buttons = M.pic_to_weixin_chat_share_buttons
      end
      local i_button = pic_share_buttons[i]
      log("click image button %d", i)
      adb_event(i_button)
   end
   for n = 1, 10 do
      if adb_top_window() ~= W.weixinImagePreviewActivity then
         adb_event("sleep .1 adb-tap 944 1894 sleep .1")
         wait_top_activity(W.weixinImagePreviewActivity)
         if n == 10 then
            prompt_user("没有等到 weixinImagePreviewActivity，无法完成此操作")
            return
         end
      else
         break
      end
   end
   adb_event("sleep .2 adb-tap 423 1861 adb-tap 490 1862 sleep .1 ")
   if yes_or_no_p("Confirm to send these images?") then
      tap_top_right()
   end
   window = wait_top_activity(chatWindow)
   if window == W.weixinImagePreviewActivity then
      adb_event("sleep .1 adb-tap 59 1871 sleep .1 adb-tap 927 148")
   elseif window:match("^PopupWindow:") then
      debug("got popup window")
      adb_event("key back sleep .5")
   else
      debug("got unknown window: %s", window)
   end
   adb_event("adb-tap 545 191") -- get rid of popup
   for n = 1, 10 do
      sleep(.1)
      adb_event("adb-tap 553 1796 sleep .1")
      local input_method, ime_height = close_ime()
      if not (adb_top_window()):match("^PopupWindow:") then
         break
      end
   end
end

M.isWeixinLuckyMoneyReceiver = function (window)
   -- com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.En_fba4b94f
   if window == "com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.LuckyMoneyReceiveUI" or (
      window:match("^com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.") and
         window ~= "com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.LuckyMoneyDetailUI"
   ) then
      log("got lucky window %s", window)
      return true
   end
   return false
end

M.sayThankYouForLuckyMoney = function(say_it_directly)
   local thanks = {
      "谢谢老板的红包🤓",
      "老板爱发红包，我就爱这样的老板😍",
      "黑夜给了我一双黑色的眼睛👀，我却用它抢红包💰——谢谢老板🙇🏿",
      "你抢或者不抢，红包就在那里💗",
   }
   for i = 1, 20 do
      if say_it_directly then
         adb_event"sleep 1"
      else
         adb_event"sleep 1 adb-key back sleep 1"
      end
      top_window = adb_top_window()
      log("Got after luck window: %s", top_window)
      if top_window and
         not top_window:match("^com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.") and
      not top_window:match("^com.tencent.mobileqq/cooperation.qwallet.") then
         log("We've got to %s to say thank you", top_window)
         local n = math.random(#thanks)

         local thank_you = thanks[n]

         if say_it_directly then
            thank_you = "http://mp.weixin.qq.com/s/h6jFAnbrtTu6mqhW5eKvQw"
         end

         if WrenchExt.getConfig("should-tell-fortune") == "true" then
            local fortune = M.qx("fortune-zh")
            fortune = fortune:gsub("%[.-m", "")
            thank_you = thank_you .. "\n\n*****\n\n" .. fortune
         end
         local how = 'weixin-chat'
         if top_window:match("^com.tencent.mobileqq/") then
            how = 'qq-chat'
         end
         wrench_post(thank_you, how)
         sleep(1)
         break
      end
   end
end

M.wrench_find_weixin_contact = function(number)
   if not number:match("^[0-9]+$") then
      return weixin_find_friend(number)
   end
   adb_am("am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getcontact 1 --es contact " .. number)
end

M.emoji_for_weixin = function(text)
   return emoji_rewrite(text, weixin_emojis_remap)
end

M.weixin_emojis_remap = {
   ["[可爱]"] = '[Joyful]', ["[大兵]"] = "[Commando]", ["[折磨]"] = "[Tormented]", ["[示爱]"] = "[Lips]", ["[挥手]"] = "[Surrender]",
   ["[街舞]"] = "[Meditate]", ['[笑哭]'] = '😂', ['[斜眼笑]'] = '[奸笑]', ['[doge]'] = '[🙃]',
}


M.get_coffee = function(what)
   for i = 1, 5 do
      if social_need_confirm and not yes_or_no_p("Will now open the Wechat App and goto it's home page") then
         return
      end
      weixin_open_homepage()
      log("Start to click for the favsearch " .. i)
      if social_need_confirm and not yes_or_no_p("Will now click my way to the Wechat bookmarks") then
         return
      end

      if social_need_confirm and not yes_or_no_p("First, click the “My” page") then
         return
      end

      adb_event"sleep .3 adb-tap 927 1830 sleep .1 adb-tap 927 1830 sleep .3"

      if social_need_confirm and not yes_or_no_p("Next, click the “My Favorites” button") then
         return
      end

      adb_tap_1080x2160(272, 633, 337, 782)

      for f_i = 1, 10 do
         local FavoriteIndexUI = "com.tencent.mm/com.tencent.mm.plugin.favorite.ui.FavoriteIndexUI"
         local top_window = wait_top_activity_n(2, FavoriteIndexUI)
         if top_window ~= FavoriteIndexUI then
            log("Still not FavoriteIndexUI at %d", f_i)
            if f_i == 10 then
               goto open_fav_search_again
            end
            if top_window == W.weixinLauncherActivity then
               log("Need click for fav again")
               adb_tap_1080x2160(272, 633, 501, 872)
            elseif top_window and top_window ~= "" then
               log("Failed to get FavoriteIndexUI, and not in W.weixinLauncherActivity at f_i = %d, top is %s", f_i, top_window)
               goto open_fav_search_again
            end
         else
            log("Got FavoriteIndexUI at %d", f_i)
            break
         end
      end

      if social_need_confirm and not yes_or_no_p("Next, click the “Search” button for the “My Favorites”") then
         return
      end
      adb_tap_1080x2160(883, 88, 833, 145)

      for fs_i = 1, 10 do
         if wait_input_target_n(1, "com.tencent.mm/com.tencent.mm.plugin.favorite.ui.FavSearchUI") ~= "" then
            log("Got FavSearchUI when fs_i is %d", fs_i)
            break
         else
            log("Still waiting for FavSearchUI at %d", fs_i)
            adb_tap_1080x2160(883, 88, 833, 145)
         end
      end
      if fs_i ~= 10 then
         break
      end
      :: open_fav_search_again ::
      log("Need retry " .. i)
      return
   end
   putclip"咕咕机 善良的动物"

   if social_need_confirm and not yes_or_no_p("Will now find the 咕咕机 Wechat App") then
      return
   end
   adb_event"key scroll_lock sleep .1 key enter sleep .5 "
   if social_need_confirm and not yes_or_no_p("Will now open the 咕咕机 Wechat App") then
      return
   end
   adb_tap_1080x2160(474, 356, 535, 458)
   if social_need_confirm and not yes_or_no_p("Will now wait for the input ready") then
      return
   end

   for i = 1, 80 do
      local input_target = wait_input_target_n(1, "com.tencent.mm/com.tencent.mm.plugin.webview.ui.tools.WebViewUI")
      if input_target:match"com.tencent.mm/com.tencent.mm.plugin.webview.ui.tools.WebViewUI" then
         break
      elseif input_target:match"com.tencent.mm/com.tencent.mm.plugin.search.ui.FTSMainUI" then
         adb_tap_1080x2160(474, 356, 535, 458)
      elseif i < 5 and adb_top_window() == "com.tencent.mm/com.tencent.mm.plugin.favorite.ui.FavSearchUI" then
         adb_tap_1080x2160(474, 356, 535, 458)
      end
      log("Click for coffee input: %s %d", input_target, i)

      adb_event"adb-tap 15 700"
      sleep(.1)
   end
   if what == "" then
      what = "秦师傅，给我来一杯拿铁，谢谢❤"
   end
   putclip(what)

   if social_need_confirm and not yes_or_no_p("Will now input your order for coffee") then
      return
   end
   adb_event"key scroll_lock sleep .5"
   if yes_or_no_p("Confirm to order coffee from Shifu Qin？") then
      if social_need_confirm then
         yes_or_no_p("I will alarm you in 3 minutes for your coffee")
         return
      end
      adb_event"key back sleep .2"
      adb_tap_1080x2160(534, 1603, 562, 1662)
      system{'alarm', '3', 'Go get your coffee (take your coffee ticket!)'}
   end

end
