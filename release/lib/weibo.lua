M.find_weibo_friend = function(friend_name)
   if friend_name == "" then
      friend_name = string_strip(M.select_args_with_history("weibo-friends", "请输入想找的微博联系人名字", "", " ")):gsub("@@wb$", "")
      if friend_name == "" then
         prompt_user("没有输入你想查找的微博联系人，无法查找")
         return
      end
   end

   putclip_nowait(friend_name)
   for i = 1, 3 do
      M.start_app(W.weibo_home_activity)
      adb_tap_1080x2160(748, 1855) -- click 3rd tab
      sleep(.3)
      adb_tap_1080x2160(270, 100) -- click search
      ime = wait_input_target_n(5, W.weibo_search_activity)
      if ime and not ime:match(W.weibo_search_activity) then
         log("wait for weibo search at %d: %s", i, ime)
         adb_event("key back")
      else
         break
      end
   end
   adb_event"key scroll_lock key enter"
   if yes_or_no_p("Is the first one who you are looking for?") then
      adb_event"adb-tap 680 456"
   end
end

M.picture_to_weibo_chat = function (pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = close_ime()
   local post_button = ('%d %d'):format(right_button_x, 1920 - ime_height - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]
      if i == 1 then
         for n = 1,30 do
            local window = adb_top_window()
            if window == W.weiboChatActivity then
               chatWindow = window
               adb_event(post_button .. " sleep .5 adb-tap 203 1430")
               wait_top_activity(W.weiboAlbumActivity)
            elseif window == W.weiboAlbumActivity then
               adb_event("adb-tap 521 398")
               sleep(.2)
            elseif window == W.weiboImagePreviewActivity then
               adb_event("sleep .5 adb-key back sleep .5")
               if wait_top_activity(W.weiboAlbumActivity) == W.weiboAlbumActivity then
                  break
               elseif adb_top_window() == W.weiboImagePreviewActivity then
                  adb_event("key back sleep .5 ")
               end
            end
         end
      end
      local pic_share_buttons = {
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(i_button)
   end
   adb_event("sleep .1 adb-tap 922 138")
   wait_top_activity(W.weiboChatActivity)
   adb_event("key back")
end

M.weibo_text_share = function(window)
   local repost = '?'
   if window == "com.sina.weibo/com.sina.weibo.DetailWeiboActivity" then
      repost = select_args{'Repost or comment', 'Repost', 'Comment', 'Repost and comment'}
      if repost:match('Repost') then
         debugging("doing post")
         adb_tap_bot_left()
      else
         adb_tap_mid_bot()
      end
      sleep(1)
   end
   if repost:match('and') then
      adb_event("sleep .1 adb-tap 57 1704")
   end
   adb_event{'key', 'scroll_lock'}
   if social_need_confirm and not yes_or_no_p("Confirm to share to weibo?") then
      return
   end
   if yes_or_no_p("Share to Weibo?") then
      adb_event{991, 166}
   end
end

M.wrench_share_to_weibo = function (text)
   share_text_to_app("com.sina.weibo", ".composerinde.ComposerDispatchActivity", text)
   wait_input_target(W.weiboShareActivity)
   if yes_or_no_p("Share to Weibo?") then
      wrench_send_action()
   end
end

M.picture_to_weibo_share = function(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
   share_pics_to_app("com.sina.weibo", ".composerinde.ComposerDispatchActivity", pics)
   wait_top_activity(W.weiboShareActivity)
   adb_event("adb-tap 162 286")
   wait_input_target(W.weiboShareActivity)
end

M.picture_to_weibo_comment = function(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local weiboShareActivity = adb_top_window() -- comment or forward
   if #pics ~= 1 then
      log("Weibo comment/repost supports only 1 picture")
   end
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]

      if i == 1 then
         wait_input_target(weiboShareActivity)
         local input_method, ime_height = close_ime()
         for n = 1,10 do
            if adb_top_window() == weiboShareActivity then
               adb_event("adb-tap 62 1843")
            elseif adb_top_window() == W.weiboAlbumActivity then
               adb_event("sleep .3 adb-tap 501 340 sleep .2")
               if wait_top_activity(weiboShareActivity) == weiboShareActivity then
                  break
               end
            end
            sleep(.2)
         end
      end
   end
end

M.emoji_for_weibo = function(text)
   return emoji_rewrite(text, weibo_emojis_remap)
end

M.post_weibo_answer = function(text)
   texts = split("\n\n", text)
   for _, p in ipairs(texts) do
      putclip(p)
      adb_event("sleep 1 key scroll_lock sleep .2 key enter sleep .5")
   end
end

M.weibo_emojis_remap = {
   ["[加油]"] = '💪', ['[勾引]'] = '[来]', ['[OK]'] = '[ok]', ['[强]'] = '[good]', ['[爱你]'] = '[haha]',
   ['[飞吻]'] = '[爱你]', ['[抱拳]'] = '[作揖]', ['[心碎]'] = '[伤心]', ['[爱心]'] = '[心]', ['[发呆]'] = '[傻眼]',
   ['[玫瑰]'] = '[鲜花]', ['[拥抱]'] = '[抱抱]', ['[呲牙]'] = '[嘻嘻]', ['[憨笑]'] = '[哈哈]', ['[笑哭]'] = '[笑cry]',
   ['[调皮]'] = '[挤眼]', ['[流泪]'] = '[泪]', ['[快哭了]'] = '[悲伤]', ['[抠鼻]'] = '[挖鼻]', ['[发怒]'] = '[怒]',
   ['[咒骂]'] = '[怒骂]', ['[流汗]'] = '[汗]', ['[惊恐]'] = '[吃惊]', ['[睡觉]'] = '[睡]', ['[糗大了]'] = '[打脸]',
   ['[难过]'] = '[失望]', ['[再见]'] = '[拜拜]', ['[胜利]'] = '[耶]', ['[无奈]'] = '[摊手]',
}

