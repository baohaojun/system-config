M.dd_pkg = "com.alibaba.android.rimet/"
M.dd_activity = "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.home.activity.HomeActivity"
M.dd_search_activity = "com.alibaba.android.rimet/com.alibaba.android.search.activity.GlobalSearchActivity"
M.dd_home = "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.LaunchHomeActivity"

M.open_dd = function(target_activity) -- target_activity can be one of ["search", "home"]
   local top_window, i
   local max_times = 6
   for i = 1, max_times do
      if not top_activity_match(M.dd_pkg) then
         start_app("com.alibaba.android.rimet")
      end

      if target_activity == "search" and M.m_focused_window == M.dd_search_activity then
         log("found dd search window")
         if find_scene("dd-sousuo-shanchu") then
            jump_out_of("dd-sousuo-shanchu")
            return
         end
         log("not found dd search window: %s", M.m_focused_window)
      elseif M.m_focused_window ~= M.dd_home then
         adb_back_from_activity()
      else
         if target_activity == "search" then
            if find_scene("dd-xiaoxi-sousuo") then
               jump_from_to("dd-xiaoxi-sousuo", "dd-xiaoxi-sousuozhong")
               return
            end
         end
         local found_duihua = find_scene("dd-xiaoxi-xuanzhong")
         if found_duihua and target_activity == "home" then
            return
         end

         local found_grey
         if not found_duihua then
            found_grey = find_scene("dd-xiaoxi-weixuanzhong")
            if found_grey and target_activity == "home" then
               return
            end
         end

         if not found_grey and not found_duihua then
            if i >= 4 then
               M.g_find_scene_debug = true
            else
               adb_back_from_activity()
            end
         elseif found_grey then
            jump_from_to("dd-xiaoxi-weixuanzhong", "dd-xiaoxi-xuanzhong")
            found_duihua = 1
         end

         if found_duihua then
            jump_from_to("dd-plus-button", "dd-xiaoxi-sousuo", {click_times = 2, x = -600})
            jump_from_to("dd-xiaoxi-sousuo", "dd-xiaoxi-sousuozhong")
            return
         end
      end
   end
end

M.wrench_find_dingding_contact = function(friend_name)
   putclip_nowait(friend_name)
   open_dd("search")
   adb_event"key scroll_lock key enter sleep .8"
   wait_for_scene("dd-NiKeNengXiangZhao")
   click_scene("dd-NiKeNengXiangZhao", {y = 200, x = 100})
end

M.click_pics = function(npics, pkg_prefix, extra_args)
   extra_args = extra_args or {cols = 4, pic1_is_button = true}
   cols = extra_args.cols or 4
   pic1_is_button = extra_args.pic1_is_button or false
   local pic_share_buttons = {}

   local click_2x1_x = scene_x(pkg_prefix .. "fatu-x2y1")
   local click_2x1_y = scene_y(pkg_prefix .. "fatu-x2y1")

   local click_3x1_x = scene_x(pkg_prefix .. "fatu-x3y1")
   local click_3x1_y = scene_y(pkg_prefix .. "fatu-x3y1")

   local click_2x2_x = scene_x(pkg_prefix .. "fatu-x2y2")
   local click_2x2_y = scene_y(pkg_prefix .. "fatu-x2y2")

   local cell_height = math.floor(click_2x2_y - click_2x1_y)
   local cell_width = math.floor(click_3x1_x - click_2x1_x)

   local click_1x1_x = click_2x1_x - cell_width
   local click_1x1_y = click_2x1_y

   for i = 1, npics do
      local col, rowt

      if pic1_is_button then
         col = i % 4 + 1
         row = math.floor(i / 4) + 1
      else
         col = (i - 1) % 4 + 1
         row = math.floor((i - 1) / 4) + 1
      end

      local tap_x = cell_width * (col - 1) + click_1x1_x + 14
      local tap_y = cell_height * (row - 1) + click_1x1_y + 20

      pic_share_buttons[i] = ("adb-tap-XY %d %d"):format(tap_x, tap_y)
      log("dingding tap: %d %s", i, pic_share_buttons[i])
      local button = pic_share_buttons[i]
      adb_event("sleep .2 " .. button .. " sleep .1")

      if i == 1 then
         jump_from_to(pkg_prefix .. "yuantu-weixuanzhong", pkg_prefix .. "yuantu-yixuanzhong")
      end
   end
end

M.click_dingding_pics = function(npics)
   click_pics(npics, "dingding-")
end

M.picture_to_dingding_chat = function(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local dd_extra_button="dingding-liaotian-gengduo-gongneng"
   if ime_is_active() then
      dd_extra_button = dd_extra_button .. "@ime"
   end

   jump_from_to(dd_extra_button, "dingding-xiangce-button")
   if #pics == 0 then
      jump_from_to(true, "dd/maybe-this-picture")
   end
   if find_scene("dd/maybe-this-picture") then
      local pkg_prefix = "dingding-"
      jump_from_to("dd/maybe-this-picture", "dingding-tupian-fasong", {x = -100, y = 200})
      -- error("hello world")
      jump_from_to(pkg_prefix .. "yuantu-weixuanzhong", pkg_prefix .. "yuantu-yixuanzhong")
      jump_from_to("dingding-tupian-fasong", "dingding-xiangce-button")
      adb_event("key back")
      return
   end

   jump_from_to("dingding-xiangce-button", "dingding-paishe-zhaopian")

   click_dingding_pics(#pics)
   jump_from_to("dingding-tupian-fasong", "dingding-xiangce-button")
   adb_event("key back")
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
   if window == W.weixinLauncherActivity then
      return picture_to_weixin_chat(pics)
   elseif window == "com.tencent.mm/com.tencent.mm.plugin.scanner.ui.BaseScanUI" then
      return M.picture_to_weixin_scan(pics)
   elseif window:match("^com.tencent.mm/com.tencent.mm.ui.chatting") then
      return picture_to_weixin_chat(pics)
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity" then
      return picture_to_qq_chat(pics)
   elseif window == "com.alibaba.android.rimet/com.alibaba.android.dingtalkim.activities.ChatMsgActivity" then
      return picture_to_dingding_chat(pics)
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.SplashActivity" then
      return picture_to_qq_chat(pics)
   elseif window == "com.sina.weibo/com.sina.weibo.weiyou.DMSingleChatActivity" then
      return picture_to_weibo_chat(pics)
   elseif window == W.weiboCommentActivity or window == W.weiboForwardActivity then
      return picture_to_weibo_comment(pics)
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

M.ding_search_and_select = function(name)
   wrench_post(name)
   wait_for_scene("dd/jianqun-weixuanzhong")
   jump_from_to("dd/jianqun-weixuanzhong", "dd/jianqun-yixuanzhong")
   jump_from_to("dd/jianqun-sousuo-close", "dd/jianqun-sousuo-hint")
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

M.emoji_for_dingding = function(text)
   return emoji_rewrite(text, dingding_emojis_remap)
end

M.dingding_emojis_remap = {
   ["[街舞]"] = "[跳舞]", ["[捂脸]"] = "[捂脸哭]", ["[100 分]"] = "[100分]"
}
