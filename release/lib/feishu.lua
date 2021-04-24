M.fs_pkg = "com.ss.android.lark/"
M.fs_activity = "com.ss.android.lark/com.ss.android.lark.main.app.MainActivity"
M.fs_search_activity = "com.ss.android.lark/com.ss.android.lark.search.impl.func.global.SearchActivity"
M.fs_home = M.fs_activity
M.fs_chat_activity = "com.ss.android.lark/com.ss.android.lark.chatwindow.ChatWindowActivity"

M.open_fs = function(target_activity) -- target_activity can be one of ["search", "home"]
   local top_window, i
   local max_times = 20
   for i = 1, max_times do
      if not top_activity_match(M.fs_pkg) then
         start_app(M.fs_pkg)
      end

      if target_activity == "search" and M.m_focused_window == M.fs_search_activity then
         log("found fs search window")
         if find_scene("fs-sousuo-shanchu") then
            jump_out_of("fs-sousuo-shanchu")
            return
         end
         log("not found fs search window: %s", M.m_focused_window)
      elseif M.m_focused_window ~= M.fs_home then
         adb_back_from_activity()
      else
         if target_activity == "search" then
            if find_scene("fs-xiaoxi-sousuo") then
               jump_from_to("fs-xiaoxi-sousuo", "fs-xiaoxi-sousuozhong")
               return
            end
         end
         local found_duihua = find_scene("fs-xiaoxi-xuanzhong")
         if found_duihua and target_activity == "home" then
            return
         end

         local found_grey
         if not found_duihua then
            found_grey = find_scene("fs-xiaoxi-weixuanzhong")
            if found_grey and target_activity == "home" then
               return
            end
         end

         if not found_grey and not found_duihua then
            if M.m_focused_window == M.fs_home then
               click_scene("feishu/plus-eclipsed", {skip_refind = true})
               log("not found_grey and not found_duihua, i = %d", i)
            elseif i >= 8 then
               M.g_find_scene_debug = true
            else
               adb_back_from_activity()
            end
         elseif found_grey then
            jump_from_to("fs-xiaoxi-weixuanzhong", "fs-xiaoxi-xuanzhong")
            found_duihua = 1
         end

         if found_duihua then
            jump_from_to("fs-xiaoxi-xuanzhong", "fs-xiaoxi-sousuo", {click_times = 2})
            jump_from_to("fs-xiaoxi-sousuo", "fs-xiaoxi-sousuozhong")
            return
         end
      end
   end
end

M.wrench_find_feishu_contact = function(friend_name)
   putclip_nowait(friend_name)
   open_fs("search")
   adb_event"key scroll_lock key enter sleep .8"
   wait_for_scene("fs-NiKeNengXiangZhao")
   jump_from_to("fs-NiKeNengXiangZhao", "fs/send-text-init", {y = 200, x = 100, skip_refind = 1})
   jump_from_to("fs/send-text-init", "fs/send-text-up", {y = -100, x = -600, skip_refind = 1})
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
      log("feishu tap: %d %s", i, pic_share_buttons[i])
      local button = pic_share_buttons[i]
      adb_event("sleep .2 " .. button .. " sleep .1")

      if i == 1 then
         jump_from_to(pkg_prefix .. "yuantu-weixuanzhong", pkg_prefix .. "yuantu-yixuanzhong")
      end
   end
end

M.click_feishu_pics = function(npics)
   click_pics(npics, "feishu-")
end

M.picture_to_feishu_chat = function(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local fs_tupian_button="fs/liaotian-tupian"

   if not ime_is_active() then
      jump_from_to("fs/send-text-init", "fs/send-text-up", {y = -100, x = -600, skip_refind = 1})
   end
   if ime_is_active() then
      fs_tupian_button = fs_tupian_button .. "@ime"
   end

   jump_from_to(fs_tupian_button, "fs/xiangce-button", {skip_refind = 1})

   jump_from_to("fs/xiangce-yuantu-weixuanzhong", "fs/xiangce-yuantu-yixuanzhong", {skip_refind = 1})
   jump_from_to("fs/xiangce-fast-clicked-1", "fs/xiangce-fast-clicked-1", {skip_refind = 1})

   jump_from_to("fs/tupian-fasong", "fs/liaotian-tupian")
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

M.feishu_search_and_select = function(name)
   wrench_post(name)
   wait_for_scene("fs/jianqun-weixuanzhong")
   jump_from_to("fs/jianqun-weixuanzhong", "fs/jianqun-yixuanzhong")
   jump_from_to("fs/jianqun-sousuo-close", "fs/jianqun-sousuo-hint")
end

M.emoji_for_fs = function(text)
   return emoji_rewrite(text, fs_emojis_remap)
end

dofile_res, M.fs_emojis_remap = pcall(dofile, "fs-emojis-remap.lua")
if not dofile_res then
   M.fs_emojis_remap = {}
end
