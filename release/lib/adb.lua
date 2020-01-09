M.wait_top_activity_n_ok = function(n_retry, activity)
   local window = wait_top_activity_n(n_retry, activity)
   if window == activity then
      return true
   end
end

M.wait_top_activity = function(...)
   return wait_top_activity_n(20, ...)
end

M.wait_top_activity_match = function(activity)
   debug("waiting for %s", activity)
   local window
   for i = 1, 20 do
      window = adb_top_window()
      if window:match(activity) then
         debug("wait ok")
         return window
      end
      sleep(.1)
   end
   if not yes_or_no_p("等了两秒钟也没有等到 %s 窗口，请确认是否放弃此操作（重启 Lua 后台）？", activity) then
      error("用户取消了小扳手的操作")
   end
   return window
end

M.wait_input_target = function(...)
   return wait_input_target_n(20, ...)
end

M.wait_input_target_n_ok = function(n_loop, activity)
   return (wait_input_target_n(n_loop, activity)):match(activity)
end

M.wait_input_target_n = function(n_loop, ...)
   activities = {...}
   for i = 1, #activities do
      debug("wait for input method for %s", activities[i])
   end
   for i = 1, n_loop do
      local window = adb_top_window()
      for ai = 1, #activities do
         local activity = activities[ai]
         if window:match(activity) or (window:match("^PopupWindow:") and M.m_focused_app:match(activity)) then
            local adb_window_dump = split("\n", M.m_window_dump)
            for x = 1, #adb_window_dump do
               if adb_window_dump[x]:match("mInputMethodTarget=") then
                  local input_method, ime_height, ime_connected = adb_get_input_window_dump()
                  if ime_connected then
                     local fields = split(" ", adb_window_dump[x])
                     local last = fields[#fields]:gsub("}", "")
                     return last
                  end
               end
            end
         end
      end
      sleep(.1)
   end
   return ""
end

M.adb_back_from_activity = function()
   top_activity = adb_top_window()

   adb_event"key back"

   for i = 1, 10 do
      sleep(.1)
      new_top_activity = adb_top_window()
      if new_top_activity ~= "" and new_top_activity ~= top_activity then
         return
      end
   end
end

M.adb_set_tap_params = function(pressure, size)
   if not pressure then
      pressure = math.random() * 0.1 + 0.1
   end

   if not size then
      size = math.random() * 0.1 + 0.10
   end

   adb_quick_input{("input set-pressure-size %.2f %.2f"):format(pressure, size)};
end

M.adb_long_press_XY = function(x, y, milli)
   adb_quick_input{("input touchscreen swipe %d %d %d %d %s;"):format(x, y, x, y, milli)}
   sleep(milli/1000)
end

M.adb_tap_XY = function(x, y) -- do not modify x and y
   -- log("adb_tap_XY(%d, %d)", x, y)
   adb_quick_input{("input tap %d %d"):format(x, y)}
end

M.adb_event = function(events)
   if type(events) == 'string' then
      debugging("adb_event %s", events)
      adb_event(split(" ", events))
      return
   end
   command_str = ''
   i = 1
   while true do
      if not events[i] then
         if not events[i - 1] then
            debugging("Error at i = %d, events: %s", i, join(' ', events))
            error("Error: wrong number of events?")
         else
            break
         end
      end

      if tonumber(events[i]) then
         local width_ratio, height_ratio = app_width_ratio, app_height_ratio

         if (events[i - 1] and events[i - 1]:match("no%-virt")) then
            width_ratio, height_ratio = 1, 1
         elseif events[i+1] * 2 < default_height then
            height_ratio = real_height_ratio
         end

         local action = (events[i - 1] or "adb-tap")
         if (action:match("%-down$")) then
            action = "wrench-down"
         elseif (action:match("%-up$")) then
            action = "wrench-up"
         elseif (action:match("%-move$")) then
            action = "wrench-move"
         else
            action = "tap"
         end

         if (events[i - 1] == 'adb-no-virt-key-tap' or events[i - 1] == 'adb-no-virt-key-up') then
            if m_is_recording then
               local record_file = io.open(m_is_recording, "a")
               local comment = select_args{"你正在录制屏幕，请输入对本次点击的目的描述", "", ""} -- You are recording screen operations, please comment this click
               top_window = adb_top_window()
               record_file:write(("wait_top_activity_n_ok(10, [[%s]])\n"):format(top_window))
               record_file:write(('adb_event"adb-tap %d %d" -- %s\n'):format(events[i], events[i+1], comment))
               record_file:close()
            end
         end

         local x, y
         x, y = events[i] * width_ratio, events[i+1] * height_ratio

         local add = ('input %s %d %d;'):format(action, x, y)
         command_str = command_str .. add
         i = i + 2
      elseif (events[i]):match('^adb%-long%-press') then
         ms = 500
         if (events[i]):match('^adb%-long%-press%-%d+') then
            ms = (events[i]):sub(#"adb-long-press-" + 1)
         end
         if sdk_version < 18 then
            ms = ""
         end
         local add = ('input touchscreen swipe %d %d %d %d %s;'):format(
            M.adjust_x(events[i+1]), M.adjust_y(events[i+2]),
            M.adjust_x(events[i+1]), M.adjust_y(events[i+2]), ms)
         if sdk_version < 17 then
            add = add:gsub("touchscreen ", "")
         end
         command_str = command_str .. add

         if sdk_version < 18 then
            command_str = command_str .. add
         end
         i = i + 3
      elseif events[i] == 'text' or events[i] == 'adb-text' then
         command_str = command_str .. ("input text %s;"):format(events[i+1])
         i = i + 2
      elseif events[i] == 'key' or events[i] == 'adb-key' then
         local event = events[i+1]:upper()
         command_str = command_str .. ('input keyevent %s;'):format(event)
         if event == "SCROLL_LOCK" then
            if M.need_wait_putclip then
               adb_wait_file_gone(M.sdcard_putclip_path)
               M.need_wait_putclip = false
            end
            check_scroll_lock()
            command_str = command_str .. "sleep .1;"
         end
         i = i + 2
      elseif events[i] == 'sleep' then
         command_str = command_str .. ('sleep %s || busybox sleep %s;'):format(events[i+1], events[i+1])
         i = i + 2
      elseif events[i] == 'swipe' or (events[i]):match('adb%-swipe%-') or (events[i]):match('adb%-no%-virt%-key%-.*swipe') then
         ms = 500
         local width_ratio, height_ratio = app_width_ratio, app_height_ratio

         if (events[i]):match('adb%-no%-virt%-key') then
            width_ratio, height_ratio = real_width_ratio, real_height_ratio
         end

         if (events[i]):match('adb%-swipe%-') then
            ms = (events[i]):sub(#'adb-swipe-' + 1)
         elseif (events[i]):match('adb%-no%-virt%-key%-swipe%-') then
            ms = (events[i]):sub(#'adb-no-virt-key-swipe-' + 1)
         end
         if sdk_version < 18 then
            ms = ""
         end

         local action = "swipe"
         if (events[i]):match('wrench%-swipe') then
            action = "wrench-swipe"
         end

         local add = ('input touchscreen %s %d %d %d %d %s;'):format(
            action,
            events[i+1] * width_ratio, events[i+2] * height_ratio,
            events[i+3] * width_ratio, events[i+4] * height_ratio, ms)
         if sdk_version < 17 then
            add = add:gsub("touchscreen ", "")
         end
         command_str = command_str .. add
         if sdk_version < 18 then
            command_str = command_str .. add
         end
         i = i + 5
      elseif events[i] == 'adb-tap-XY' then
         local add = ("input tap %d %d;"):format(events[i+1], events[i+2])
         command_str = command_str .. add
         i = i + 3
      elseif events[i] == 'adb-tap' or events[i]:match('adb%-no%-virt%-key%-') then
         i = i + 1
      else
         error(string.format("Error: unknown event: %d: '%s' (%s)", i, events[i], join(' ', events)))
      end
   end
   if adb_quick_input ~= nil then
      debugging("doing with " .. command_str)
      local res = adb_quick_input{command_str}
      if res ~= "input ok\n" then
         debugging("adb_quick_input not ok: %s, redo using adb sell", res)
         adb_shell(command_str)
      end
   else
      adb_shell(command_str)
   end
end

M.vnc_scroll_a_page = function(how)
   local x, y, old_x, old_y

   x = real_width / 2
   old_x = x

   local min_y = 50
   if how == "up" then
      old_y = real_height - min_y
      y = min_y
   elseif how == "down" then
      old_y = min_y * 2
      y = real_height - 1
   end
   adb_event(("adb-no-virt-key-wrench-swipe %s %s %s %s"):format(old_x, old_y, x, y))
end

M.vnc_scroll = function(key, mod)
   local x, y, old_x, old_y
   local x_delta = 0
   local y_delta = 0

   delta = 50

   if mod ~= "" then
      delta = 400
   end
   x = real_width / 2
   y = real_height / 2

   edge_ratio = 99.0 / 100

   if key == "up" then
      y_delta = -delta
   elseif key == "down" then
      y_delta = delta
   elseif key == "left" then
      x_delta = -delta * 4 -- I want to scroll kindle page to the left
      adb_event(("adb-no-virt-key-tap %d %d"):format(real_width * edge_ratio, real_height / 2))
      return
   elseif key == "right" then
      x_delta = delta * 4 -- I want to scroll to the right
      adb_event(("adb-no-virt-key-tap %d %d"):format(real_width * (1 - edge_ratio), real_height / 2))
      return
   end

   adb_event(("adb-no-virt-key-swipe-180 %s %s %s %s"):format(x, y, x + x_delta, y + y_delta))
end

M.vnc_page_down = function()
   M.vnc_scroll_a_page("down")
end

M.vnc_page_up = function()
   M.vnc_scroll_a_page("up")
end

M.adb_tap_bot_left = function()
   adb_event{20, 1882}
end

M.adb_tap_mid_bot = function()
   adb_event{560, 1840}
end

M.ask_for_window_type = function(window)
   window_type = select_args{('How to send the msg for %s?'):format(window),
                             'Find my send button using image matching',
                             "Enter with the <Enter> key",
                             'Do nothing, I will click the send button myself',
   }
   if window_type == 'Find my send button using image matching' then
      window_type = 'Find-Button'
   elseif window_type == "Enter with the <Enter> key" then
      window_type = 'Enter-Key'
   else
      window_type = 'No-Post'
   end
   window_post_button_map[window] = window_type
   save_window_types()
   return window_type
end

M.wrench_post = function(text, how_to_post, confirm_before_post) -- use weixin
   local window = adb_top_window()
   debug("sharing text: %s for window: %s", text, window)
   if text and text:match("^@%?") then
      wrench_post("@", 'No-Post')
      prompt_user("请选择你要 @ 谁，然后继续")
      text = text:gsub("^@%?", "")
      how_to_post = 'no-post'
   end

   if text and text:match("@%?$") then
      text = text:gsub("@%?$", "")
      wrench_post(text, 'Enter-Key')
      wrench_post("@", 'Enter-Key')
      return
   end

   if text then
      if text:match("@$") and not how_to_post then
         how_to_post = 'manual-post'
      end

      if text:match("^​") and text ~= "​" then
         text = text:sub(string.len("​") + 1)
         local func = loadstring(text)
         wrench_eval(func, text)
         return "executed string"
      end
      if text:match("^#!lua") and text ~= "#!lua" then
         text = text:sub(string.len("#!lua") + 1)
         local func = loadstring(text)
         wrench_eval(func, text)
         return "executed string"
      end

      local pkg = window:gsub("/.*", "")

      if pkg == 'com.tencent.mobileqq' then
         putclip(emoji_for_qq(text))
      elseif pkg == "com.tencent.mm" then
         -- text = text:gsub("\n", "​\n")
         putclip(emoji_for_weixin(text))
      elseif pkg == "com.sina.weibo"  then
         putclip(emoji_for_weibo(text))
      elseif pkg == "com.alibaba.android.rimet" then
         putclip(emoji_for_dingding(text))
      else
         putclip(text)
      end
   end
   if window == W.weixinAlbumPreviewActivity or window == W.weiboPicFilterActivity then
      sleep(.5)
       window = adb_top_window()
   end

   if window == "com.sina.weibo/com.sina.weibo.qac.answer.AnswerComposerActivity" then
      post_weibo_answer(text)
      return
   end

   if window then print("window is %s", window) end

   if string.match(window, "^PopupWindow:") then
      window = M.m_focused_app
   end

   if window == "com.immomo.momo/com.immomo.momo.android.activity.feed.PublishFeedActivity"
      or (window:match("^com.sina.weibo/") and not window:match("com.sina.weibo/com.sina.weibo.weiyou.DMSingleChatActivity"))
   then
      weibo_text_share(window)
      return
   elseif window == "com.google.android.gm/com.google.android.gm.ConversationListActivityGmail" then
      local how = select_args{"请选择回复方法", "单独回复", "群体回复", "手动回复"}
      if how == "单独回复" then
         adb_event("key dpad_down key dpad_down key tab key tab key enter sleep 1")
      elseif how == "群体回复" then
         adb_event("key dpad_down key tab key dpad_down key enter key enter key tab key tab key enter sleep 1")
      else
         adb_event("sleep 1")
      end
      wrench_post()
      return
   elseif postAfterBackKey(window) then
      return
   elseif window == "com.google.android.gm/com.google.android.gm.ComposeActivityGmail" then
      adb_event("key scroll_lock adb-tap 870 173")
      return
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.QQLSActivity" then
      adb_event("adb-tap 297 884 key scroll_lock adb-tap 923 909")
   elseif text and (
      window == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI" or
         window == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsCommentUI"
   ) then
      weixin_text_share(window, text)
      return
   elseif window == "SmsPopupDialog" then
      wrench_sms(window)
      return
   elseif window == "com.google.android.apps.plus/com.google.android.apps.plus.phone.sharebox.PlusShareboxActivity" then
      wrench_google_plus(window)
      return
   elseif window == "com.smartisanos.notes/com.smartisanos.notes.NotesActivity" then
      wrench_smartisan_notes(window)
      return
   elseif window == W.smartisan_mail_compose or
      window == "com.android.email/com.android.email.activity.Welcome" or
      window == "com.android.email/com.android.email2.ui.MailActivityEmail" or
   window == W.emailSmartisanActivity then
      wrench_mail(window)
      return
   else
      local post_button = ''
      local input_method, ime_height, ime_connected = adb_get_input_window_dump() -- $(adb dumpsys window | perl -ne 'print if m/^\s*Window #\d+ Window\{[a-f0-9]* u0 InputMethod\}/i .. m/^\s*mHasSurface/')
      log("input_method is %s, ime_xy is %s, ime_connected is %s", input_method, ime_height, ime_connected)
      -- debugging("ime_xy is %s", ime_xy)

      if not input_method then
         if not ime_connected then
            wait_input_target(window)
            if not adb_top_window():match("^PopupWindow") then
               adb_event("key back")
               for n = 1, 5 do
                  input_method, ime_height, ime_connected = adb_get_input_window_dump()
                  -- log("ime_height is %d: %d", ime_height, n)
                  if ime_height == 0 then
                     adb_top_window() -- make sure we know that the nav bar is gone.
                     break
                  else
                     sleep(.2 * n)
                  end
               end
            end
         end
      end

      if ime_height ~= 0 then
         post_button = ('send-button/%s.ime-on'):format(window)
      else
         post_button = ('send-button/%s.ime-off'):format(window)
      end

      local window_type = (how_to_post or window_post_button_map[window])

      if not window_type then
         window_type = ask_for_window_type(window)
      end

      if text then
         adb_event("key scroll_lock")
      end

      log("window type is %s", window_type)

      if window_type == 'Find-Button' then
         log("post button is %s", post_button)
         click_post_button(post_button)
      elseif window_type == 'Enter-Key' then
         adb_event"key enter"
      end
   end
   return "text sent"
end

M.wrench_send_action = function()
   wrench_post('​')
end

M.wrench_post2 = function(texwrench, text2)
   putclip(texwrench)
   adb_event("key scroll_lock key dpad_down")
   wrench_post(text2)
end

M.adb_get_input_window_dump = function()
   -- $(adb dumpsys window | perl -ne 'print if m/^\s*Window #\d+ Window\{[a-f0-9]+.*\SInputMethod/i .. m/^\s*mHasSurface/')
   local dump_str = adb_pipe("dumpsys input_method; dumpsys window policy; dumpsys window windows")
   local dump = split("\n", dump_str)
   local current_input_method
   local input_method_lines = {}
   local input_method_active = false
   local looking_at_input_method = false
   local looking_at_input_method_package = ""
   for i = 1, #dump do
      if dump[i]:match("mCurMethodId=") then
         current_input_method = dump[i]:gsub(".*mCurMethodId=", "")
         current_input_method = current_input_method:gsub("/.*", "") -- only the package
      end
      if not looking_at_input_method
         and dump[i]:match("^%s*Window #?%d* ?Window{[a-f0-9]+.*%sInputMethod")
      then
         looking_at_input_method = true
         debugging("looking_at_input_method is true")
      end
      if looking_at_input_method == true then
         debugging("got a line: %s", dump[i])

         if dump[i]:match("package=") then
            looking_at_input_method_package = dump[i]:gsub(".*package=", "")
            looking_at_input_method_package = looking_at_input_method_package:gsub("%s.*", "")
            if current_input_method == looking_at_input_method_package then -- only look at the last section
               input_method_lines = {}
            end
         end

         if not current_input_method or current_input_method == looking_at_input_method_package then
            input_method_lines[#input_method_lines + 1] = dump[i]
         end
         if dump[i]:match("^%s*mHasSurface") then
            looking_at_input_method = false
         end
      end
   end
   local input_window_dump = join("\n", input_method_lines)
   input_method_active = string.match(input_window_dump, "mHasSurface=true")
   local ime_xy = last(string.gmatch(input_window_dump, "Requested w=%d+ h=%d+"))
   local ime_height = 0
   if input_method_active and ime_xy:match('Requested w=%d+ h=') then
      ime_height = ime_app_height * 0.105 * 4
   end
   debug("ime_height = %d; real_height = %d; default_height = %d; napp_height = %d",
         ime_height, real_height, default_height, app_height)

   ime_height = ime_height * default_height / app_height
   M.ime_height = ime_height
   M.input_method_active = input_method_active

   local ime_connected = not (
      dump_str:match("mServedInputConnection=null") or
         dump_str:match("mStartedInputConnection=null")
   )
   return input_method_active, ime_height, ime_connected, current_input_method
end

M.reset_input_method = function()
   if phone_info_map['user_input_method'] then
      input_method_id = phone_info_map['user_input_method']
      local command = (
         [[
cd /data/data/com.android.shell
PATH=/data/data/com.android.shell/bin:$PATH
touch not-started.$$
setsid nohup setsid /system/bin/sh -c 'set -x
rm 'not-started.$$'
if test "$(cat /sdcard/Wrench/usb_online)" = watching-ime; then
    exit
fi
echo watching-ime > /sdcard/Wrench/usb_online
ime set com.wrench.inputmethod.pinyin/.PinyinIME
while test -e /sdcard/Wrench/usb_online; do
    sleep 5
done
ime enable %s
ime set %s
killall -INT androidvncserver || busybox killall -INT androidvncserver
'>nohup.ime 2>&1 & for i in 1 2 3 4 5 $(seq 1 20); do
   if test -e not-started.$$; then
      sleep .1 || sleep 1
   else
       echo started at i = $i, pid = $$ > nohup.start
       exit
   fi
done
]]):format(input_method_id, input_method_id)
      adb_shell(command)
   end
end

M.wait_top_activity_n = function(n_retry, ...)
   activities = {...}
   for i = 1, #activities do
      debug("wait for top activity: %s", activities[i])
   end

   local window
   for i = 1, n_retry do
      for ai = 1, #activities do
         local activity = activities[ai]
         window = adb_top_window()
         if window == activity then
            debug("wait ok")
            return window
         end
      end
      sleep(.1)
   end
   return window
end

M.adb_top_window = function()
   -- dumpsys window|grep mFocusedWindow|perl -npe 's/.*?(\S+)}$/$1/')
   for i = 1, 50 do
      update_screen_size()
      if M.m_focused_window ~= "" then
         return M.m_focused_window
      end

      sleep(.1)
      if i > 20 then
         log("Can't get a valid top window at %d", i)
      end

      if i == 50 then
         error("Error: can't get a valid top window")
      end
   end
   return ""
end

M.adb_tap_1080x2160 = function (x, y, x1080, y1920, long_tap, random)
   if not (real_height >= 2160 and real_width == 1080 or
           real_height == 1080 and real_width >= 2160) then
      x = x1080 or x
      y = y1920 or y
   end
   local action = "adb-tap "
   if random then
      if type(random) ~= 'number' then
         random = 5
      end
      x = x - random + math.floor(random * 2 * math.random())
      y = y - random + math.floor(random * 2 * math.random())
      adb_set_tap_params(1.0)
   end

   if long_tap then
      if type(long_tap) ~= number then
         long_tap = 800
      end
      action = ("adb-long-press-%d "):format(long_tap)
   end

   adb_event(action .. x .. " " .. y)
end

M.adb_input_method_is_null = function ()
   -- if adb dumpsys input_method | grep mServedInputConnection=null -q; then
   local dump = adb_pipe{'dumpsys', 'input_method'}
   if dump:match("mServedInputConnection=null") or dump_str:match("mStartedInputConnection=null") then
      return true
   else
      return false
   end
end

M.adb_is_window = function (w)
   return w == adb_top_window()
end

M.save_window_types = function()
   local mapfile = io.open(M.configDirFile("window_post_botton.lua"), "w")
   mapfile:write("local map = {}\n")
   for k, v in spairs(window_post_button_map) do
      if k ~= "" then
         mapfile:write(("map['%s'] = '%s'\n"):format(k, v))
      end
   end
   mapfile:write("return map\n")
   mapfile:close()
end

M.top_activity_match = function(regexp)
   local top_window = adb_top_window()
   if top_window and top_window:match(regexp) then
      return true
   else
      return false
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
