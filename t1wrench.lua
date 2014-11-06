#!/usr/bin/lua
local function shell_quote(str)
   return "'" .. string.gsub(str, "'", "'\\''") .. "'"
end

local function debug(fmt, ...)
   print(string.format(fmt, ...))
end

local function split(pat, str)
   local start = 1
   if pat == ' ' then
      pat = "%s+"
   end

   local list, i, j = {}
   while true do
      i, j = str:find(pat, start)
      if (i and i >= start) then
         if i > start then
            list[#list + 1] = str:sub(start, i - 1)
         end
      elseif #str >= start then
         list[#list + 1] = str:sub(start)
      end
      if i then
         start = j + 1
      else
         break
      end
   end
   return list
end

local function join(mid, args)
   text = ''
   for i = 1, #args do
      if i ~= 1 then
         text = text .. mid
      end
      text = text .. args[i]
   end
   return text
end

local function system(cmds)
   if type(cmds) == 'string' then
      os.execute(cmds)
   elseif type(cmds) == 'table' then
      command_str = ''
      for i = 1, #cmds do
         command_str = command_str .. shell_quote(cmds[i]) .. ' ';
      end
      os.execute("set -x; " .. command_str)
   end
end

local function adb_do(func, cmds)
   if type(cmds) == 'string' then
      return adb_do(func, {"sh", "-c", cmds})
   else
      assert(type(cmds) == 'table', "command must be a sequence");
      if (#cmds == 1) then
         return adb_do(func, cmds[1])
      end

      command_str = ''
      quoted_cmds = {}
      for i = 1, #cmds do
         quoted_cmds[i] = shell_quote(shell_quote(cmds[i]))
         if string.find(quoted_cmds[i], " ") then
            quoted_cmds[i] = '\\"' .. quoted_cmds[i] .. '\\"'
         end
         command_str = command_str .. quoted_cmds[i] .. ' '
      end
      return func("set -x; the-true-adb shell " .. command_str)
   end
end

local function adb_shell(cmds)
   return adb_do(os.execute, cmds)
end

local function adb_pipe(cmds)
   return adb_do(io.popen, cmds):read('*a'):gsub("\r", "")
end

local function adb_focused_window()
   wdump = adb_pipe{"dumpsys", "window"}
   print('wdump is ' .. wdump)
   return string.match(wdump, "mFocusedWindow[^}]*%s(%S+)}")
end

local function select_args(args)
   return args[1]
end

local function adb_event(events)
   if type(events) == 'string' then
      adb_event(split(" ", events))
      return
   end
   command_str = ''
   i = 1
   while true do
      if tonumber(events[i]) then
         command_str = command_str .. ('input tap %d %d;'):format(events[i], events[i+1])
         i = i + 2
      elseif events[i] == 'tap2' then
         i = i + 1
         command_str = command_str .. ('input tap %d %d;'):format(events[i], events[i+1])
         command_str = command_str .. ('input tap %d %d;'):format(events[i], events[i+1])
         i = i + 2
      elseif events[i] == 'key' then
         command_str = command_str .. ('input keyevent %s;'):format(events[i+1]:upper())
         i = i + 2
      elseif events[i] == 'sleep' then
         command_str = command_str .. ('sleep %s;'):format(events[i+1])
         i = i + 2
      elseif events[i] == 'swipe' then
         command_str = command_str .. ('input touchscreen swipe %s %s %s %s 500;'):format(
            events[i+1], events[i+2], events[i+3], events[i+4])
         i = i + 5
      elseif events[i] == 'adb-tap' then
         i = i + 1
      elseif events[i] then
         error(string.format("Error: unknown event: %d: '%s' (%s)", i, events[i], join(' ', events)))
      elseif not events[i - 1] then
         debug("Error at i = %d, events: %s", i, join(' ', events))
         error("Error: wrong number of events?")
      else
         break
      end
   end
   adb_shell(command_str)
end

local function adb_tap_bot_left()
   adb_event{20, 1882}
end

local function adb_tap_mid_bot()
   adb_event{560, 1840}
end

local function sleep(time)
   system("sleep " .. time)
end

local function t1_weibo(window)
   if window == "com.sina.weibo/com.sina.weibo.DetailWeiboActivity" then
      repost = select_args{'repost', 'comment'}
      if repost == 'repost' then
         adb_tap_bot_left()
      else
         adb_tap_mid_bot()
      end
      sleep(.5)
   end
   adb_event{'key', 'scroll_lock', 991, 166}
end

local function t1_weixin_new(window)
   adb_event{'key', 'scroll_lock', 961, 171}
end

local function t1_sms(window)
   adb_event{182, 1079, 'key', 'scroll_lock', 864, 921}
end

local function t1_google_plus(window)
   adb_event{467, 650, 'key', 'scroll_lock', 932, 1818}
end

local function t1_smartisan_notes(window)
   adb_event{'key', 'scroll_lock', 940, 140, 933, 117, 323, 1272, 919, 123}
end

local function t1_mail(window)
   if window == 'com.android.email/com.android.email.activity.Welcome' or window == 'com.android.email/com.android.email2.ui.MailActivityEmail' then
      adb_tap_mid_bot()
      sleep(2)
   end
   adb_event{'key', 'scroll_lock'}
   if window == 'com.google.android.gm/com.google.android.gm.ComposeActivityGmail' then
      adb_event{806, 178}
   else
      adb_event{998, 174}
   end
end

local function t1_paste()
   adb_event{'key', 'scroll_lock'}
end

local function last(func)
   local x, y
   y = func()
   while y do
      x = y
      y = func()
   end
   return x
end

local function adb_get_input_window_dump()
   -- $(adb dumpsys window | perl -ne 'print if m/^\s*Window #\d+ Window\{[a-f0-9]* u0 InputMethod\}/i .. m/^\s*mHasSurface/')
   local dump = adb_pipe{'dumpsys', 'window'}
   local input_method = {}
   local started = false
   dump = split("\n", dump)
   for i = 1, #dump do
      if not started and dump[i]:match("^%s*Window #%d+ Window{[a-f0-9]* u0 InputMethod}") then
         started = true
      end
      if started == true then
         input_method[#input_method + 1] = dump[i]
         if dump[i]:match("^%s*mHasSurface") then
            started = false
         end
      end
   end
   local input_window_dump = join("\n", input_method)
   local input_method = string.match(input_window_dump, "mHasSurface=true")
   local ime_xy = last(string.gmatch(input_window_dump, "Requested w=1080 h=%d+"))
   local ime_height = 0
   if input_method and ime_xy:match('Requested w=1080 h=') then
      ime_height = ime_xy:sub(#'Requested w=1080 h=' + 1)
      if ime_height == '1525' then -- this is latin input method, it's wrong
         ime_height = 800
      end
   end
   return input_method, ime_height
end

local function adb_input_method_is_null()
   --         if adb dumpsys input_method | grep mServedInputConnection=null -q; then
   local dump = adb_pipe{'dumpsys', 'input_method'}
   if dump:match("mServedInputConnection=null") then
      return true
   else
      return false
   end
end

local function t1_post(text) -- use weixin
   local file = io.open("/tmp/lua-smartisan-t1.txt", "w")
   file:write(text)
   file:close()
   system{'adb', 'push', '/tmp/lua-smartisan-t1.txt', '/sdcard/putclip.txt'}
   adb_shell(
      [[
         am startservice --user 0 -n com.bhj.setclip/.PutClipService&
         for x in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do
             if test -e /sdcard/putclip.txt; then
                 sleep .1;
                 echo $x;
             else
                 exit;
             fi;
         done
        ]])
   window = adb_focused_window()
   print("window is " .. window)
   if window == "com.sina.weibo/com.sina.weibo.EditActivity" or window == "com.sina.weibo/com.sina.weibo.DetailWeiboActivity" then
      t1_weibo(window)
      return
   elseif window == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI" or window == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsCommentUI" then
      t1_weixin_new(window)
      return
   elseif window == "SmsPopupDialog" then
      t1_sms(window)
      return
   elseif window == "com.google.android.apps.plus/com.google.android.apps.plus.phone.sharebox.PlusShareboxActivity" then
      t1_google_plus(window)
      return
   elseif window == "com.smartisanos.notes/com.smartisanos.notes.NotesActivity" then
      t1_smartisan_notes(window)
      return
   elseif window == "com.android.email/com.android.mail.compose.ComposeActivity" or
      window == "com.android.email/com.android.email.activity.Welcome" or
   window == "com.android.email/com.android.email2.ui.MailActivityEmail" then
      t1_mail(window)
      return
   elseif string.match(window, "^PopupWindow:") then
      t1_paste()
      return
   else
      local add, post_button = '', '958 1820'
      local input_method, ime_height = adb_get_input_window_dump() -- $(adb dumpsys window | perl -ne 'print if m/^\s*Window #\d+ Window\{[a-f0-9]* u0 InputMethod\}/i .. m/^\s*mHasSurface/')
      -- debug("input_method is %s", input_method)
      -- debug("ime_xy is %s", ime_xy)

      if input_method then
         add = "key BACK"
      else
         add = "" -- # add="560 1840 key DEL key BACK"
      end
      if input_method then
         if ime_height ~= 0 then
            add = ''
            post_button = ('984 %d'):format(1920 - ime_height - 50)
         end
      else
         if adb_input_method_is_null() then --         if adb dumpsys input_method | grep mServedInputConnection=null -q; then
            add = '560 1840 sleep .1 997 1199 sleep .1'
         end
      end

      if window == "com.github.mobile/com.github.mobile.ui.issue.CreateCommentActivity" then
         post_button = '954 166'
      end

      adb_event(split(" ", string.format("%s key scroll_lock %s", add, post_button)))
   end
   return "text sent\n"
end

local function upload_pics(...)
   local pics = {...}
   adb_shell(
      [[
            for x in /sdcard/DCIM/Camera/t1wrench-*; do
               if test -e "$x"; then
                  rm -f "$x"
                  am startservice -n com.bhj.setclip/.PutClipService --es picture "$x"
               fi
           done
   ]])
   debug("pics[1] is %s", pics[1])
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = ('/sdcard/DCIM/Camera/t1wrench-%d%s'):format(i, ext)
      system{'adb', 'push', pics[i], target}
      adb_shell{"am", "startservice", "-n", "com.bhj.setclip/.PutClipService", "--es", "picture", target}
   end
end

local function picture_to_weixin_share(...)
   local pics = {...}
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = ('/sdcard/DCIM/Camera/t1wrench-%d%s'):format(i, ext)

      if i == 1 then
         adb_shell("am start -n com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI")
         adb_event("sleep .5 adb-tap 141 597 sleep .5")
      end

      local pic_share_buttons = {
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(split(" ", i_button))
   end
   adb_event("adb-tap 901 1841 adb-tap 75 1867 adb-tap 903 133")
   return "Prompt: please say something"
end

local function picture_to_weibo_share(...)
   local pics = {...}
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = ('/sdcard/DCIM/Camera/t1wrench-%d%s'):format(i, ext)

      if i == 1 then
         adb_shell("am start -n com.sina.weibo/com.sina.weibo.EditActivity")
         adb_event("sleep 1 adb-tap 104 980 sleep 2")
      end

      local pic_share_buttons = {
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(split(" ", i_button))
   end
   adb_event("adb-tap 141 1849 adb-tap 922 1891")
end

local function picture_to_weixin_chat(...)
   local pics = {...}
   local input_method, ime_height = adb_get_input_window_dump()
   local post_button = ('984 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = ('/sdcard/DCIM/Camera/t1wrench-%d%s'):format(i, ext)
      if i == 1 then
         local events = post_button .. " sleep .1 swipe 125 1285 500 1285 sleep .1 " ..
            "125 1285 sleep 1"
         adb_event(split(" ", events))
      end

      local pic_share_buttons = {
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(split(" ", i_button))
   end
   adb_event("adb-tap 944 1894 adb-tap 59 1871 adb-tap 927 148")
end

local function picture_to_qq_chat(...)
   local pics = {...}
   local input_method, ime_height = adb_get_input_window_dump()
   local post_button = ('159 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = ('/sdcard/DCIM/Camera/t1wrench-%d%s'):format(i, ext)
      if i == 1 then
         local events = post_button .. " sleep .1 adb-tap 203 1430 sleep .1"
         adb_event(split(" ", events))
         while adb_focused_window() ~= "com.tencent.mobileqq/com.tencent.mobileqq.activity.photo.AlbumListActivity" do
            adb_event{118, 152, "sleep", .5}
         end
         adb_event("457 493 sleep .1 swipe 519 403 519 1800 sleep .3")
      end
      local pic_share_buttons = {
         "adb-tap 191 394",
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(split(" ", i_button))
   end
   adb_event("adb-tap 608 1831 adb-tap 403 1679 adb-tap 918 1862 sleep .5 adb-tap 312 1275")
end

local function picture_to_qqlite_chat(...)
   local pics = {...}
   local input_method, ime_height = adb_get_input_window_dump()
   local post_button = ('984 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = ('/sdcard/DCIM/Camera/t1wrench-%d%s'):format(i, ext)
      if i == 1 then
         local events = post_button .. " sleep .1 adb-tap 203 1430 sleep .1"
         adb_event(split(" ", events))
         while adb_focused_window() ~= "com.tencent.qqlite/com.tencent.mobileqq.activity.photo.AlbumListActivity" do
            adb_event{118, 152, "sleep", .5}
         end
         adb_event{457, 493, 'sleep', .5}
      end
      local pic_share_buttons = {
         "adb-tap 191 394",
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(split(" ", i_button))
   end
   adb_event("adb-tap 519 1841 adb-tap 434 1071 adb-tap 918 1862 sleep .5 adb-tap 279 1221")
end

local function picture_to_weibo_chat(...)
   local pics = {...}
   local input_method, ime_height = adb_get_input_window_dump()
   local post_button = ('984 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = ('/sdcard/DCIM/Camera/t1wrench-%d%s'):format(i, ext)
      if i == 1 then
         local events = post_button .. " sleep .1 adb-tap 375 1410 sleep .1 adb-tap 645 135 sleep .2 adb-tap 369 679 sleep 2"
         adb_event(split(" ", events))
      end
      local pic_share_buttons = {
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(split(" ", i_button))
   end
   adb_event("adb-tap 943 1868 adb-tap 194 1163")
end

local function t1_picture(...)
   local pics = {...}
   upload_pics(...)
   local window = adb_focused_window()
   if window == "com.tencent.mm/com.tencent.mm.ui.LauncherUI" then
      picture_to_weixin_chat(...)
   elseif window == "com.tencent.qqlite/com.tencent.mobileqq.activity.ChatActivity" then
      picture_to_qqlite_chat(...)
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity" then
      picture_to_qq_chat(...)
   elseif window == "com.sina.weibo/com.sina.weibo.weiyou.DMSingleChatActivity" then
      picture_to_weibo_chat(...)
   elseif window:match("com.sina.weibo") then
      picture_to_weibo_share(...)
   elseif window:match("com.tencent.mm") then
      picture_to_weixin_share(...)
   else
      return "Error: can't decide where to share"
   end
   return #pics .. " pictures sent\n"
end

local M = {}
M.t1_post = t1_post
M.adb_shell = adb_shell
M.adb_pipe = adb_pipe
M.t1_picture = t1_picture

if arg and type(arg) == 'table' and string.find(arg[0], "t1wrench.lua") then
   -- t1_post(join(' ', arg))
   t1_picture(arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9])
else
   return M
end
