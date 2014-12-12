#!/usr/bin/lua

-- functions
local shell_quote, putclip, t1_post
local adb_start_activity
local picture_to_weixin_share, picture_to_weibo_share
local picture_to_momo_share
local adb_get_input_window_dump, adb_top_window
local adb_start_weixin_share, adb_is_window
local adb_focused_window
local t1_config, check_phone
local emoji_for_qq, debug, get_a_note
local adb_get_last_pic
-- variables
local using_scroll_lock = true
local using_adb_root
local adb_unquoter
local is_windows = false
local debug_set_x = ""
local ime_height_ref = 874
local default_width, default_height = 1080, 1920
local init_width, init_height = 1080, 1920
local app_width, app_height = 1080,1920
local width_ratio, height_ratio = app_width / default_width,  app_height / default_height
local using_smartisan_os = true
local using_xiaomi_os = false
local using_oppo_os = false
local brand = "smartisan"
local model = "T1"
local qq_emojis
local sdk_version = 19
local emojis, emojis_map
local the_true_adb = "./the-true-adb"

local qq_emoji_table = {
   "微笑", "撇嘴", "色", "发呆", "得意", "流泪", "害羞", "闭嘴", "睡", "大哭",
   "尴尬", "发怒", "调皮", "呲牙", "惊讶", "难过", "酷", "冷汗", "抓狂", "吐",
   "偷笑", "可爱", "白眼", "傲慢", "饥饿", "困", "惊恐", "流汗", "憨笑", "大兵",
   "奋斗", "咒骂", "疑问", "嘘", "晕", "折磨", "衰", "骷髅", "敲打", "再见",
   "擦汗", "抠鼻", "鼓掌", "糗大了", "坏笑", "左哼哼", "右哼哼", "哈欠", "鄙视",
   "委屈", "快哭了", "阴险", "亲亲", "吓", "可怜", "菜刀", "西瓜", "啤酒",
   "篮球", "乒乓", "咖啡", "饭", "猪头", "玫瑰", "凋谢", "示爱", "爱心", "心碎",
   "蛋糕", "闪电", "炸弹", "刀", "足球", "瓢虫", "便便", "月亮", "太阳", "礼物",
   "拥抱", "强", "弱", "握手", "胜利", "抱拳", "勾引", "拳头", "差劲", "爱你",
   "NO", "OK", "爱情", "飞吻", "跳跳", "发抖", "怄火", "转圈", "磕头", "回头",
   "跳绳", "挥手", "激动", "街舞", "献吻", "左太极", "右太极",
}

for i in ipairs(qq_emoji_table) do
   qq_emoji_table[qq_emoji_table[i]] = i;
end

if package.config:sub(1, 1) == '/' then
   shell_quote = function (str)
      return "'" .. string.gsub(str, "'", "'\\''") .. "'"
   end
   debug_set_x = "set -x; "
   adb_unquoter = '\\"'
else -- windows
   shell_quote = function (str)
      str = str:gsub('\n', '')
      str = str:gsub('\\', '\\\\')
      str = str:gsub('"', '\\"')
      return '"' .. str .. '"'
   end
   debug_set_x = ""
   adb_unquoter = '\\"'
   is_windows = true
   the_true_adb = ".\\the-true-adb"
end


emoji_for_qq = function(text)
   local s = 1
   local replace = ""
   repeat
      local fs, fe = text:find("%[.-%]", s)
      if fs then
         local emoji = text:sub(fs + 1, fe - 1)
         if qq_emoji_table[emoji] then
            replace = replace .. text:sub(s, fs - 1)
            local idx = qq_emoji_table[emoji]
            replace = replace .. qq_emojis[idx]
            s = fe + 1
         else
            replace = replace .. text:sub(s, fs)
            s = fs + 1
         end
      else
         replace = replace .. text:sub(s)
         break
      end
   until s > #text
   return replace
end

local function system(cmds)
   if type(cmds) == 'string' then
      return os.execute(cmds)
   elseif type(cmds) == 'table' then
      command_str = ''
      for i = 1, #cmds do
         if i == 1 and is_windows then
            command_str = command_str .. cmds[i] .. ' '
         else
            command_str = command_str .. shell_quote(cmds[i]) .. ' '
         end
      end
      return os.execute(debug_set_x .. command_str)
   end
end

debug = function(fmt, ...)
   print(string.format(fmt, ...))
end

local function split(pat, str, allow_null)
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
         elseif allow_null then
            list[#list + 1] = ""
         end
      elseif #str >= start then
         list[#list + 1] = str:sub(start)
      elseif allow_null then
         list[#list + 1] = ""
      end
      if i then
         start = j + 1
      else
         break
      end
   end
   return list
end

local function replace_img_with_emoji(text, html)
   debug("text is %s, html is %s", text, html)
   local texts = split("￼", text, true)
   for k, v in pairs(texts) do
      print(k, v)
   end
   local n = 2
   local res = texts[1]
   for emoji in html:gmatch('img src="(.-)"') do
      debug("emoji is %s", emoji)
      if not emojis then
         emojis = require"emojis"
         emojis_map = {}
         for k, v in ipairs(emojis) do
            emojis_map[v[3]] = v[1]
         end
      end
      emoji = emojis_map[emoji] or "[unknown emoji]"
      res = res .. emoji
      if texts[n] then
         res = res .. texts[n]
      end
      n = n + 1
   end
   debug("res is %s", res)
   return res
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
            quoted_cmds[i] = adb_unquoter .. quoted_cmds[i] .. adb_unquoter
         end
         command_str = command_str .. quoted_cmds[i] .. ' '
      end
      return func(debug_set_x .. the_true_adb .. " shell " .. command_str)
   end
end

local function adb_shell(cmds)
   return adb_do(os.execute, cmds)
end

local function adb_pipe(cmds)
   local pipe = adb_do(io.popen, cmds)
   if not pipe then
      return ""
   end
   local out = pipe:read('*a')
   if not out then
      return ""
   end
   return out:gsub("\r", "")
end

adb_is_window = function (w)
   return w == adb_focused_window()
end

adb_start_activity = function(a)
   adb_shell("am start -n " .. a)
end

adb_focused_window = function()
   local wdump = adb_pipe{"dumpsys", "window"}
   local match = string.match(wdump, "mFocusedWindow[^}]*%s(%S+)}")
   if match then
      return match
   end
   match = wdump:match("mTopFullscreenOpaqueWindowState=Window.-(%S+)%s+paused=false}")
   if match then
      return match
   end
   if check_phone() or true then
      return adb_focused_window()
   end
   error("Can't find focused window: " .. wdump:sub(1, 20))
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
      if not events[i] then
         if not events[i - 1] then
            debug("Error at i = %d, events: %s", i, join(' ', events))
            error("Error: wrong number of events?")
         else
            break
         end
      end

      if tonumber(events[i]) then
         local add = ('input tap %d %d;'):format(events[i] * width_ratio, events[i+1] * height_ratio)
         command_str = command_str .. add
         i = i + 2
      elseif events[i] == 'tap2' or events[i] == 'adb-tap-2' then
         i = i + 1
         local add = ('input tap %d %d;'):format(events[i] * width_ratio, events[i+1] * height_ratio)
         command_str = command_str .. add .. add
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
            events[i+1] * width_ratio, events[i+2] * height_ratio,
            events[i+1] * width_ratio, events[i+2] * height_ratio, ms)
         if sdk_version < 17 then
            add = add:gsub("touchscreen ", "")
         end
         command_str = command_str .. add

         if sdk_version < 18 then
            command_str = command_str .. add
         end
         i = i + 3
      elseif events[i] == 'key' or events[i] == 'adb-key' then
         command_str = command_str .. ('input keyevent %s;'):format(events[i+1]:upper())
         i = i + 2
      elseif events[i] == 'sleep' then
         command_str = command_str .. ('sleep %s || busybox sleep %s;'):format(events[i+1], events[i+1])
         i = i + 2
      elseif events[i] == 'swipe' or (events[i]):match('adb%-swipe%-') then
         ms = 500
         if (events[i]):match('adb%-swipe%-') then
            ms = (events[i]):sub(#'adb-swipe-' + 1)
         end
         if sdk_version < 18 then
            ms = ""
         end

         local add = ('input touchscreen swipe %d %d %d %d %s;'):format(
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
      elseif events[i] == 'adb-tap' then
         i = i + 1
      else
         error(string.format("Error: unknown event: %d: '%s' (%s)", i, events[i], join(' ', events)))
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
   adb_event(("sleep %s"):format(time))
end

local function weibo_text_share(window)
   if window == "com.sina.weibo/com.sina.weibo.DetailWeiboActivity" then
      repost = select_args{'repost', 'comment'}
      if repost == 'repost' then
         adb_tap_bot_left()
      else
         adb_tap_mid_bot()
      end
      sleep(.5)
   end
   local input_method, ime_height = adb_get_input_window_dump()
   if ime_height ~= 0 then
      adb_event("key back")
   end
   if using_scroll_lock then
      adb_event{'key', 'scroll_lock', 991, 166}
   elseif using_smartisan_os then
      adb_event("adb-tap 24 308 adb-key SPACE adb-long-press-800 17 294 adb-tap 545 191 adb-tap 991 166")
   elseif using_xiaomi_os then
      adb_event("adb-tap-2 24 308 sleep .1 adb-tap 77 179 adb-tap 991 166")
   elseif using_oppo_os then
      adb_event("adb-long-press-800 34 312 adb-tap 72 149 adb-tap 991 166")
   else
      adb_event("adb-key space adb-long-press-800 17 294 adb-tap-2 991 166")
   end

end

local function t1_share_to_weibo(text)
   adb_shell{"am", "start", "-n", "com.sina.weibo/com.sina.weibo.EditActivity"}
   if text then putclip(text) else sleep(1) end
   t1_post()
end

adb_top_window = function()
   -- dumpsys window|grep mFocusedWindow|perl -npe 's/.*?(\S+)}$/$1/')
   local adb_window_dump = adb_pipe("dumpsys window")
   if not adb_window_dump then return nil end
   local focused_line = adb_window_dump:match("mFocusedWindow=.-}")
   if not focused_line then return nil end
   local top_window = focused_line:match("%S+}$")
   if not top_window then return nil end
   return top_window:sub(1, -2)
end

adb_start_weixin_share = function(text_or_image)
   if using_adb_root then
      if text_or_image == 'text' then
         adb_shell("am start -n com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsCommentUI --ei sns_comment_type 1")
      elseif text_or_image == 'image' then
         adb_shell("am start -n com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI")
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

   adb_shell("am start -n com.tencent.mm/com.tencent.mm.ui.LauncherUI")
   for i = 1, 3 do
      if adb_top_window() ~= "com.tencent.mm/com.tencent.mm.ui.LauncherUI" then
         adb_event("adb-tap 88 170 sleep " .. (.2 * i))
         adb_shell("am start -n com.tencent.mm/com.tencent.mm.ui.LauncherUI")
      else
         adb_event("adb-tap-2 88 170")
         break
      end
   end
   adb_event("adb-tap 654 1850 sleep .1 adb-tap 332 358 sleep .2 " .. click .. " 961 160")
   if text_or_image == 'image' then
      adb_event("adb-tap 213 929") -- choose picture
   end
end

local function t1_share_to_weixin(text)
   adb_start_weixin_share('text')
   if text then
      text = text:gsub("\n", "​\n")
      putclip(text)
   else
      sleep(1)
   end
   t1_post()
end

local function weixin_text_share(window, text)
   if text then
      text = text:gsub("\n", "​\n")
   end
   if using_scroll_lock then
      adb_event{'key', 'scroll_lock', 961, 171}
   elseif using_smartisan_os then
      adb_event(
         [[
               adb-key SPACE
               adb-tap
               adb-tap 117 283 adb-tap 117 283 adb-tap 325 170 adb-tap 860 155 adb-tap 961 171
      ]])
   elseif using_xiaomi_os then
      adb_event("adb-long-press-800 422 270 adb-tap 147 213 adb-tap 1007 134")
   elseif using_oppo_os then
      adb_event("adb-tap 87 312 adb-tap 92 156 adb-tap 947 132")
   else
      adb_event("adb-key space adb-long-press-800 111 369 adb-tap 97 265 adb-tap 991 166")
   end
end

local function t1_sms(window)
   if using_scroll_lock then
      adb_event{182, 1079, 'key', 'scroll_lock', 864, 921}
   else
      local input_method, ime_height = adb_get_input_window_dump()
      if ime_height == 0 then
         adb_event("adb-tap 182 1079 sleep .8")
      end

      local y_double_click = 928
      local y_paste = 811
      local y_send = y_double_click

      adb_event(
         ([[
                  adb-long-press-800 522 %d
                  adb-tap 149 %d
                  adb-tap 919 %d
         ]]):format(y_double_click, y_paste, y_send)
      )
   end
end

local function t1_google_plus(window)
   if using_scroll_lock then
      adb_event{467, 650, 'key', 'scroll_lock', 932, 1818}
   else
      adb_event(
         [[
               adb-tap 233 503
               sleep .5
               adb-tap 571 1821
               adb-tap 571 1821

      ]])

      local input_method, ime_height = adb_get_input_window_dump()
      if ime_height ~= 0 then
         adb_event("key back")
      end
      adb_event(
         [[
               adb-tap-2 105 464
               adb-tap 286 259
               adb-tap 875 255
               adb-tap 922 1819
         ]]
      )
   end
end

local function t1_smartisan_notes(window)
   if using_scroll_lock then
      adb_event{'key', 'scroll_lock', 940, 140, 933, 117, 323, 1272, 919, 123}
   else
      adb_event(
         [[
                            adb-long-press 428 412
                            adb-tap 80 271
                            adb-tap 940 140
                            adb-tap 933 117
                            adb-tap 323 1272
                            adb-tap 919 123
         ]]
      )
   end
end

local function t1_mail(window)
   if window == 'com.android.email/com.android.email.activity.Welcome' or window == 'com.android.email/com.android.email2.ui.MailActivityEmail' then
      adb_tap_mid_bot()
      sleep(2)
   end
   if using_scroll_lock then
      adb_event{'key', 'scroll_lock'}
   else

      local input_method, ime_height = adb_get_input_window_dump()
      local virtual_key_ratio = app_height / init_height
      local ime_height_diff = ime_height / (init_height / default_height) - ime_height_ref
      local y_start_scroll = 1022 / virtual_key_ratio - ime_height_diff

      adb_event(
         ([[
               adb-swipe-300 586 %d 586 68
               adb-tap 560 1840
               adb-tap-2 299 299
               adb-tap 505 192
         ]]):format(y_start_scroll)
      )
   end
   if window == 'com.google.android.gm/com.google.android.gm.ComposeActivityGmail' then
      adb_event{806, 178}
   else
      adb_event("sleep .1 adb-tap 998 174")
   end
end

local function t1_paste()
   if using_scroll_lock then
      adb_event{'key', 'scroll_lock'}
   else
      return "无法在此窗口内贴粘"
   end
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

adb_get_input_window_dump = function()
   -- $(adb dumpsys window | perl -ne 'print if m/^\s*Window #\d+ Window\{[a-f0-9]+.*\SInputMethod/i .. m/^\s*mHasSurface/')
   local dump = adb_pipe{'dumpsys', 'window'}
   local input_method = {}
   local started = false
   dump = split("\n", dump)
   for i = 1, #dump do
      if not started and dump[i]:match("^%s*Window #?%d* ?Window{[a-f0-9]+.*%sInputMethod") then
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
   local ime_xy = last(string.gmatch(input_window_dump, "Requested w=%d+ h=%d+"))
   local ime_height = 0
   if input_method and ime_xy:match('Requested w=%d+ h=') then
      ime_height = ime_xy:match('Requested w=%d+ h=(%d+)')
      if tonumber((ime_height - (init_height - app_height)) * default_height / init_height ) >= 1200 then -- new version of google pinyin ime?
         if input_window_dump:match('package=com.google.android.inputmethod.pinyin') then
            ime_height = (1920 - 1140) * init_height / default_height + (init_height - app_height)
         elseif input_window_dump:match('package=com.google.android.inputmethod.latin') then
            ime_height = 800 * init_height / default_height + (init_height - app_height)
         end
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

check_phone = function()
   if not adb_pipe("uname || busybox uname"):match("Linux") then
      error("Error: can't put text on phone, not connected?")
   end
end

putclip = function(text)
   if not text and os.getenv("PUTCLIP_ANDROID_FILE") then
      local file = io.open(os.getenv("PUTCLIP_ANDROID_FILE"))
      text = file:read("*a")
      file:close()
      local window = adb_focused_window()
      if window:match("com.tencent.mobileqq") then
         text = emoji_for_qq(text)
      end
   end
   local file, path
   local tmp = os.getenv("TEMP") or "/tmp"
   path = tmp .. package.config:sub(1, 1) .. "lua-smartisan-t1.txt"
   file = io.open(path, "w")
   if not file then
      error("TEMP env not set")
   end
   file:write(text)
   file:close()
   check_phone()
   system{the_true_adb, 'push', path, '/sdcard/putclip.txt'}
   adb_shell(
      [[
               am startservice --user 0 -n com.bhj.setclip/.PutClipService&
               for x in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do
                  if test -e /sdcard/putclip.txt; then
                     sleep .1 || busybox sleep .1;
                     echo $x;
                  else
                     exit;
                 fi;
              done
      ]])
end

t1_config = function()
   -- install the apk
   system(the_true_adb .. " devices")
   local uname = adb_pipe("uname || busybox uname")
   if not uname:match("Linux") then
      local home = os.getenv("HOME")
      if is_windows then
         home = os.getenv("USERPROFILE")
      end
      if not home or home == "" or home == "/" then
         error("Your HOME environment variable is not set up correctly: '" .. home .. "'")
      end
      local dot_android = home .. package.config:sub(1, 1) .. ".android"
      if is_windows then
         system{"md", dot_android}
      else
         system{"mkdir", "-p", dot_android}
      end
      local ini = dot_android .. package.config:sub(1, 1) .. "adb_usb.ini"
      local ini_file = io.open(ini, "r")
      local done_vid = true
      if not ini_file then
         done_vid = false
      else
         local ini_lines = ini_file:read("*a")
         if ini_lines ~= "0x29a9\n" then
            done_vid = false
         end
         ini_file:close()
      end
      if not done_vid then
         local err
         ini_file, err = io.open(ini, "w")
         if not ini_file then
            error("can't open " .. ini .. ": " .. err)
         end
         ini_file:write("0x29a9\n")
         ini_file:close()
         system{the_true_adb, "kill-server"}
         error("Done config for your adb devices, please try again")
      else
         error("No phone found, can't set up, uname is: " .. uname)
      end
   end
   local setclip_phone_md5 = adb_pipe("cat /sdcard/t1wrench-setclip.md5")
   local md5file = io.open("setclip.apk.md5")
   local setclip_local_md5 = md5file:read("*a")
   io.close(md5file)
   debug("on phone: %s, local: %s", setclip_phone_md5, setclip_local_md5)
   if setclip_phone_md5 ~= setclip_local_md5 then
      local install_output = io.popen(the_true_adb .. " install -r SetClip.apk"):read("*a")
      if install_output:match("\nSuccess\r?\n") then
         system(the_true_adb .. " push setclip.apk.md5 /sdcard/t1wrench-setclip.md5")
         local setclip_phone_md5 = adb_pipe("cat /sdcard/t1wrench-setclip.md5")
         local md5file = io.open("setclip.apk.md5")
         local setclip_local_md5 = md5file:read("*a")
         io.close(md5file)
         if setclip_phone_md5 ~= setclip_local_md5 then
            error("Can't mark the setclip.apk as been installed")
         end
      else
         error("Install setclip.apk failed, output is " .. install_output)
      end
   end

   sdk_version = adb_pipe("getprop ro.build.version.sdk")
   brand = adb_pipe("getprop ro.product.brand"):gsub("\n.*", "")
   model = adb_pipe("getprop ro.product.model"):gsub("\n.*", "")

   debug("sdk is %s\nbrand is %s\nmodel is %s\n", sdk_version, brand, model)
   sdk_version = tonumber(sdk_version)
   if tonumber(sdk_version) < 16 then
       error("Error, you phone's sdk version is " .. sdk_version .. ",  must be at least 16")
   end
   local dump = adb_pipe{'dumpsys', 'window'}
   init_width = dump:match('init=(%d+x%d+)')
   init_height = tonumber(init_width:match('x(%d+)'))
   init_width = tonumber(init_width:match('(%d+)x'))

   app_width = dump:match('app=(%d+x%d+)')
   app_height = app_width:match('x(%d+)')
   app_width = app_width:match('(%d+)x')
   width_ratio, height_ratio = app_width / default_width,  app_height / default_height


   if brand:match("smartisan") then
      using_smartisan_os = true
   else
      using_smartisan_os = false
   end

   if brand:match("Xiaomi") then
      using_xiaomi_os = true
   elseif brand:match("OPPO") then
      using_oppo_os = true
   end

   local id = adb_pipe("id")
   if id:match("uid=0") then
      using_adb_root = true
   else
      using_adb_root = false
   end

   local scroll = adb_pipe("getprop persist.smartisan.pastetool")
   if scroll:match("1") then
      debug("pastetool is true")
      using_scroll_lock = true
   else
      using_scroll_lock = false
      debug("pastetool is false")
   end
   return ("brand is %s, paste is %s"):format(brand, using_scroll_lock)
end

get_a_note = function(text)
   if text then
      putclip(text)
   end
   adb_shell("am start -n com.smartisanos.notes/com.smartisanos.notes.NotesActivity")
   adb_event(
      [[
            sleep .1
            adb-tap-2 71 162
            adb-tap 941 163
   ]])
   if using_scroll_lock then
      adb_event("sleep .5 key scroll_lock sleep .4")
   else
      adb_event(
         [[
                adb-long-press-800 226 440
                adb-tap 106 258
         ]])
      end
   adb_event(
      [[
            adb-tap 934 155
            adb-tap 984 149
            adb-tap 409 1256
            adb-tap 906 207
   ]])
   for i = 1, 10 do
      local window = adb_focused_window()
      if window == 'com.smartisanos.notes/com.smartisanos.notes.LongLengthWeiboActivity' then
         sleep(.1 * i)
      end
   end
   adb_event(
      [[
            adb-tap-2 71 162
            adb-key BACK
   ]])
   adb_get_last_pic('notes', true)
end

adb_get_last_pic = function(which, remove)
   if which == 'notes' then
      local dir = '/sdcard/smartisan/Notes'
      local ls_out1 = adb_pipe("busybox ls -t -1 " .. dir)
      ls_out1 = ls_out1:gsub("\n.*", "")
      ls_out1 = ls_out1:gsub("\x1b.-m", "")
      ls_out1 = ls_out1:gsub("%?+", "*")

      if ls_out1:match('%*') then
         ls_out1 = adb_pipe(('bash -c "ls %s/%s"'):format(dir, ls_out1))
         ls_out1 = ls_out1:gsub("\n", "")
         ls_out1 = ls_out1:gsub(".*/", "")
      end

      system{the_true_adb, "pull", ("%s/%s"):format(dir, ls_out1), ("last-pic-%s.png"):format(which)}
      if remove then
         system{the_true_adb, "shell", "rm", ("%s/%s"):format(dir, ls_out1)}
         adb_shell(("am startservice --user 0 -n com.bhj.setclip/.PutClipService --es picture %s/%s"):format(dir, ls_out1))
      end
   end
end

t1_post = function(text) -- use weixin
   local window = adb_focused_window()
   if text then
      if window:match("com.tencent.mobileqq") then
         putclip(emoji_for_qq(text))
      else
         putclip(text)
      end
   end
   if window then print("window is " .. window) end
   if window == "com.sina.weibo/com.sina.weibo.EditActivity" or window == "com.sina.weibo/com.sina.weibo.DetailWeiboActivity" or window == "com.immomo.momo/com.immomo.momo.android.activity.feed.PublishFeedActivity" then
      weibo_text_share(window)
      return
   elseif window == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI" or window == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsCommentUI" then
      weixin_text_share(window, text)
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
      window == "com.android.email/com.android.email2.ui.MailActivityEmail" or
   window == "com.android.email/com.android.email.activity.ComposeActivityEmail" then
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

      if using_scroll_lock then
         adb_event(string.format("%s key scroll_lock %s", add, post_button))
      else
         if not input_method then
            adb_event(
               [[
                     adb-tap 560 1840
                     sleep .1
               ]]
            )
         end

         local input_method, ime_height = adb_get_input_window_dump()
         local virtual_key_ratio = app_height / init_height
         local ime_height_diff = ime_height / (init_height / default_height) - ime_height_ref
         local y_double_click = 951 / virtual_key_ratio - ime_height_diff
         local y_select_all = 862 / virtual_key_ratio - ime_height_diff
         local y_paste = y_select_all
         local y_send = (945 - ((default_height - init_height) / 70 + (init_height - app_height - 44) / 22)) / virtual_key_ratio - ime_height_diff

         if using_smartisan_os then
            adb_event(
               ([[
                adb-tap 560 1840 adb-tap-2 560 %d adb-tap 296 %d adb-tap 888 %d adb-tap 976 %d
            ]]):format(y_double_click, y_select_all, y_paste, y_send)
            )
         elseif using_xiaomi_os then
            adb_event(
               ([[
                        adb-tap 560 1840 adb-long-press-800 560 %d adb-tap 310 %d adb-tap 501 %d adb-tap 976 %d
               ]]):format(y_double_click, y_select_all, y_paste, y_send)
            )
         elseif using_oppo_os then
            adb_event(
               ([[
                        adb-tap 560 1840 adb-long-press-800 560 %d adb-tap 263 %d sleep .1 adb-tap 452 %d adb-tap 976 %d
               ]]):format(y_double_click, y_select_all, y_paste, y_send)
            )
         else
            debug("not using smartisan os")
            adb_event(
               ([[
                        adb-tap 560 1824 adb-long-press-800 353 %d adb-tap 220 %d adb-tap 995 %d
               ]]):format(y_double_click, y_paste, y_send)
            )
         end
      end
   end
   return "text sent"
end

local function upload_pics(...)
   local pics = {...}
   adb_shell(
      [[
            for x in /sdcard/DCIM/Camera/t1wrench-*; do
               if test -e "$x"; then
                  rm -f "$x";
                  am startservice --user 0 -n com.bhj.setclip/.PutClipService --es picture "$x";
               fi;
            done
   ]])

   print("hello world")
   local targets = {}
   time = os.time()
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = ('/sdcard/DCIM/Camera/t1wrench-%d-%d%s'):format(time, i, ext)
      targets[#targets + 1] = target
      system{the_true_adb, 'push', pics[i], target}
      adb_shell{"am", "startservice", "--user", "0", "-n", "com.bhj.setclip/.PutClipService", "--es", "picture", target}
   end
   return targets
end

picture_to_weixin_share = function(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]

      if i == 1 then
         adb_start_weixin_share('image')
         if using_adb_root then
             adb_event("sleep .5 adb-tap 141 597 sleep .5")
         else
             sleep(.5)
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
   adb_event("adb-tap 901 1841 adb-tap 75 1867 adb-tap 903 133")
   return "Prompt: please say something"
end

local function picture_to_weibo_share_upload(...)
   local pics = upload_pics(...)
   picture_to_weibo_share(pics)
end

local function picture_to_momo_share_upload(...)
   local pics = upload_pics(...)
   picture_to_momo_share(pics)
end

local function picture_to_weixin_share_upload(...)
   local pics = upload_pics(...)
   picture_to_weixin_share(pics)
end

picture_to_weibo_share = function(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]

      if i == 1 then
         adb_shell("am start -n com.sina.weibo/com.sina.weibo.EditActivity")
         adb_event("sleep .5")
         local input_method, ime_height = adb_get_input_window_dump()
         if ime_height ~= 0 then
            adb_event("key back")
         end
         adb_event("sleep 1 adb-tap 62 1843 sleep 2")
      end

      local pic_share_buttons = {
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(i_button)
   end
   adb_event("adb-tap 141 1849 adb-tap 922 1891")
end

picture_to_momo_share = function(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]

      if i == 1 then
         local momoStart = "com.immomo.momo/com.immomo.momo.android.activity.WelcomeActivity"
         local momoActivity = "com.immomo.momo/com.immomo.momo.android.activity.maintab.MaintabActivity"
         adb_start_activity(momoStart)
         for try = 1, 5 do
            adb_event("sleep .2")
            if not adb_is_window(momoActivity) then
               adb_event("key back")
               adb_start_activity(momoStart)
            end
         end
         adb_event("sleep .2 adb-tap 288 1821 adb-tap 261 337 adb-tap 972 165 sleep 1")
      end

      local pic_share_buttons = {
         "adb-tap 141 408", "adb-tap 387 368", "adb-tap 689 360", "adb-tap 913 324",
         "adb-tap 142 687", "adb-tap 379 612",
      }
      local i_button = pic_share_buttons[i]
      adb_event(i_button)
   end
   adb_event("adb-tap 404 1854 adb-tap 201 534")
end

local function picture_to_weixin_chat(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = adb_get_input_window_dump()
   if (ime_height ~= 0) then
       ime_height = 0
       adb_event("key back")
   end
   local post_button = ('984 %d'):format(1920 - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]
      if i == 1 then
         local events = post_button .. [[
             sleep .1
             adb-tap 125 1285
             sleep .1
         ]]
         adb_event(events)
         if adb_focused_window() ~= "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.AlbumPreviewUI" then
            adb_event("125 1285")
         end
         sleep(1)
      end

      local pic_share_buttons = {
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(i_button)
   end
   adb_event("adb-tap 944 1894 adb-tap 59 1871 adb-tap 927 148")
end

local function picture_to_qq_chat(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = adb_get_input_window_dump()
   if (ime_height ~= 0) then
       ime_height = 0
       adb_event("key back")
   end
   local post_button = ('159 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]
      if i == 1 then
         local events = post_button .. " sleep .1 adb-tap 203 1430 sleep .1"
         adb_event(events)
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
      adb_event(i_button)
   end
   adb_event("adb-tap 608 1831 adb-tap 403 1679 adb-tap 918 1862 sleep .5 adb-tap 312 1275")
end

local function picture_to_qqlite_chat(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = adb_get_input_window_dump()
   if (ime_height ~= 0) then
       ime_height = 0
       adb_event("key back")
   end
   local post_button = ('984 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]
      if i == 1 then
         local events = post_button .. " sleep .1 adb-tap 203 1430 sleep .1"
         adb_event(events)
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
      adb_event(i_button)
   end
   adb_event("adb-tap 519 1841 adb-tap 434 1071 adb-tap 918 1862 sleep .5 adb-tap 279 1221")
end

local function picture_to_weibo_chat(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = adb_get_input_window_dump()
   if (ime_height ~= 0) then
       ime_height = 0
       adb_event("key back")
   end
   local post_button = ('984 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]
      if i == 1 then
         local events = post_button .. " sleep .1 adb-tap 375 1410 sleep .1 adb-tap 645 135 sleep .2 adb-tap 369 679 sleep 2"
         adb_event(events)
      end
      local pic_share_buttons = {
         "adb-tap 614 281", "adb-tap 1000 260", "adb-tap 268 629",
         "adb-tap 652 645", "adb-tap 1004 632", "adb-tap 301 1008",
         "adb-tap 612 996", "adb-tap 1006 992", "adb-tap 265 1346",
      }
      local i_button = pic_share_buttons[i]
      adb_event(i_button)
   end
   adb_event("adb-tap 943 1868 adb-tap 194 1163")
end

local function t1_picture(...)
   local pics = upload_pics(...)
   local window = adb_focused_window()
   if window == "com.tencent.mm/com.tencent.mm.ui.LauncherUI" then
      picture_to_weixin_chat(pics)
   elseif window == "com.tencent.mm/com.tencent.mm.ui.chatting.ChattingUI" then
      picture_to_weixin_chat(pics)
   elseif window == "com.tencent.qqlite/com.tencent.mobileqq.activity.ChatActivity" then
      picture_to_qqlite_chat(pics)
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity" then
      picture_to_qq_chat(pics)
   elseif window == "com.sina.weibo/com.sina.weibo.weiyou.DMSingleChatActivity" then
      picture_to_weibo_chat(pics)
   elseif window:match("com.sina.weibo") then
      picture_to_weibo_share(pics)
   elseif window:match("com.tencent.mm") then
      picture_to_weixin_share(pics)
   else
      return "Error: can't decide where to share"
   end
   return #pics .. " pictures sent"
end

local function t1_follow_me()
   check_phone()
   -- http://weibo.com/u/1611427581 (baohaojun)
   -- http://weibo.com/u/1809968333 (beagrep)
   adb_shell{"am", "start", "-n", "com.sina.weibo/.ProfileInfoActivity", "--es", "uid", "1611427581"}
   if init_width < 720 then
      adb_event("sleep 1 adb-tap 659 950 key back")
   else
      adb_event("sleep 1 adb-tap 659 870 key back")
   end
end

local function t1_spread_it()
   check_phone()
   -- http://weibo.com/1611427581/Bviui9tzF
   -- http://weibo.com/1611427581/BvnNk2PwH?from=page_1005051611427581_profile&wvr=6&mod=weibotime&type=comment
   -- http://m.weibo.cn/1809968333/3774599487375417
   adb_shell{"am", "start", "sinaweibo://detail?mblogid=BvnNk2PwH"}
   adb_event("sleep 1 adb-tap 911 1863 adb-tap 156 1876 sleep .1")
   if using_smartisan_os then
      t1_post("#如果别人认为你还没有疯，那只是因为你还不够努力😼#")
   elseif brand:match("Xiaomi") then
      t1_post("我在小米手机上用Smartisan T1小扳手，赞！下一台手机考虑换Smartisan T1吧😼")
   else
     t1_post(("我在%s的%s手机上用Smartisan T1小扳手，赞！下一台手机考虑换Smartisan T1吧😼"):format(brand, model))
   end
end

local M = {}
M.putclip = putclip
M.t1_post = t1_post
M.adb_shell = adb_shell
M.adb_pipe = adb_pipe
M.t1_picture = t1_picture
M.t1_follow_me = t1_follow_me
M.t1_share_to_weibo = t1_share_to_weibo
M.t1_share_to_weixin = t1_share_to_weixin
M.picture_to_weibo_share = picture_to_weibo_share_upload
M.picture_to_weixin_share = picture_to_weixin_share_upload
M.t1_spread_it = t1_spread_it
M.adb_start_weixin_share = adb_start_weixin_share
M.t1_config = t1_config
M.emoji_for_qq = emoji_for_qq
M.split = split
M.replace_img_with_emoji = replace_img_with_emoji
M.system = system
M.debug = debug
M.get_a_note = get_a_note
M.adb_get_last_pic = adb_get_last_pic
M.picture_to_momo_share = picture_to_momo_share_upload

local function do_it()
   if arg and type(arg) == 'table' and string.find(arg[0], "t1wrench.lua") then
      -- t1_post(join(' ', arg))
      local file = io.open("setclip.apk.md5")
      if file then
         t1_config()
         file:close()
      end
      if type(M[arg[1]]) == 'function' then
         _G.M = M
         cmd = "M[arg[1]]("
         for i = 2, #arg do
            if i ~= 2 then
               cmd = cmd .. ', '
            end

            cmd = cmd .. "arg[" .. i .. "]"
         end
         cmd = cmd .. ")"
         debug("cmd is %s", cmd)
         loadstring(cmd)()
      end
      os.exit(0)
      t1_picture(arg[1]) -- , arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9])
      os.exit(0)
      print(5)
      debug_set_x = arg[#arg]
      arg[#arg] = nil
      adb_shell(arg)
   else
      return M
   end
end
qq_emojis = {
[[]], [[(]], [[]], [[+]], [[]], [[	]], [[]], [[j]],
[[#]], [[ú]], [[]], [[]], [[]], [[ ]], [[!]], [[ ]],
[[]], [[]], [[]] .. "\r", [[]], [[]], [[]], [[]], [[]],
[[Q]], [[R]], [[]] .. "\x1a", [[]], [[%]], [[2]], [[*]], [[S]],
[["]], [[]], [[1]], [[T]], [[']], [[N]], [[]], [[]],
[[]], [[U]], [[V]], [[W]], [[.]], [[X]], [[,]],
[[Y]], [[0]], [[]], [[Z]], [[)]], [[$]], [[[]], [[3]],
[[]], [[<]], [[=]], [[\]], [=[]]=], [[B]], [[:]], [[]],
[[]], [[9]], [[]], [[]], [[J]], [[;]], [[P]], [[]],
[[F]], [[M]], [[>]], [[]], [[D]], [[K]], [[L]], [[-]],
[[4]], [[5]], [[6]], [[7]], [[8]], [[?]], [[I]], [[H]],
[[A]], [[^]], [[@]], [[&]], [[/]], [[_]], [[G]], [[`]],
[[a]], [[b]], [[c]], [[d]], [[O]], [[e]], [[f]], [[g]],
[[h]], [[i]], [[l]], [[m]], [[n]], [[o]], [[p]], [[q]],
[[r]], [[s]], [[t]], [[u]], [[v]], [[w]], [[x]], [[y]], [[z]]
}

return do_it()

-- Local variables:
-- coding: utf-8
-- End:
