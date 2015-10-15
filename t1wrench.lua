#!/usr/bin/lua

-- module
local M

-- functions
local t1_call, t1_run, t1_adb_mail, t1_save_mail_heads
local shell_quote, putclip, t1_post, push_text
local adb_start_activity
local picture_to_weixin_share, picture_to_weibo_share
local picture_to_momo_share, t1_add_mms_receiver
local adb_get_input_window_dump, adb_top_window
local adb_start_weixin_share, adb_is_window
local adb_focused_window
local t1_config, check_phone
local emoji_for_qq, debug, get_a_note, emoji_for_weixin, emoji_for_qq_or_weixin
local adb_get_last_pic, debugging
local adb_weixin_lucky_money
local adb_weixin_lucky_money_output
local t1_find_weixin_contact
local adb_start_service_and_wait_file_gone
local adb_start_service_and_wait_file, adb_am
local wait_input_target, wait_top_activity, wait_top_activity_match
local t1_eval, log, share_pics_to_app, share_text_to_app
local picture_to_weibo_comment
local check_scroll_lock

-- variables
local where_is_dial_key
local rows_mail_att_finder
local UNAME_CMD = "uname || busybox uname || { echo -n Lin && echo -n ux; }"
local is_debugging = false
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
local using_oppo_os = false
local brand = "smartisan"
local model = "T1"
local qq_emojis, weixin_emojis
local sdk_version = 19
local emojis, emojis_map
local the_true_adb = "./the-true-adb"
local t1_send_action
local weixinAlbumPreviewActivity = "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.AlbumPreviewUI"
local weixinChatActivity = "com.tencent.mm/com.tencent.mm.ui.chatting.ChattingUI"
local weixinLauncherActivity = "com.tencent.mm/com.tencent.mm.ui.LauncherUI"
local weixinSnsUploadActivity = "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI"
local weixinImagePreviewActivity = "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.ImagePreviewUI"
local weiboShareActivity = "com.sina.weibo/com.sina.weibo.composerinde.OriginalComposerActivity"
local weiboCommentActivity = "com.sina.weibo/com.sina.weibo.composerinde.CommentComposerActivity"
local weiboForwardActivity = "com.sina.weibo/com.sina.weibo.composerinde.ForwardComposerActivity"
local qqChatActivity = "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity"
local qqChatActivity2 = "com.tencent.mobileqq/com.tencent.mobileqq.activity.SplashActivity"
local qqPhotoFlow = "com.tencent.mobileqq/com.tencent.mobileqq.activity.photo.PhotoListFlowActivity"
local qqPhotoPreview = "com.tencent.mobileqq/com.tencent.mobileqq.activity.photo.PhotoPreviewActivity"
local qqPhoteList = "com.tencent.mobileqq/com.tencent.mobileqq.activity.photo.PhotoListActivity"
local weiboAlbumActivity = "com.sina.weibo/com.sina.weibo.photoalbum.PhotoAlbumActivity"
local weiboImagePreviewActivity = "com.sina.weibo/com.sina.weibo.photoalbum.ImagePagerActivity"
local weiboPicFilterActivity = "com.sina.weibo/com.sina.weibo.photoalbum.PicFilterActivity"
local weiboChatActivity = "com.sina.weibo/com.sina.weibo.weiyou.DMSingleChatActivity"
local notePicPreview = "com.smartisanos.notes/com.smartisanos.notes.Convert2PicturePreviewActivity"

local qq_emoji_table = {
   "微笑", "撇嘴", "色", "发呆", "得意", "流泪", "害羞", "闭嘴",
   "睡", "大哭", "尴尬", "发怒", "调皮", "呲牙", "惊讶", "难过",
   "酷", "冷汗", "抓狂", "吐", "偷笑", "可爱", "白眼", "傲慢",
   "饥饿", "困", "惊恐", "流汗", "憨笑", "大兵", "奋斗", "咒骂",
   "疑问", "嘘", "晕", "折磨", "衰", "骷髅", "敲打", "再见",
   "擦汗", "抠鼻", "鼓掌", "糗大了", "坏笑", "左哼哼", "右哼哼", "哈欠",
   "鄙视", "委屈", "快哭了", "阴险", "亲亲", "吓", "可怜", "菜刀",
   "西瓜", "啤酒", "篮球", "乒乓", "咖啡", "饭", "猪头", "玫瑰",
   "凋谢", "示爱", "爱心", "心碎", "蛋糕", "闪电", "炸弹", "刀",
   "足球", "瓢虫", "便便", "月亮", "太阳", "礼物", "拥抱", "强",
   "弱", "握手", "胜利", "抱拳", "勾引", "拳头", "差劲", "爱你",
   "NO", "OK", "爱情", "飞吻", "跳跳", "发抖", "怄火", "转圈",
   "磕头", "回头", "跳绳", "挥手", "激动", "街舞", "献吻", "左太极",
   "右太极",
}

for i in ipairs(qq_emoji_table) do
   qq_emoji_table[qq_emoji_table[i]] = i;
end

local p = io.popen("the-true-adb version")
local v = p:read("*a")
adb_unquoter = ""
if v:match("1.0.31") then
   adb_unquoter = '\\"'
end

if package.config:sub(1, 1) == '/' then
   shell_quote = function (str)
      return "'" .. string.gsub(str, "'", "'\\''") .. "'"
   end
   if is_debugging then
      debug_set_x = "set -x; "
   else
      debug_set_x = ""
   end
else -- windows
   shell_quote = function (str)
      str = str:gsub('\n', '')
      str = str:gsub('\\', '\\\\')
      str = str:gsub('"', '\\"')
      return '"' .. str .. '"'
   end
   debug_set_x = ""
   is_windows = true
   the_true_adb = ".\\the-true-adb"
end


emoji_for_qq = function(text)
   return emoji_for_qq_or_weixin(text, qq_emojis)
end

emoji_for_qq_or_weixin = function(text, which_emojis)
   local s = 1
   local replace = ""
   repeat
      local fs, fe = text:find("%[.-%]", s)
      if fs then
         local emoji = text:sub(fs + 1, fe - 1)
         if qq_emoji_table[emoji] then
            replace = replace .. text:sub(s, fs - 1)
            if which_emojis == qq_emojis then
               local idx = qq_emoji_table[emoji]
               replace = replace .. which_emojis[idx]
            else
               replace = replace .. "/" .. emoji
            end
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

emoji_for_weixin = function(text)
   return emoji_for_qq_or_weixin(text, weixin_emojis)
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

debugging = function(fmt, ...)
   if is_debugging then
      debug(fmt, ...)
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
   debugging("text is %s, html is %s", text, html)
   local texts = split("￼", text, true)
   for k, v in pairs(texts) do
      print(k, v)
   end
   local n = 2
   local res = texts[1]
   for emoji in html:gmatch('img src="(.-)"') do
      debugging("emoji is %s", emoji)
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
   debugging("res is %s", res)
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
   check_phone()
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
   if is_exiting then
      check_phone()
   end
   if qt_adb_pipe then
      if type(cmds) == 'string' then
         cmds = {'sh', '-c', cmds}
      end
      local quoted_cmds = {}
      for i = 1, #cmds do
         quoted_cmds[i] = shell_quote(cmds[i])
         if string.find(quoted_cmds[i], " ") and adb_unquoter ~= "" then
            quoted_cmds[i] = '"' .. quoted_cmds[i] .. '"'
         end
      end

      return qt_adb_pipe(quoted_cmds)
   end

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
   adb_am("am start -n " .. a)
end

adb_focused_window = function()
   return adb_top_window() or ""
end

adb_am = function(cmd)
   if type(cmd) ~= 'string' then
      cmd = join(' ', cmd)
   end
   if adb_quick_am ~= nil then
      local res = adb_quick_am{cmd}
      if res ~= "input ok\n" then
         debugging("adb_quick_am not ok: %s, redo using adb sell", res)
         adb_shell(cmd)
      end
   else
      adb_shell(cmd)
   end
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
            debugging("Error at i = %d, events: %s", i, join(' ', events))
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
      elseif events[i] == 'text' or events[i] == 'adb-text' then
         command_str = command_str .. ("input text %s;"):format(events[i+1])
         i = i + 2
      elseif events[i] == 'key' or events[i] == 'adb-key' then
         local event = events[i+1]:upper()
         command_str = command_str .. ('input keyevent %s;'):format(event)
         if event == "SCROLL_LOCK" then
            check_scroll_lock()
            command_str = command_str .. "sleep .1;"
         end
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

local function adb_tap_bot_left()
   adb_event{20, 1882}
end

local function adb_tap_mid_bot()
   adb_event{560, 1840}
end

local function sleep(time)
   adb_event(("sleep %s"):format(time))
end

check_scroll_lock = function()
   if using_scroll_lock then
      return
   end

   local input_method, ime_height, ime_connected, current_input_method
   local function using_wrench_ime()
      input_method, ime_height, ime_connected, current_input_method = adb_get_input_window_dump()
      return current_input_method == 'com.wrench.inputmethod.pinyin'
   end

   while not using_wrench_ime() do
      if select_args then
         local prompt_str = ("您的手机系统及当前输入法：%s不支持小扳手，必须使用小扳手输入法才能正常输入，请确保已安装并设置小扳手输入法为当前手机输入法。"):format(current_input_method)
         if select_args{prompt_str} == "" then
            error("您取消了本次操作，请在手机上配置好小扳手输入法后再继续使用小扳手")
         end
      else
         debug("您的手机系统及当前输入法：%s不支持小扳手，必须使用小扳手输入法才能正常输入，请确保已安装并设置小扳手输入法为当前手机输入法", current_input_method)
         sleep(1)
      end
   end
end

local function weibo_text_share(window)
   local repost = '?'
   if window == "com.sina.weibo/com.sina.weibo.DetailWeiboActivity" then
      repost = select_args{'转发还是评论', '转发', '评论', '转发并评论'}
      if repost:match('转发') then
         debugging("doing post")
         adb_tap_bot_left()
      else
         adb_tap_mid_bot()
      end
      sleep(1)
   end
   if repost:match('并') then
      adb_event("sleep .1 adb-tap 57 1704")
   end
   adb_event{'key', 'scroll_lock', 991, 166}
end

local function t1_share_to_weibo(text)
   share_text_to_app("com.sina.weibo", ".composerinde.ComposerDispatchActivity", text)
   wait_input_target(weiboShareActivity)
   t1_send_action()
end

wait_top_activity = function(activity)
   debug("waiting for %s", activity)
   local window
   for i = 1, 20 do
      window = adb_focused_window()
      if window == activity then
         debug("wait ok")
         return window
      end
      sleep(.1)
   end
   return window
end

wait_top_activity_match = function(activity)
   debug("waiting for %s", activity)
   local window
   for i = 1, 20 do
      window = adb_focused_window()
      if window:match(activity) then
         debug("wait ok")
         return window
      end
      sleep(.1)
   end
   return window
end

wait_input_target = function(activity)
   debug("wait for input method for %s", activity)
   for i = 1, 20 do
      local window = adb_focused_window()
      if window:match(activity) then
         local adb_window_dump = split("\n", adb_pipe("dumpsys window"))
         for x = 1, #adb_window_dump do
            if adb_window_dump[x]:match("mInputMethodTarget.*"..activity) then
               local input_method, ime_height, ime_connected = adb_get_input_window_dump()
               if ime_connected then
                  return adb_window_dump[x]
               end
            end
         end
      end
      sleep(.1)
   end
   return ""
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

   adb_am("am start -n " .. weixinLauncherActivity)
   for i = 1, 3 do
      if adb_top_window() ~= weixinLauncherActivity then
         adb_event("adb-tap 88 170 sleep " .. (.2 * i))
         adb_am("am start -n " .. weixinLauncherActivity)
      else
         adb_event("adb-tap-2 88 170")
         break
      end
   end
   adb_event("adb-tap 654 1850 sleep .5 adb-tap 332 358")
   if wait_top_activity("com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsTimeLineUI") == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsTimeLineUI" then
      adb_event("sleep .2 " .. click .. " 961 160")
   else
      log("无法切换到朋友圈界面")
   end
   if text_or_image == 'image' then
      adb_event("adb-tap 213 929") -- choose picture
   end
   adb_event("adb-tap 143 264")
   wait_input_target("com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI")
   log("start weixin share complete")
end

local function t1_share_to_weixin(text)
   debug("share to weixin: %s", text)
   weixinShareActivity = "com.tencent.mm/com.tencent.mm.plugin.sns.ui"
   adb_start_weixin_share('text')
   if text then
      text = text:gsub("\n", "​\n")
      putclip(text)
   end
   wait_input_target(weixinShareActivity)
   t1_post()
end

local function weixin_text_share(window, text)
   if text then
      text = text:gsub("\n", "​\n")
   end
   adb_event("adb-key scroll_lock sleep .2 adb-tap 961 171")
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
      adb_event("sleep .1 adb-tap 998 174")
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

adb_get_input_window_dump = function()
   -- $(adb dumpsys window | perl -ne 'print if m/^\s*Window #\d+ Window\{[a-f0-9]+.*\SInputMethod/i .. m/^\s*mHasSurface/')
   local dump_str = adb_pipe("dumpsys input_method; dumpsys window")
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
      ime_height = ime_xy:match('Requested w=%d+ h=(%d+)')
      if tonumber((ime_height - (init_height - app_height)) * default_height / init_height ) >= 800 then -- new version of google pinyin ime?
         if input_window_dump:match('package=com.google.android.inputmethod.pinyin') then
            ime_height = (1920 - 1140) * init_height / default_height + (init_height - app_height)
         elseif input_window_dump:match('package=com.wrench.inputmethod.pinyin') then
            ime_height = (1920 - 1125) * init_height / default_height + (init_height - app_height)
         elseif input_window_dump:match('package=com.google.android.inputmethod.latin') or
            input_window_dump:match('package=com.android.inputmethod.latin') then
            ime_height = 800 * init_height / default_height + (init_height - app_height)
         end
      end
   end

   local ime_connected = not dump_str:match("mServedInputConnection=null")
   return input_method_active, ime_height, ime_connected, current_input_method
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
   if is_exiting and is_exiting() then
      error("exiting")
   end
end

adb_start_service_and_wait_file_gone = function(service_cmd, file)
   adb_am("am startservice --user 0 -n " .. service_cmd)
   adb_shell(
      (
      [[
            for x in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do
               if test -e %s; then
                  sleep .1 || busybox sleep .1;
               else
                  exit;
               fi;
            done
      ]]):format(file))
end

adb_start_service_and_wait_file = function(service_cmd, file)
   adb_shell(
      (
      [[
            rm %s;
            am startservice --user 0 -n %s&
            for x in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do
               if test ! -e %s; then
                  sleep .1 || busybox sleep .1;
               else
                  exit;
               fi;
            done
      ]]):format(file, service_cmd, file))
end

push_text = function(text)
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
end

putclip = function(text)
   push_text(text)
   adb_start_service_and_wait_file_gone('com.bhj.setclip/.PutClipService', '/sdcard/putclip.txt')
end

local check_file_pushed = function(file, md5)
   local md5_on_phone = adb_pipe("cat /sdcard/" .. md5)
   md5_on_phone = md5_on_phone:gsub("\n", "")
   local md5file = io.open(md5)
   local md5_on_PC = md5file:read("*a")
   md5_on_PC = md5_on_PC:gsub("\n", "")
   io.close(md5file)
   debugging("on phone: %s, local: %s", md5_on_phone, md5_on_PC)
   if md5_on_phone ~= md5_on_PC then
      log("需要把" .. file .. "上传到手机。")
      system(("%s push %s /data/data/com.android.shell/%s.bak"):format(the_true_adb, file, file))
      system(("%s shell mv /data/data/com.android.shell/%s.bak /data/data/com.android.shell/%s"):format(the_true_adb, file, file))

      system(("%s push %s /sdcard/%s"):format(the_true_adb, md5, md5))
      local md5_on_phone = adb_pipe("cat /sdcard/" .. md5)
      md5_on_phone = md5_on_phone:gsub("\n", "")
      if md5_on_phone ~= md5_on_PC then
         error("Can't mark the " .. file .. " as been installed: \n'" .. md5_on_phone .. "'\n : \n'" .. md5_on_PC .. "'")
      else
         log("小扳手辅助文件" .. file .. "上传成功")
      end
   end
end

local check_apk_installed = function(apk, md5)
   local md5_on_phone = adb_pipe("cat /sdcard/" .. md5)
   md5_on_phone = md5_on_phone:gsub("\n", "")
   local md5file = io.open(md5)
   local md5_on_PC = md5file:read("*a")
   md5_on_PC = md5_on_PC:gsub("\n", "")
   io.close(md5file)
   debugging("on phone: %s, local: %s", md5_on_phone, md5_on_PC)
   if md5_on_phone ~= md5_on_PC then
      log("需要在手机上安装小扳手辅助应用" .. apk .. "，请确保手机允许安装未知来源的apk。")
      local install_output = io.popen(the_true_adb .. " install -r " .. apk):read("*a")
      if install_output:match("\nSuccess\r?\n") then
         system(("%s push %s /sdcard/%s"):format(the_true_adb, md5, md5))
         local md5_on_phone = adb_pipe("cat /sdcard/" .. md5)
         md5_on_phone = md5_on_phone:gsub("\n", "")
         if md5_on_phone ~= md5_on_PC then
            error("Can't mark the " .. apk .. " as been installed: \n'" .. md5_on_phone .. "'\n : \n'" .. md5_on_PC .. "'")
         else
            log("小扳手辅助应用" .. apk .. "安装成功")
         end
      else
         if not os.execute("test -e .quiet-apk-install-failure") then
            error("Install " .. apk .. " failed, output is " .. install_output)
         end
      end
   end
end

t1_config = function()
   -- install the apk
   system(the_true_adb .. " devices")
   local uname = adb_pipe(UNAME_CMD)
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
   check_apk_installed("Setclip.apk", "Setclip.apk.md5")
   check_file_pushed("am.jar", "am.jar.md5")

   local weixin_phone_file, _, errno = io.open("weixin-phones.txt", "rb")
   if not vcf_file then
      adb_start_service_and_wait_file("com.bhj.setclip/.PutClipService --ei listcontacts 1", "/sdcard/listcontacts.txt")
      system(the_true_adb .. " pull /sdcard/listcontacts.txt weixin-phones.txt")
   end

   sdk_version = adb_pipe("getprop ro.build.version.sdk")
   brand = adb_pipe("getprop ro.product.brand"):gsub("\n.*", "")
   model = adb_pipe("getprop ro.product.model"):gsub("\n.*", "")

   debugging("sdk is %s\nbrand is %s\nmodel is %s\n", sdk_version, brand, model)
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
   log("width_ratio is " .. width_ratio .. ", height_ratio is " .. height_ratio)

   local id = adb_pipe("id")
   if id:match("uid=0") then
      using_adb_root = true
   else
      using_adb_root = false
   end

   local scroll = adb_pipe("getprop persist.smartisan.pastetool")
   if scroll:match("1") then
      debugging("pastetool is true")
      using_scroll_lock = true
   else
      using_scroll_lock = false
      check_apk_installed("WrenchIME.apk", "WrenchIME.apk.md5")
      debugging("pastetool is false")
   end
   return ("brand is %s, paste is %s"):format(brand, using_scroll_lock)
end

get_a_note = function(text)
   if text then
      push_text(text)
   end
   notesActivity = 'com.smartisanos.notes/com.smartisanos.notes.NotesActivity'
   adb_am("am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei share-to-note 1")
   wait_input_target(notesActivity)
   adb_event(
      [[
            adb-tap 958 135
            sleep 1
            adb-tap 958 135
            sleep 1
            adb-tap 344 1636
            sleep 1
            adb-tap 711 151
   ]])
   if wait_top_activity(notePicPreview) ~= notePicPreview then
      log("便签好像出问题了")
   end

   while (wait_top_activity_match("com.smartisanos.notes")):match("com.smartisanos.notes/") do
      adb_event("key back sleep .5")
      if not (adb_top_window()):match("com.smartisanos.notes/") then
         break
      end
   end
   adb_get_last_pic('notes', true)
end

adb_get_last_pic = function(which, remove)
   -- WHICH must be 'notes'
   adb_start_service_and_wait_file("com.bhj.setclip/.PutClipService --ei get-last-note-pic 1", "/sdcard/putclip.txt")
   local pic = adb_pipe("cat /sdcard/putclip.txt")
   pic = pic:gsub('^/storage/emulated/0', '/sdcard')
   system{the_true_adb, "pull", pic, ("last-pic-%s.png"):format(which)}
   if remove then
      system{the_true_adb, "shell", "rm", pic}
      adb_am(("am startservice --user 0 -n com.bhj.setclip/.PutClipService --es picture %s"):format(pic))
   end
end

t1_post = function(text) -- use weixin
   local window = adb_focused_window()
   debug("sharing text: %s for window: %s", text, window)
   if text then
      if text:match("^​") and text ~= "​" then
         text = text:sub(string.len("​") + 1)
         local func = loadstring(text)
         t1_eval(func)
         return "executed string"
      end
      if window:match("com.tencent.mobileqq") then
         putclip(emoji_for_qq(text))
      elseif window:match("com.tencent.mm/") then
         putclip(emoji_for_weixin(text))
      else
         putclip(text)
      end
   end
   if window == weixinAlbumPreviewActivity or window == weiboPicFilterActivity then
      sleep(.5)
       window = adb_focused_window()
   end
   if window then print("window is " .. window) end
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
      t1_post()
      return
   elseif window == "com.google.android.gm/com.google.android.gm.ComposeActivityGmail" then
      adb_event("key scroll_lock adb-tap 870 173")
      return
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.QQLSActivity" then
      adb_event("adb-tap 297 884 key scroll_lock adb-tap 923 909")
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
      local input_method, ime_height, ime_connected = adb_get_input_window_dump() -- $(adb dumpsys window | perl -ne 'print if m/^\s*Window #\d+ Window\{[a-f0-9]* u0 InputMethod\}/i .. m/^\s*mHasSurface/')
      debug("input_method is %s, ime_xy is %s", input_method, ime_height)
      -- debugging("ime_xy is %s", ime_xy)

      if input_method then
         add = "key BACK"
      else
         add = "" -- # add="560 1840 key DEL key BACK"
      end
      if input_method then
         if ime_height ~= 0 then
            add = ''
            post_button = ('984 %d'):format(1920 - ime_height - 100)
         end
      else
         if not ime_connected then
            add = '560 1840 sleep .2 key back sleep .2'
         end
      end

      if window == "com.github.mobile/com.github.mobile.ui.issue.CreateCommentActivity" then
         post_button = '954 166'
      end

      debugging("add is %s", add)

      adb_event(string.format("%s key scroll_lock %s", add, post_button))
   end
   return "text sent"
end

t1_send_action = function()
   t1_post('​')
end

local function upload_pics(...)
   local pics = {...}
   adb_shell(
      [[
            for x in /sdcard/DCIM/Camera/t1wrench-*; do
               if test -e "$x"; then
                  rm -rf "$x";
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
      adb_am{"am", "startservice", "--user", "0", "-n", "com.bhj.setclip/.PutClipService", "--es", "picture", target}
   end
   return targets
end

share_text_to_app = function(pkg, cls, text)
   push_text(text)

   if cls:match("^%.") then
       cls = pkg .. cls
   end

   adb_am{"am", "startservice", "--user", "0",
          "-n", "com.bhj.setclip/.PutClipService",
          "--ei", "share-text", "1",
          "--es", "package", pkg,
          "--es", "class", cls
   }
end

share_pics_to_app = function(pkg, cls, pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   if cls:match("^%.") then
       cls = pkg .. cls
   end

   local pics_str = ""

   for i = 1, #pics do
      if i ~= #pics then
         pics_str = pics_str .. ","
      end
      pics_str = pics_str .. pics[i]
   end

   adb_am{"am", "startservice", "--user", "0",
          "-n", "com.bhj.setclip/.PutClipService",
          "--ei", "share-pics", "1",
          "--es", "pics", pics_str,
          "--es", "package", pkg,
          "--es", "class", cls
   }
end

picture_to_weixin_share = function(pics, ...)
   share_pics_to_app("com.tencent.mm", "com.tencent.mm.ui.tools.ShareToTimeLineUI", pics)
   wait_top_activity(weixinSnsUploadActivity)
   adb_event("adb-tap 228 401")
   wait_input_target(weixinSnsUploadActivity)
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
   share_pics_to_app("com.sina.weibo", ".composerinde.ComposerDispatchActivity", pics)
   wait_top_activity(weiboShareActivity)
   adb_event("adb-tap 162 286")
   wait_input_target(weiboShareActivity)
end

picture_to_weibo_comment = function(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local weiboShareActivity = adb_top_window() -- comment or forward
   if #pics ~= 1 then
      log("微博评论、转发只支持一张图片")
   end
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]

      if i == 1 then
         wait_input_target(weiboShareActivity)
         local input_method, ime_height = adb_get_input_window_dump()
         if ime_height ~= 0 then
            adb_event("key back sleep .5")
         end
         for n = 1,10 do
            if adb_top_window() == weiboShareActivity then
               adb_event("adb-tap 62 1843")
            elseif adb_top_window() == weiboAlbumActivity then
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

picture_to_momo_share = function(pics, ...)
   share_pics_to_app("com.immomo.momo", ".android.activity.feed.SharePublishFeedActivity", pics)
   wait_top_activity("com.immomo.momo/com.immomo.momo.android.activity.feed.PublishFeedActivity")
   adb_event("adb-tap 176 329")
   wait_input_target("com.immomo.momo/com.immomo.momo.android.activity.feed.PublishFeedActivity")
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
   local chatWindow = adb_top_window()
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]
      if i == 1 then
         for n = 1,10 do
            local window = adb_top_window()
            if window == chatWindow then
               debug("chatWindow for n: %d", n)
               chatWindow = window
               adb_event(post_button .. " sleep .3")
               if adb_top_window():match("PopupWindow") then
                  debug("got popup window?")
                  adb_event("key back sleep .1 adb-tap 123 1853")
                  wait_top_activity(chatWindow)
               end
               adb_event("adb-tap 203 1430")

               debug("chatWindow: clicked")
               wait_top_activity(weixinAlbumPreviewActivity)
            elseif window == weixinAlbumPreviewActivity then
               adb_event("adb-tap 521 398")
               sleep(.2)
            elseif window == weixinImagePreviewActivity then
               adb_event("sleep .1 adb-key back")
               if wait_top_activity(weixinAlbumPreviewActivity) == weixinAlbumPreviewActivity then
                  sleep(.2)
                  break
               elseif adb_top_window() == weixinImagePreviewActivity then
                  adb_event("key back")
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
   while adb_top_window() ~= weixinImagePreviewActivity do
      adb_event("sleep .1 adb-tap 944 1894 sleep .1")
      wait_top_activity(weixinImagePreviewActivity)
   end
   adb_event("sleep .2 adb-tap 59 1871 sleep .1 adb-tap 927 148")
   window = wait_top_activity(chatWindow)
   if window == weixinImagePreviewActivity then
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
      local input_method, ime_height = adb_get_input_window_dump()
      if ime_height ~= 0 then
         adb_event("sleep .1 adb-key back")
         break
      end
      if not (adb_top_window()):match("^PopupWindow:") then
         break
      end
   end
end

local function picture_to_qq_chat(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = adb_get_input_window_dump()
   if (ime_height ~= 0) then
      ime_height = 0
      log("发送back键以隐藏输入法")
      adb_event("key back")
   end
   local chatWindow
   local post_button = ('159 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local ext = last(pics[i]:gmatch("%.[^.]+"))
      local target = pics[i]
      if i == 1 then
         for n = 1,50 do
            local window = adb_top_window()
            if window == qqChatActivity or window == qqChatActivity2 then
               chatWindow = window
               adb_event(post_button .. " sleep .5 adb-tap 203 1430")
               if wait_top_activity(qqPhotoFlow) ~= qqPhotoFlow then
                  log("Wait for qqPhotoFlow failed")
                  adb_event("sleep .5 adb-tap 203 1430")
                  wait_top_activity(qqPhotoFlow)
               end
               adb_event("adb-tap 351 1703")
            elseif window == qqPhoteList then
               adb_event("adb-tap 171 427")
            elseif window == qqPhotoPreview then
               adb_event("adb-key back")
               if wait_top_activity(qqPhoteList) == qqPhoteList then
                  break
               elseif adb_top_window() == qqPhotoPreview then
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
      local i_button = pic_share_buttons[i]
      sleep(.1)
      adb_event(i_button)
   end
   adb_event("sleep .1 adb-tap 477 1835 sleep .1 adb-tap 898 1840")
   wait_top_activity(chatWindow)
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
         for n = 1,30 do
            local window = adb_top_window()
            if window == weiboChatActivity then
               chatWindow = window
               adb_event(post_button .. " sleep .5 adb-tap 203 1430")
               wait_top_activity(weiboAlbumActivity)
            elseif window == weiboAlbumActivity then
               adb_event("adb-tap 521 398")
               sleep(.2)
            elseif window == weiboImagePreviewActivity then
               adb_event("sleep .5 adb-key back sleep .5")
               if wait_top_activity(weiboAlbumActivity) == weiboAlbumActivity then
                  break
               elseif adb_top_window() == weiboImagePreviewActivity then
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
   wait_top_activity(weiboChatActivity)
   adb_event("key back")
end

local function t1_picture(...)
   local pics = upload_pics(...)
   local window = adb_focused_window()
   if window == weixinLauncherActivity then
      picture_to_weixin_chat(pics)
   elseif window == "com.tencent.mm/com.tencent.mm.ui.chatting.ChattingUI" then
      picture_to_weixin_chat(pics)
   elseif window == "com.tencent.qqlite/com.tencent.mobileqq.activity.ChatActivity" then
      picture_to_qqlite_chat(pics)
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity" then
      picture_to_qq_chat(pics)
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.SplashActivity" then
      picture_to_qq_chat(pics)
   elseif window == "com.sina.weibo/com.sina.weibo.weiyou.DMSingleChatActivity" then
      picture_to_weibo_chat(pics)
   elseif window == weiboCommentActivity or window == weiboForwardActivity then
      picture_to_weibo_comment(pics)
   else
      return "Error: can't decide how to share for window: " .. window
   end
   return #pics .. " pictures sent"
end

local function t1_follow_me()
   check_phone()
   -- http://weibo.com/u/1611427581 (baohaojun)
   -- http://weibo.com/u/1809968333 (beagrep)
   adb_am("am start sinaweibo://userinfo?uid=1611427581")
   wait_top_activity_match("com.sina.weibo/com.sina.weibo.page.")
   adb_event("sleep 1 adb-tap 187 1884")
   log("T1 follow me")
   for n = 1, 10 do
      sleep(.2)
      if adb_top_window() == "com.sina.weibo" then
         sleep(.5)
         adb_event("key back")
         break
      end
   end
end

t1_save_mail_heads = function(file, subject, to, cc, bcc, attachments)
   local f = io.open(file, "w")
   f:write(('t1_load_mail_heads([[%s]], [[%s]], [[%s]], [[%s]], [[%s]])'):format(subject, to, cc, bcc, attachments))
   f:close()
   debugging("hello saving to %s t1_save_mail_heads", file)
end

t1_adb_mail = function(subject, to, cc, bcc, attachments)
   adb_am("am start -n com.android.email/com.android.email.activity.ComposeActivityEmail mailto:; sleep 1; mkdir -p /sdcard/adb-mail")

   adb_event("sleep .5 adb-tap 842 434 sleep .5")

   if attachments:gsub("%s", "") ~= "" then
      local files = split("\n", attachments)
      for i in ipairs(files) do
         local file = files[i]
         adb_event"adb-tap 993 883 sleep .5"

         if not rows_mail_att_finder or rows_mail_att_finder == "手动点" then
            rows_mail_att_finder = select_args{"有几行邮件附件添加应用图标？", "一行", "两行", "手动点（训练）"}
         end
         if rows_mail_att_finder == "一行" then
            adb_event"adb-tap 201 1760"
         elseif rows_mail_att_finder == "两行" then
            adb_event"adb-tap 153 1455"
         else
            select_args{"手动点完之后请回车", "请回车", "请回车!"}
         end


         local target = file:gsub(".*[\\/]", "")
         target = "/sdcard/adb-mail/" .. i .. "." .. target
         system{the_true_adb, "push", file, target}
         target = "../../../../../.." .. target
         putclip(target)

         local window = adb_focused_window()
         if window ~= "org.openintents.filemanager/org.openintents.filemanager.IntentFilterActivity" then
            window = window:gsub("/.*", "")
            error("必须安装并使用OI文件管理器才能选择附件，你使用的是： " .. window)
         end
         adb_event"sleep .5 key back key scroll_lock sleep .5"
         adb_event"adb-tap 959 1876 sleep 1"
      end
   end
   local insert_text = function(contact)
      if contact ~= "" then
         putclip(contact)
         adb_event"key scroll_lock"
      end
      adb_event"key DPAD_DOWN"
   end
   adb_event"adb-tap 247 287"
   insert_text(to)
   insert_text(cc)
   insert_text(bcc)
   insert_text(subject)

   adb_event"key DPAD_UP key DPAD_UP"
end

adb_weixin_lucky_money_output = function(password, bless, money, number)
   adb_am("am start -n " .. weixinLauncherActivity)
   local w = adb_focused_window()
   adb_event("key back")
   adb_event("adb-tap 448 336 adb-tap 961 1835 adb-tap 899 1313 sleep 2")

   if money and number then
      system(("sendtext-android %s; adb-key tab; sendtext-android %s; adb-key tab tab"):format(number, money))
   else
      system("sendtext-android $(random+1 5 5); adb-key tab; sendtext-android $(random+1 5 5).$(random+1 99); adb-key tab tab")
   end

   if not bless then
      t1_post("二十连发，每周四Linux分享，一定要来哦🐉")
   else
      t1_post(bless)
   end
   adb_event("adb-tap 993 1210 adb-tap 375 1396 sleep 3")

   password:gsub(".", function(c)
                    if c == '1' then
                       adb_event("adb-tap 202 1288")
                    elseif c == '2' then
                       adb_event("adb-tap 528 1319")
                    elseif c == '3' then
                       adb_event("adb-tap 910 1240")
                    elseif c == '4' then
                       adb_event("adb-tap 228 1465")
                    elseif c == '5' then
                       adb_event("adb-tap 554 1495")
                    elseif c == '6' then
                       adb_event("adb-tap 944 1475")
                    elseif c == '7' then
                       adb_event("adb-tap 380 1376")
                    elseif c == '8' then
                       adb_event("adb-tap 512 1670")
                    elseif c == '9' then
                       adb_event("adb-tap 886 1637")
                    elseif c == '0' then
                       adb_event("adb-tap 546 1855")
                    end
   end)
end

adb_weixin_lucky_money = function ()
   local loop_n = 1
   while true do
      loop_n = loop_n + 1
      adb_am("am start -n " .. weixinLauncherActivity)
      local w = adb_focused_window()
      if w ~= weixinLauncherActivity then
         adb_event("key back key back")
      end
      adb_event("adb-tap 106 178 adb-tap 173 1862 adb-tap 375 340")


      adb_event([[
               adb-tap 703 1572 sleep .1 adb-tap 523 1263
               adb-tap 711 1409 adb-tap 703 1572 sleep .1 adb-tap 523 1263
               adb-tap 711 1209 adb-tap 703 1572 sleep .1 adb-tap 523 1263
      ]])
      local w = adb_focused_window()
      if w == "com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.LuckyMoneyReceiveUI" then
         adb_event("adb-tap 523 1263 sleep .1")
         if adb_focused_window() == "com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.LuckyMoneyDetailUI" then
            adb_event("key back")
         else
            adb_event("key back")
         end
      elseif loop_n % 3 == 1 then
         adb_event("key back adb-tap 304 510")
         adb_event([[
                  adb-tap 703 1572 sleep .1 adb-tap 523 1263
                  adb-tap 711 1409 adb-tap 703 1572 sleep .1 adb-tap 523 1263
                  adb-tap 711 1209 adb-tap 703 1572 sleep .1 adb-tap 523 1263
         ]])
         w = adb_focused_window()
         if w == "com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.LuckyMoneyReceiveUI" then
            adb_event("adb-tap 523 1263 sleep .1")

            if adb_focused_window() == "com.tencent.mm/com.tencent.mm.plugin.luckymoney.ui.LuckyMoneyDetailUI" then
               adb_event("key back")
            else
               adb_event("key back")
            end
         end
      end
   end
end


t1_find_weixin_contact = function(number)
   adb_am("am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getcontact 1 --es contact " .. number)
end

local press_dial_key = function()
   if not where_is_dial_key then
      where_is_dial_key = select_args{"拨号键在哪儿呢？", "中间", "左数第一个", "左数第二个"}
   end
   debugging("where_is_dial_key is %s", where_is_dial_key)
   if where_is_dial_key == "中间" then
      adb_event("adb-tap 554 1668")
   elseif where_is_dial_key == "左数第一个" then
      adb_event"adb-tap 156 1633"
   elseif where_is_dial_key == "左数第二个" then
      adb_event"adb-tap 420 1634"
   else
      where_is_dial_key = nil
   end
end

t1_call = function(number)
   adb_am("am start -a android.intent.action.DIAL tel:" .. number)
   adb_event("sleep .5")
   press_dial_key()
   adb_event("sleep 1")
   if adb_top_window() == "com.android.contacts/com.android.contacts.activities.DialtactsActivity" then
      press_dial_key()
   end
end

t1_add_mms_receiver = function(number)
   while adb_is_window('com.android.mms/com.android.mms.ui.ComposeMessageActivity') do
      adb_event("key back sleep .1")
   end
   adb_am("am start -n com.android.mms/com.android.mms.ui.ComposeMessageActivity")

   putclip(number .. ',')

   adb_event("sleep 1 key scroll_lock")
   return "请在小扳手文字输入区输入短信内容并发送"
end

log = function(str)
   if log_to_ui then
      log_to_ui(str)
   end
end

t1_eval = function(f)
   for k, v in pairs(M) do
      if not _ENV[k] then
         _ENV[k] = v
      end
   end
   f()
end

t1_run = function (file)
   local ext = file:gsub(".*%.", "")
   if ext ~= "twa" and ext ~= "小扳手" then
      return "Can not run this script, must be a .twa file"
   end
   local f = loadfile(file)
   t1_eval(f)
end

local function t1_spread_it()
   check_phone()
   -- http://weibo.com/1611427581/Bviui9tzF
   -- http://weibo.com/1611427581/BvnNk2PwH?from=page_1005051611427581_profile&wvr=6&mod=weibotime&type=comment
   -- http://m.weibo.cn/1809968333/3774599487375417
   adb_am("am start sinaweibo://userinfo?uid=1611427581")
   wait_top_activity_match("com.sina.weibo/com.sina.weibo.page.")

   for i = 1, 10 do
      sleep((11 - i) * .06)
      if adb_top_window() == "com.sina.weibo/com.sina.weibo.DetailWeiboActivity" then
         break
      end
      adb_event("adb-tap 584 1087")
      if adb_top_window() == "com.sina.weibo/com.sina.weibo.DetailWeiboActivity" then
         break
      end
   end
   adb_event("adb-tap 911 1863")
   sleep(.2)
   adb_event("sleep .2 adb-tap 527 1911")
   wait_input_target("com.sina.weibo/com.sina.weibo.composerinde.CommentComposerActivity")
   adb_event("adb-tap 99 932")
   t1_post("#小扳手真好用# 💑💓💕💖💗💘💙💚💛💜💝💞💟😍😻♥❤")
   adb_event("sleep .5 adb-key back sleep .5")
end

M = {}
M.putclip = putclip
M.start_weibo_share = start_weibo_share
M.t1_post = t1_post
M.t1_find_weixin_contact = t1_find_weixin_contact
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
M.sleep = sleep
M.debugg = debug
M.get_a_note = get_a_note
M.picture_to_momo_share = picture_to_momo_share_upload
M.t1_call = t1_call
M.t1_run = t1_run
M.t1_add_mms_receiver = t1_add_mms_receiver
M.t1_adb_mail = t1_adb_mail
M.t1_save_mail_heads = t1_save_mail_heads
M.adb_weixin_lucky_money = adb_weixin_lucky_money
M.adb_weixin_lucky_money_output = adb_weixin_lucky_money_output
M.adb_event = adb_event
M.t1_send_action = t1_send_action

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
         debugging("cmd is %s", cmd)
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

weixin_emojis = {
   "/::)", "/::~", "/::B", "/::|", "/:8-)", "/::<", "/::$", "/::X", "/::Z", "/::'(",
   "/::-|", "/::@", "/::P", "/::D", "/::O", "/::(", "/::+", "/:--b", "/::Q", "/::T",
   "/:,@P", "/:,@-D", "/::d", "/:,@o", "/::g", "/:|-)", "/::!", "/::L", "/::>", "/::,@",
   "/:,@f", "/::-S", "/:?", "/:,@x", "/:,@@", "/::8", "/:,@!", "/:!!!", "/:xx", "/:bye",
   "/:wipe", "/:dig", "/:handclap", "/:&-(", "/:B-)", "/:<@", "/:@>", "/::-O", "/:>-|", "/:P-(",
   "/::'|", "/:X-)", "/::*", "/:@x", "/:8*", "/:pd", "/:<W>", "/:beer", "/:basketb", "/:oo",
   "/:coffee", "/:eat", "/:pig", "/:rose", "/:fade", "/:showlove", "/:heart", "/:break", "/:cake", "/:li",
   "/:bome", "/:kn", "/:footb", "/:ladybug", "/:shit", "/:moon", "/:sun", "/:gift", "/:hug", "/:strong",
   "/:weak", "/:share", "/:v", "/:@)", "/:jj", "/:@@", "/:bad", "/:lvu", "/:no", "/:ok",
   "/:love", "/:<L>", "/:jump", "/:shake", "/:<O>", "/:circle", "/:kotow", "/:turn", "/:skip", "/:oY",
   "/:#-0", "/:hiphot", "/:kiss", "/:<&", "/:&>",
}

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
