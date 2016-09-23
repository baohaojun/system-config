#!/usr/bin/lua

-- module
local M

-- functions
local adb_input_method_is_null, close_ime
local window_post_button_map = {}
local mail_group_map = {}
local phone_info_map = {}
local save_window_types
local save_phone_info
local phone_serial = ""
local configDir = "."
local last_uploaded_pics = {}
local weixin_open_homepage
local file_exists

local t1_call, t1_run, t1_adb_mail, t1_save_mail_heads
local adb_push, adb_pull, adb_install
local shell_quote, putclip, t1_post, push_text, t1_post2, kill_android_vnc
local adb_start_activity
local picture_to_weixin_share, picture_to_weibo_share, picture_to_qq_share
local picture_to_momo_share, t1_add_mms_receiver
local adb_get_input_window_dump, adb_top_window
local adb_start_weixin_share, adb_is_window
local adb_focused_window
local t1_config, check_phone
local weixin_find_friend, qq_find_friend, qq_find_group_friend
local emoji_for_qq, debug, get_a_note, emoji_for_weixin, emoji_for_qq_or_weixin
local adb_get_last_pic, debugging
local adb_weixin_lucky_money
local adb_weixin_lucky_money_output
local t1_find_weixin_contact, t1_find_qq_contact, t1_find_dingding_contact
local adb_start_service_and_wait_file_gone
local adb_start_service_and_wait_file, adb_am
local wait_input_target, wait_top_activity, wait_top_activity_match
local wait_input_target_n
local t1_eval, log, share_pics_to_app, share_text_to_app
local picture_to_weibo_comment
local check_scroll_lock, prompt_user, yes_or_no_p
local smartisan_mail_compose = "com.android.email/com.android.mail.compose.ComposeActivity"

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
local real_width, real_height = 1080, 1920
local app_width, app_height = 1080,1920
local app_width_ratio, app_height_ratio = app_width / default_width,  app_height / default_height
local real_width_ratio, real_height_ratio = real_width / default_width, real_height / default_height
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
local weixinSearchActivity = "com.tencent.mm/com.tencent.mm.plugin.search.ui.FTSMainUI"
local weixinSnsUploadActivity = "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI"
local weixinImagePreviewActivity = "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.ImagePreviewUI"
local weiboShareActivity = "com.sina.weibo/com.sina.weibo.composerinde.OriginalComposerActivity"
local qqShareActivity = "com.qzone/com.qzonex.module.operation.ui.QZonePublishMoodActivity"
local emailSmartisanActivity = "com.android.email/com.android.mail.compose.ComposeActivity"
local oiFileChooseActivity = "org.openintents.filemanager/org.openintents.filemanager.IntentFilterActivity"
local weiboCommentActivity = "com.sina.weibo/com.sina.weibo.composerinde.CommentComposerActivity"
local weiboForwardActivity = "com.sina.weibo/com.sina.weibo.composerinde.ForwardComposerActivity"
local qqChatActivity = "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity"
local qqChatActivity2 = "com.tencent.mobileqq/com.tencent.mobileqq.activity.SplashActivity"
local qqAlbumList = "com.tencent.mobileqq/com.tencent.mobileqq.activity.photo.AlbumListActivity"
local qqCameraFlow = "com.tencent.mobileqq/com.tencent.mobileqq.activity.richmedia.FlowCameraPtvActivity2"
local qqGroupSearch = "com.tencent.mobileqq/com.tencent.mobileqq.search.activity.GroupSearchActivity"
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

adb_unquoter = ""

shell_quote = function (str)
   return "'" .. string.gsub(str, "'", "'\\''") .. "'"
end

function spairs(t, order)
    -- collect the keys
    local keys = {}
    for k in pairs(t) do keys[#keys+1] = k end

    -- if order function given, sort by it by passing the table and keys a, b,
    -- otherwise just sort the keys
    if order then
        table.sort(keys, function(a,b) return order(t, a, b) end)
    else
        table.sort(keys)
    end

    -- return the iterator function
    local i = 0
    return function()
        i = i + 1
        if keys[i] then
            return keys[i], t[keys[i]]
        end
    end
end

if package.config:sub(1, 1) == '/' then
   if is_debugging then
      debug_set_x = "set -x; "
   else
      debug_set_x = ""
   end
else -- windows
   if adb_quick_am == nil then
      shell_quote = function (str)
         str = str:gsub('\n', '')
         str = str:gsub('\\', '\\\\')
         str = str:gsub('"', '\\"')
         return '"' .. str .. '"'
      end
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

local adb_pipe
local function adb_shell(cmds)
   if qt_adb_pipe then
      return adb_pipe(cmds)
   end
   return adb_do(os.execute, cmds)
end

adb_pipe = function(cmds)
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
         local width_ratio, height_ratio = app_width_ratio, app_height_ratio
         if (events[i - 1] == 'adb-no-virt-key-tap') then
            width_ratio, height_ratio = real_width_ratio, real_height_ratio
         end
         local add = ('input tap %d %d;'):format(events[i] * width_ratio, events[i+1] * height_ratio)
         command_str = command_str .. add
         i = i + 2
      elseif events[i] == 'tap2' or events[i] == 'adb-tap-2' then
         i = i + 1
         local add = ('input tap %d %d;'):format(events[i] * app_width_ratio, events[i+1] * app_height_ratio)
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
            events[i+1] * app_width_ratio, events[i+2] * app_height_ratio,
            events[i+1] * app_width_ratio, events[i+2] * app_height_ratio, ms)
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
      elseif events[i] == 'swipe' or (events[i]):match('adb%-swipe%-') or (events[i]):match('adb%-no%-virt%-key%-swipe%-') then
         ms = 500
         local width_ratio, height_ratio = app_width_ratio, app_height_ratio

         if (events[i]):match('adb%-swipe%-') then
            ms = (events[i]):sub(#'adb-swipe-' + 1)
         elseif (events[i]):match('adb%-no%-virt%-key%-swipe%-') then
            ms = (events[i]):sub(#'adb-no-virt-key-swipe-' + 1)
            width_ratio, height_ratio = real_width_ratio, real_height_ratio
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
      elseif events[i] == 'adb-tap' or events[i] == 'adb-no-virt-key-tap' then
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

prompt_user = function(txt)
   if select_args then
      return select_args{txt}
   end
end

yes_or_no_p = function(txt)
   if select_args then
      return select_args{txt} ~= ""
   end
   return false
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
      local prompt_str = ("Your phone's current input method：%s does not work with Wrench, must use “小扳手输入法”, please make sure you have installed it (it comes with Wrench source code), and set it as the current input method."):format(current_input_method)
      if select_args then
         if select_args{prompt_str} == "" then
            error("You've canceled, please configure “小扳手输入法” on your phone before you continue with Wrench")
         end
      else
         debug("%s", prompt_str)
         sleep(1)
      end
   end
end

local function weibo_text_share(window)
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
   adb_event{'key', 'scroll_lock', 991, 166}
end

local function t1_share_to_weibo(text)
   share_text_to_app("com.sina.weibo", ".composerinde.ComposerDispatchActivity", text)
   wait_input_target(weiboShareActivity)
   t1_send_action()
end

local function t1_share_to_qq(text)
   share_text_to_app("com.qzone", "com.qzonex.module.operation.ui.QZonePublishMoodActivity",  text)
   wait_input_target(qqShareActivity)
   t1_send_action()
end

local wait_top_activity_n = function(n_retry, ...)
   activities = {...}
   for i = 1, #activities do
      debug("wait for top activity: %s", activities[i])
   end

   local window
   for i = 1, n_retry do
      for ai = 1, #activities do
         local activity = activities[ai]
         window = adb_focused_window()
         if window == activity then
            debug("wait ok")
            return window
         end
      end
      sleep(.1)
   end
   return window
end

wait_top_activity = function(...)
   return wait_top_activity_n(20, ...)
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

wait_input_target = function(...)
   return wait_input_target_n(20, ...)
end

wait_input_target_n = function(n_loop, ...)
   activities = {...}
   for i = 1, #activities do
      debug("wait for input method for %s", activities[i])
   end
   for i = 1, n_loop do
      local window = adb_focused_window()
      for ai = 1, #activities do
         local activity = activities[ai]
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

local function search_mail(what)
   putclip_nowait(what)
   for i = 1, 5 do
      adb_start_activity"com.android.email/com.android.email.activity.Welcome"
      adb_event"sleep .5 adb-tap 66 155 sleep .5"
      if adb_top_window() == "com.android.email/com.android.email.activity.setup.AccountSettings" then
         break
      end
      log("Did not get mail settings at %d", i)
      adb_event"key back"
   end
   if adb_top_window() ~= "com.android.email/com.android.email.activity.setup.AccountSettings" then
      log("Failed to get mail settings")
      return
   end
   adb_event"key back sleep .5 adb-tap 218 343 sleep .2 key scroll_lock"
end

local function get_coffee(what)
   for i = 1, 5 do
      weixin_open_homepage()
      log("Start to click for the favsearch " .. i)
      adb_event"adb-tap 927 1830 sleep .2 adb-tap 337 772 sleep 1 adb-tap 833 145 sleep .2"
      if adb_top_window() == "com.tencent.mm/com.tencent.mm.plugin.favorite.ui.FavSearchUI" then
         break
      end
      log("Need retry " .. i)
   end
   putclip"我正在使用咕咕机"
   adb_event"key scroll_lock sleep .5 adb-tap 535 458 sleep 3 adb-tap 15 612"
   for i = 1, 50 do
      local input_target = wait_input_target_n(1, "com.tencent.mm/com.tencent.mm.plugin.webview.ui.tools.WebViewUI")
      if input_target:match"com.tencent.mm/com.tencent.mm.plugin.webview.ui.tools.WebViewUI" then
         break
      end
      log("Click for coffee input: %s %d", input_target, i)

      adb_event"adb-tap 15 700"
      sleep(.1)
   end
   if what == "" then
      what = "秦师傅，给我来一杯拿铁，谢谢❤"
   end
   putclip(what)
   adb_event"key scroll_lock sleep .5"
   if yes_or_no_p("确认发送秦师傅咖啡订单？") then
      adb_event"adb-tap 539 957"
      system{'alarm', '10', 'Go get your coffee (take your coffee ticket!)'}
   end

end

weixin_open_homepage = function()
   adb_am("am start -n " .. weixinLauncherActivity)
   wait_top_activity_match("com.tencent.mm/")
   for i = 1, 20 do
      sleep(.1)
      log("touch the search button " .. i)

      adb_event"adb-tap 801 132"
      local waiting_search = true
      local top_window
      for i_search = 1, 4 do
         sleep(.1)
         top_window = adb_top_window()
         if top_window == weixinSearchActivity then
            log("exit from search by key back " .. i_search)
            wait_input_target(weixinSearchActivity)
            adb_event"key back sleep .1 key back sleep .1"
            sleep(.1)
            waiting_search = false
            return
         elseif top_window ~= weixinLauncherActivity then
            log("exit the current '%s' by back key %d", top_window, i)
            waiting_search = false
         end
         if not waiting_search then
            break
         end
      end
      log("exit the current '%s' by touching back botton %d", top_window, i)
      adb_event"88 170 sleep .1 88 170 sleep .1"
      sleep(.1)
      adb_am("am start -n " .. weixinLauncherActivity)
   end
end

local function dingding_open_homepage()
   local dingding_splash = "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.SplashActivity"
   local dingding_home = "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.home.activity.HomeActivity"
   adb_am("am start -n " .. dingding_splash)
   wait_top_activity_match("com.alibaba.android.rimet/")
   for i = 1, 20 do
      sleep(.1)
      local window = adb_top_window()
      if window ~= dingding_splash and window ~= dingding_home then
         if window == "com.alibaba.android.rimet/com.alibaba.android.user.login.SignUpWithPwdActivity" then
            log("You need to sign in dingding")
            break
         end
         log("window is %s at %d", window, i)
         adb_event"key back sleep .1"
         sleep(.1)
         adb_am("am start -n " .. dingding_splash)
         wait_top_activity_match("com.alibaba.android.rimet/")
      elseif window == dingding_splash then
         adb_event"adb-tap 863 222"
      else
         break
      end

   end
end

local function qq_open_homepage()
   adb_start_activity(qqChatActivity2)

   for qq_try = 1, 40 do
      sleep(.2)
      local ime_active, height, ime_connected = adb_get_input_window_dump()
      log("ime activity is %s, height is %d, ime_connected is %s", ime_active, height, ime_connected)
      local done_back = false
      if ime_connected then
         adb_event"key back sleep .1"
         done_back = true
      end
      if ime_active then
         done_back = true
         adb_event"key back sleep .1"
      end
      if not done_back and adb_top_window() == qqChatActivity2 then
         adb_event"adb-tap 181 1863 sleep .5"
         local top_window = adb_top_window()
         if not top_window or top_window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.QQSettingSettingActivity" then
            log("got into setting!")
            adb_event"key back sleep .1 key back sleep .1 key back sleep .1"
         else
            break
         end
      end

      adb_event"key back sleep .1"
      adb_am("am start -n " .. qqChatActivity2)
   end
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

   weixin_open_homepage()
   adb_event("adb-tap 654 1850 sleep .5 adb-tap 332 358")
   if wait_top_activity("com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsTimeLineUI") == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsTimeLineUI" then
      adb_event("sleep .2 " .. click .. " 961 160")
   else
      log("Can't switch to the Friend Zone page.")
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
   adb_event"adb-tap 192 1227 sleep .5 adb-key scroll_lock"
   if yes_or_no_p("确认发送短信？") then
      adb_event"adb-tap 857 1008"
   end
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
      wait_input_target(smartisan_mail_compose)
      sleep(.5)
   end
   adb_event("key scroll_lock sleep .5")
   if yes_or_no_p("确认发送邮件") then
      if window == 'com.google.android.gm/com.google.android.gm.ComposeActivityGmail' then
         adb_event{806, 178}
      else
         adb_event("adb-tap 998 174")
      end
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
      if tonumber((ime_height - (real_height - app_height)) * default_height / real_height ) >= 800 then -- new version of google pinyin ime?
         if input_window_dump:match('package=com.google.android.inputmethod.pinyin') then
            ime_height = (1920 - 1140) * real_height / default_height + (real_height - app_height)
         elseif input_window_dump:match('package=com.wrench.inputmethod.pinyin') then
            ime_height = (1920 - 1125) * real_height / default_height + (real_height - app_height)
         elseif input_window_dump:match('package=com.google.android.inputmethod.latin') or
            input_window_dump:match('package=com.android.inputmethod.latin') then
            ime_height = 800 * real_height / default_height + (real_height - app_height)
         end
      end
   end

   local ime_connected = not dump_str:match("mServedInputConnection=null")
   return input_method_active, ime_height, ime_connected, current_input_method
end

adb_input_method_is_null = function ()
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

adb_wait_file_gone = function(file)
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

local function adb_start_service(service_cmd)
   adb_am("am startservice --user 0 -n " .. service_cmd)
end

adb_start_service_and_wait_file_gone = function(service_cmd, file)
   adb_start_service(service_cmd)
   adb_wait_file_gone(file)
end

adb_start_service_and_wait_file = function(service_cmd, file)
   local res = adb_pipe(
      (
         [[
            rm %s;
            am startservice --user 0 -n %s&
            for x in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do
               if test ! -e %s; then
                  sleep .1 || busybox sleep .1;
               else
                  echo -n ye && echo s
                  exit;
               fi;
            done
      ]]):format(file, service_cmd, file))
   if res:match("yes") then
      return true
   else
      return false
   end
end

adb_push = function(lpath, rpath)
   if type(lpath) == 'table' then
      if #lpath ~= 2 then
         error("invalid adb_push call")
      end
      lpath, rpath = lpath[1], lpath[2]
   end
   if qt_adb_push then
      qt_adb_push{lpath, rpath}
   else
      system{the_true_adb, 'push', lpath, rpath}
   end
end

adb_pull = function(rpath, lpath)
   if type(rpath) == 'table' then
      if #rpath ~= 2 then
         error("invalid adb_pull call")
      end
      rpath, lpath = rpath[1], rpath[2]
   end
   if qt_adb_pull then
      qt_adb_pull{rpath, lpath}
   else
      system{the_true_adb, 'pull', rpath, lpath}
   end
end

adb_install = function(apk)
   if qt_adb_install then
      return qt_adb_install{apk}
   else
      return io.popen(the_true_adb .. " install -r " .. apk):read("*a")
   end
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
   adb_push{path, '/sdcard/putclip.txt'}
end

putclip_nowait = function(text)
   push_text(text)
   adb_start_service('com.bhj.setclip/.PutClipService')
end

putclip = function(text)
   push_text(text)
   adb_start_service_and_wait_file_gone('com.bhj.setclip/.PutClipService', '/sdcard/putclip.txt')
end

local check_file_push_and_renamed = function(file, md5, rename_to)
   if not rename_to then
      rename_to = file:gsub(".*/", "")
   end

   local md5_on_phone = adb_pipe("cat /sdcard/" .. md5)
   md5_on_phone = md5_on_phone:gsub("\n", "")
   local md5file = io.open(md5)
   local md5_on_PC = md5file:read("*a")
   md5_on_PC = md5_on_PC:gsub("\n", "")
   io.close(md5file)
   debugging("on phone: %s, local: %s", md5_on_phone, md5_on_PC)
   if md5_on_phone ~= md5_on_PC then
      log("Need to upload %s to your phone.", file)
      adb_push{file, "/data/data/com.android.shell/" .. rename_to .. ".bak"}
      adb_shell(("mv /data/data/com.android.shell/%s.bak /data/data/com.android.shell/%s; chmod 755 /data/data/com.android.shell/%s"):format(rename_to, rename_to, rename_to))

      adb_push{md5, "/sdcard/" .. md5}
      local md5_on_phone = adb_pipe("cat /sdcard/" .. md5)
      md5_on_phone = md5_on_phone:gsub("\n", "")
      if md5_on_phone ~= md5_on_PC then
         error("Can't mark the " .. file .. " as been installed: \n'" .. md5_on_phone .. "'\n : \n'" .. md5_on_PC .. "'")
      else
         log("Wrench helper file %s upload OK.", file)
      end
   end
end

local check_file_pushed = function(file, md5)
   return check_file_push_and_renamed(file, md5, nil)
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
      log("Need to install on your phone Wrench helper App %s, please make sure your phone allows it.", apk)
      local install_output = adb_install(apk)
      if install_output:match("\nSuccess") then
         adb_push{md5, "/sdcard/" .. md5}
         local md5_on_phone = adb_pipe("cat /sdcard/" .. md5)
         md5_on_phone = md5_on_phone:gsub("\n", "")
         if md5_on_phone ~= md5_on_PC then
            error("Can't mark the " .. apk .. " as been installed: \n'" .. md5_on_phone .. "'\n : \n'" .. md5_on_PC .. "'")
         else
            log("Wrench helper App %s install OK.", apk)
         end
      else
         if not os.execute("test -e .quiet-apk-install-failure") then
            error("Install " .. apk .. " failed, output is " .. install_output)
         end
      end
   end
end

if not t1_set_variable then
   t1_set_variable = function(name, val)
   end
end

t1_config = function(passedConfigDirPath)
   -- install the apk
   if not qt_adb_pipe then
      local p = io.popen("the-true-adb version")
      local v = p:read("*a")
      if v:match("1.0.31") then
         adb_unquoter = '\\"'
      end
   end

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
         adb_kill_server()
         error("Done config for your adb devices, please try again")
      else
         error(string.format("No phone found, can't set up, uname is: '%s', ANDROID_SERIAL is '%s'", uname, os.getenv("ANDROID_SERIAL")))
      end
   end
   check_apk_installed("Setclip.apk", "Setclip.apk.md5")
   check_file_pushed("am.jar", "am.jar.md5")
   check_file_pushed("busybox", "busybox.md5")

   local weixin_phone_file, _, errno = io.open("weixin-phones.txt", "rb")
   if not vcf_file then
      local res = adb_pipe("if test -e /sdcard/listcontacts.txt; then echo yes; fi")
      if res and res:match("yes") then
         adb_pull{"/sdcard/listcontacts.txt", "weixin-phones.txt"}
      end
      if adb_start_service_and_wait_file("com.bhj.setclip/.PutClipService --ei listcontacts 1", "/sdcard/listcontacts.txt") then
         adb_pull{"/sdcard/listcontacts.txt", "weixin-phones.txt"}
      else
         log("Can't sync Wechat contacts")
      end
   end

   sdk_version = adb_pipe("getprop ro.build.version.sdk")
   brand = adb_pipe("getprop ro.product.brand"):gsub("\n.*", "")
   model = adb_pipe("getprop ro.product.model"):gsub("\n.*", "")
   arm_arch = adb_pipe("/data/data/com.android.shell/busybox uname -m")
   androidvncserver = ("androidvncserver-%s.sdk%s"):format(arm_arch, sdk_version)

   if file_exists(androidvncserver) then
      check_file_push_and_renamed(androidvncserver, androidvncserver ..  ".md5", "androidvncserver")
      t1_set_variable("using-vnc", "true")
   else
      t1_set_variable("using-vnc", "false")
   end

   debugging("sdk is %s\nbrand is %s\nmodel is %s\n", sdk_version, brand, model)
   sdk_version = tonumber(sdk_version)
   if tonumber(sdk_version) < 16 then
       error("Error, you phone's sdk version is " .. sdk_version .. ",  must be at least 16")
   end
   local dump = adb_pipe{'dumpsys', 'window'}
   real_width = dump:match('init=(%d+x%d+)')
   real_height = tonumber(real_width:match('x(%d+)'))
   real_width = tonumber(real_width:match('(%d+)x'))

   app_width = dump:match('app=(%d+x%d+)')
   app_height = app_width:match('x(%d+)')
   app_width = app_width:match('(%d+)x')
   app_width_ratio, app_height_ratio = app_width / default_width,  app_height / default_height
   real_width_ratio, real_height_ratio = real_width / default_width, real_height / default_height
   log("app_width_ratio is %f, app_height_ratio is %f ", app_width_ratio, app_height_ratio)

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


   if passedConfigDirPath then
      configDir = passedConfigDirPath
   end
   local dofile_res
   dofile_res, mail_group_map = pcall(dofile, configDir .. package.config:sub(1, 1) .. "mail_groups.lua")
   if not dofile_res then
      mail_group_map = {}
   end

   dofile_res, window_post_button_map = pcall(dofile, configDir .. package.config:sub(1, 1) .. "window_post_botton.lua")
   if not dofile_res then
      dofile_res, window_post_button_map = pcall(dofile, "window_post_botton.lua")
      if not dofile_res then
         window_post_button_map = {}
         save_window_types()
      end
   end

   dofile_res, phone_info_map = pcall(dofile, configDir .. package.config:sub(1, 1) .. "phone_info.lua")
   if not dofile_res then
      log("phone info failed")
      phone_info_map = {}
      save_phone_info()
   end

   phone_serial = adb_pipe("getprop ro.serialno"):gsub("\n", "")
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
      log("Seems to have a problem with Smartisan Notes")
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
   adb_pull{pic, ("last-pic-%s.png"):format(which)}
   if remove then
      adb_shell{rm, pic}
      adb_am(("am startservice --user 0 -n com.bhj.setclip/.PutClipService --es picture %s"):format(pic))
   end
end

t1_post2 = function(text1, text2)
   putclip(text1)
   adb_event("key scroll_lock key dpad_down")
   t1_post(text2)
end

weixin_find_friend = function(friend_name)
   weixin_open_homepage()
   adb_event"adb-tap 786 116"
   putclip(friend_name)
   wait_input_target("com.tencent.mm/com.tencent.mm.plugin.search.ui.FTSMainUI")
   adb_event"sleep .2 key scroll_lock sleep .5"
   adb_event"adb-tap 245 382"
end

t1_find_dingding_contact = function(friend_name)
   dingding_open_homepage()
   adb_event"adb-tap 831 129"
   putclip(friend_name)
   wait_input_target("com.alibaba.android.rimet/com.alibaba.android.user.search.activity.GlobalSearchInputActivity")
   adb_event"sleep .2 key scroll_lock sleep .8"

   adb_event"adb-tap 276 354 sleep .8 adb-tap 140 1880"
end

qq_find_friend = function(friend_name)
   putclip_nowait(friend_name)
   log("qq find friend: %s", friend_name)
   for i = 1, 5 do
      qq_open_homepage()
      adb_event"sleep .3 adb-tap 391 288"
      local top_window = wait_input_target_n(15, qqChatActivity2, qqGroupSearch)
      adb_event"key scroll_lock sleep .6"
      if top_window and top_window:match(qqGroupSearch) then
         log"Fonud qqGroupSearch"
         adb_event"adb-tap 365 384"
         break
      else
         log("Got stuck in qqChatActivity2, ime stuck?: %s at %d", top_window, i)
         if i == 5 then
            error("Can't get to qqGroupSearch in the end")
         end
         adb_event"adb-tap 303 291"
      end
   end
end

qq_find_group_friend = function(friend_name)
   putclip_nowait(friend_name)
   log("qq find group friend: %s", friend_name)
   local window = adb_top_window()
   if window ~= qqChatActivity and window ~= qqChatActivity2 then
      log("qq window is not chat: %s", window)
      return
   end
   adb_event("adb-tap 974 167")
   local chatSetting = "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatSettingForTroop"
   window = wait_top_activity(chatSetting)
   if window ~= chatSetting then
      log("did not get chatSetting: %s", window)
      return
   end
   adb_event("sleep 1 adb-tap 330 1482")
   local troopList = "com.tencent.mobileqq/com.tencent.mobileqq.activity.TroopMemberListActivity"
   window = wait_top_activity(troopList)
   if window ~= troopList then
      log("did not get troopWindow: %s", window)
      return
   end
   adb_event("sleep .5 adb-tap 243 305")
   wait_input_target(troopList)
   adb_event("key scroll_lock key space key DEL sleep .5 adb-tap 326 320")
   local troopMember = "com.tencent.mobileqq/com.tencent.mobileqq.activity.TroopMemberCardActivity"
   for i = 1, 5 do
      window = wait_top_activity_n(2, troopMember)
      if window == troopMember then
         break
      else
         log("Did not get into troopMember, try " .. i)
         adb_event("key space sleep .5 key DEL sleep .5 adb-tap 326 320")
         if i == 5 then
            log("Giving up...")
            return
         end
      end
   end
   adb_event("sleep .5 adb-tap 864 1800")
end

save_window_types = function()
   local mapfile = io.open(configDir .. package.config:sub(1, 1) .. "window_post_botton.lua", "w")
   mapfile:write("local map = {}\n")
   for k, v in spairs(window_post_button_map) do
      if k ~= "" then
         mapfile:write(("map['%s'] = '%s'\n"):format(k, v))
      end
   end
   mapfile:write("return map\n")
   mapfile:close()
end

save_phone_info = function()
   local infofile = io.open(configDir .. package.config:sub(1, 1) .. "phone_info.lua", "w")
   infofile:write("local map = {}\n")
   for k, v in pairs(phone_info_map) do
      infofile:write(("map['%s'] = '%s'\n"):format(k, v))
   end
   infofile:write("return map\n")
   infofile:close()
end

kill_android_vnc = function()
   adb_shell"busybox killall -INT androidvncserver"
end

file_exists = function(name)
   local f=io.open(name,"r")
   if f ~= nil then
      io.close(f)
      return true
   else
      return false
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
   elseif window == smartisan_mail_compose or
      window == "com.android.email/com.android.email.activity.Welcome" or
      window == "com.android.email/com.android.email2.ui.MailActivityEmail" or
   window == emailSmartisanActivity then
      t1_mail(window)
      return
   elseif string.match(window, "^PopupWindow:") then
      t1_paste()
      return
   else
      local add, post_button = '', '958 1820'
      local input_method, ime_height, ime_connected = adb_get_input_window_dump() -- $(adb dumpsys window | perl -ne 'print if m/^\s*Window #\d+ Window\{[a-f0-9]* u0 InputMethod\}/i .. m/^\s*mHasSurface/')
      log("input_method is %s, ime_xy is %s", input_method, ime_height)
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
            adb_event("560 1840")
            wait_input_target(window)
            adb_event("key back sleep .2")
            add = ''
         end
      end

      if window == "com.github.mobile/com.github.mobile.ui.issue.CreateCommentActivity" then
         post_button = '954 166'
      end

      local window_type = window_post_button_map[window]
      if not window_type then
         window_type = select_args{'Where is the send button',
                                   'Above the input method, the right end',
                                   'Above the input method, the right end, with a row of buttons in between (like QQ)',
                                   'Above the input method, the right end, confirm before send',
                                   "Top-right corner of phone's screen",
                                   "Top-right corner of phone's screen, confirm before send",
                                   'I will click the send button myself',
         }
         if window_type == 'Above the input method, the right end' then
            window_type = 'weixin-chat'
         elseif window_type == 'Above the input method, the right end, with a row of buttons in between (like QQ)' then
            window_type = 'qq-chat'
         elseif window_type == "Top-right corner of phone's screen" then
            window_type = 'weibo-share'
         elseif window_type == 'Above the input method, the right end, confirm before send' then
            window_type = 'weixin-confirm'
         elseif window_type == "Top-right corner of phone's screen, confirm before send" then
            window_type = 'weibo-confirm'
         else
            window_type = 'manual-post'
         end
         window_post_button_map[window] = window_type
         save_window_types()
      end
      if window_type == 'weixin-chat' then
         post_button = post_button -- empty
      elseif window_type == 'qq-chat' then
         post_button = ('975 %d'):format(1920 - ime_height - 200)
      elseif window_type == 'weixin-confirm' then
         if yes_or_no_p("Send button is above the input method, on the right end. Confirm?") then
            post_button = post_button
         else
            post_button = ''
         end
      elseif window_type == 'weibo-share' then
         post_button = '991 166'
      elseif window_type == 'weibo-confirm' then
         if yes_or_no_p("Send button is top-right corner of phone's screen. Confirm?") then
            post_button = '991 166'
         else
            post_button = ''
         end
      elseif window_type == 'manual-post' then
         post_button = ''
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
            for x in /sdcard/DCIM/Camera/000-wrench-*; do
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
      local target = ('/sdcard/DCIM/Camera/000-wrench-%d-%d%s'):format(time, i, ext)
      targets[#targets + 1] = target
      adb_push{pics[i], target}
      adb_am{"am", "startservice", "--user", "0", "-n", "com.bhj.setclip/.PutClipService", "--es", "picture", target}
   end
   last_uploaded_pics = targets
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
   if pics == nil then
      pics = last_uploaded_pics
   end
   share_pics_to_app("com.tencent.mm", "com.tencent.mm.ui.tools.ShareToTimeLineUI", pics)
   wait_top_activity(weixinSnsUploadActivity)
   adb_event("adb-tap 228 401")
   wait_input_target(weixinSnsUploadActivity)
end

picture_to_qq_share = function(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
   log("share to qq")
   share_pics_to_app("com.qzone", "com.qzonex.module.operation.ui.QZonePublishMoodActivity", pics)
   wait_top_activity(qqShareActivity)
   adb_event("adb-tap 228 401")
   wait_input_target(qqShareActivity)
end

picture_to_weibo_share = function(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
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
   if pics == nil then
       pics = last_uploaded_pics
   end
   share_pics_to_app("com.immomo.momo", ".android.activity.feed.SharePublishFeedActivity", pics)
   wait_top_activity("com.immomo.momo/com.immomo.momo.android.activity.feed.PublishFeedActivity")
   adb_event("adb-tap 176 329")
   wait_input_target("com.immomo.momo/com.immomo.momo.android.activity.feed.PublishFeedActivity")
end

local function picture_to_weixin_chat(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = close_ime()
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
      local input_method, ime_height = close_ime()
      if not (adb_top_window()):match("^PopupWindow:") then
         break
      end
   end
end

close_ime = function()
   local input_method, ime_height = adb_get_input_window_dump()
   if (ime_height ~= 0) then
      ime_height = 0
      log("Send the back key to hide IME.")
      adb_event("key back")
   end
   return input_method, ime_height
end

local function click_to_album_wx_chat_style(event1, activity1, ...)
   local input_method, ime_height = close_ime()
   local post_button = ('984 %d'):format(1920 - 50)
   local old_top_window = adb_top_window()

   adb_event(post_button .. " sleep .2 " .. event1)
   local top_window = activity1
   if top_window ~= activity1 then
      log("Can't get to %s, got %s", activity1, top_window)
      return nil
   end
   return true
end

local function picture_to_dingding_chat(pics, ...)
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
   if not wait_top_activity_n(2, old_top_window) then
      log"Can't get old dd chat window"
   end
end

local function picture_to_qq_chat(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = close_ime()
   local chatWindow
   local image_button = ('288 %d'):format(1920 - ime_height - 50)
   local post_button = ('159 %d'):format(1920 - ime_height - 50)
   for i = 1, #pics do
      local target = pics[i]
      if i == 1 then
         for n = 1,50 do
            local window = adb_top_window()
            if window == qqChatActivity or window == qqChatActivity2 then
               chatWindow = window
               adb_event(image_button .. " sleep 2 adb-tap 70 1873")
               local top_window = wait_top_activity(qqAlbumList, qqCameraFlow)

               if top_window == qqCameraFlow then
                  log("get qqCameraFlow")
                  while adb_top_window() == qqCameraFlow do
                      log("still got qqCameraFlow")
                      adb_event"key back sleep .5"
                  end
                  image_button = ('380 %d'):format(1920 - ime_height - 50)
               elseif top_window ~= qqAlbumList then
                  log("Wait for qqAlbumList failed")
                  return
               else
                  adb_event("sleep .5 adb-tap 329 336")
               end
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

local function picture_to_weibo_chat(pics, ...)
   if type(pics) ~= "table" then
      pics = {pics, ...}
   end

   local input_method, ime_height = close_ime()
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
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity" then
      picture_to_qq_chat(pics)
   elseif window == "com.alibaba.android.rimet/com.alibaba.android.dingtalkim.activities.ChatMsgActivity" then
      picture_to_dingding_chat(pics)
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

expand_mail_groups = function(contacts)
   local contact_array = split(",", contacts)
   local res = ""
   for i in ipairs(contact_array) do
      local contact = contact_array[i]
      if mail_group_map[contact] then
         res = res .. mail_group_map[contact]
      else
         res = res .. contact
      end
      res = res .. ","
   end
   return res
end

t1_adb_mail = function(subject, to, cc, bcc, attachments)
   to = expand_mail_groups(to)
   if to ~= "" and subject == "" and cc == "" and bcc == "" and attachments == "" then
         while adb_top_window() == "com.android.contacts/com.android.contacts.activities.ContactSelectionActivity" do
             log("Need get rid of contact selection dialog")
             adb_event"key back sleep 1"

         end
         putclip(to)
         adb_event"sleep .5 key scroll_lock sleep .5"
         return
   end

   local paste_attachment_only = false

   if attachments ~= "" and subject == "" and cc == "" and bcc == "" and to == "" then
      paste_attachment_only = true
   end

   if not paste_attachment_only then

      cc = expand_mail_groups(cc)
      bcc = expand_mail_groups(bcc)

      adb_am("am start -n " .. emailSmartisanActivity .. " mailto:")
      adb_shell"mkdir -p /sdcard/adb-mail"
      wait_input_target(emailSmartisanActivity)

      adb_event("adb-tap 842 434 sleep 1.5") -- 展开
   end

   if attachments:gsub("%s", "") ~= "" then
      local files = split("\n", attachments)
      for i in ipairs(files) do
         local file = files[i]
         if paste_attachment_only then
            if yes_or_no_p("Please click on the phone the button for adding attachments") then
               sleep(.5)
            else
               return
            end
         else
            adb_event"adb-tap 993 883 sleep .5"
         end

         if not rows_mail_att_finder or rows_mail_att_finder:match"^Manual Click" then
            rows_mail_att_finder = select_args{"How many lines of Apps are there？", "One", "Two", "Manual Click (OI File Manager is not the first App yet)"}
         end
         if rows_mail_att_finder == "One" then
            adb_event"adb-tap 201 1760"
         elseif rows_mail_att_finder == "Two" then
            adb_event"adb-tap 153 1455"
         else
            prompt_user("Click OK to dismiss after you clicked OI File Manager manually.")
         end


         local target = file:gsub(".*[\\/]", "")
         target = "/sdcard/adb-mail/" .. i .. "." .. target
         adb_push{file, target}
         target = "../../../../../.." .. target
         putclip(target)

         wait_input_target(oiFileChooseActivity)
         local window = adb_focused_window()
         if window ~= oiFileChooseActivity then
            window = window:gsub("/.*", "")
            error("Must install and use OI File Manager, you are using: " .. window)
         end
         adb_event"sleep .5 key back key scroll_lock sleep .5"
         adb_event"adb-tap 959 1876 sleep 1"
      end
   end

   if paste_attachment_only then
      return
   end

   local insert_text = function(contact)
      if contact ~= "" then
         putclip(contact)
         adb_event"sleep .8 key scroll_lock sleep .5"
      end
      adb_event"key DPAD_DOWN"
   end
   adb_event"adb-tap 247 287"
   insert_text(to)
   insert_text(cc)
   insert_text(bcc)
   adb_event"key DPAD_DOWN"
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
   if not number:match("^[0-9]+$") then
      return weixin_find_friend(number)
   end
   adb_am("am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getcontact 1 --es contact " .. number)
end

t1_find_qq_contact = function(number)
   local contact_type
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
      t1_find_qq_contact(number)
   end
end

local press_dial_key = function()
   if not where_is_dial_key then
      where_is_dial_key = phone_info_map[phone_serial .. ":拨号键位置"]
      if not where_is_dial_key then
         where_is_dial_key = select_args{"Where is the dial button？", "Middle", "First from left", "Second from left"}
         phone_info_map[phone_serial .. ":拨号键位置"] = where_is_dial_key
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
      log("Error: unknown where_is_dial_key: %s, must be one of Middle, First from left, Second from left.\n\nPlease update %s", where_is_dial_key, configDir .. package.config:sub(1, 1) .. "phone_info.lua")
      where_is_dial_key = nil
   end
end

t1_call = function(number)
   if number:match("@@") then
      local names = split("@@", number)
      local who, where = names[1] or "", names[2] or ""
      if where == "qq" then
         t1_find_qq_contact(who)
      elseif where == "wx" then
         t1_find_weixin_contact(who)
      elseif where == "dd" then
         t1_find_dingding_contact(who)
      elseif where == "coffee" then
         get_coffee(who)
      elseif where == "mail" then
         search_mail(who)
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

t1_add_mms_receiver = function(number)
   while adb_is_window('com.android.mms/com.android.mms.ui.ComposeMessageActivity') do
      adb_event("key back sleep .1")
   end
   adb_am("am start -n com.android.mms/com.android.mms.ui.ComposeMessageActivity")

   putclip(number .. ',')

   adb_event("sleep 1 key scroll_lock")
   return "Please input your SMS in the Wrench Composer and click Send."
end

log = function(fmt, ...)
   if log_to_ui then
      log_to_ui(string.format(fmt, ...))
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
M.kill_android_vnc = kill_android_vnc
M.t1_find_weixin_contact = t1_find_weixin_contact
M.adb_shell = adb_shell
M.adb_pipe = adb_pipe
M.t1_picture = t1_picture
M.t1_follow_me = t1_follow_me
M.t1_share_to_weibo = t1_share_to_weibo
M.t1_share_to_weixin = t1_share_to_weixin
M.picture_to_weibo_share = picture_to_weibo_share
M.picture_to_weixin_share = picture_to_weixin_share
M.picture_to_momo_share = picture_to_momo_share
M.picture_to_qq_share = picture_to_qq_share
M.t1_spread_it = t1_spread_it
M.upload_pics = upload_pics
M.adb_start_weixin_share = adb_start_weixin_share
M.t1_config = t1_config
M.emoji_for_qq = emoji_for_qq
M.split = split
M.replace_img_with_emoji = replace_img_with_emoji
M.system = system
M.sleep = sleep
M.debugg = debug
M.get_a_note = get_a_note
M.t1_call = t1_call
M.t1_run = t1_run
M.t1_add_mms_receiver = t1_add_mms_receiver
M.t1_adb_mail = t1_adb_mail
M.t1_save_mail_heads = t1_save_mail_heads
M.adb_weixin_lucky_money = adb_weixin_lucky_money
M.adb_weixin_lucky_money_output = adb_weixin_lucky_money_output
M.adb_event = adb_event
M.t1_send_action = t1_send_action
M.t1_post2 = t1_post2
M.t1_find_qq_contact = t1_find_qq_contact
M.t1_share_to_qq = t1_share_to_qq
M.weixin_find_friend = weixin_find_friend
M.qq_open_homepage = qq_open_homepage

local function do_it()
   if arg and type(arg) == 'table' and string.find(arg[0], "wrench.lua") then
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
