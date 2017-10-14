#!/usr/bin/lua

-- module
local M = _ENV

local W = {}

M.ext_args = {}
M.ExtMods = {}
M.is_debugging = false
M.W = W

-- functions
local WrenchExt = {}
local search_sms, string_strip
local adb_input_method_is_null
local window_post_button_map = {}
local mail_group_map = {}
local phone_info_map = {}
local save_window_types
local phone_serial = ""
local configDir = "."
local last_uploaded_pics = {}
local file_exists
local social_need_confirm = false
local right_button_x = 984
local start_or_stop_recording, m_is_recording, current_recording_file

local wrench_call, wrench_run, wrench_adb_mail, wrench_save_mail_heads
local reset_input_method, adb_shell
local adb_push, adb_pull, adb_install
local shell_quote, putclip, wrench_post, push_text, wrench_post2
local adb_start_activity, launch_apps, on_app_selected
local picture_to_weixin_share, picture_to_weibo_share, picture_to_qq_share
local picture_to_momo_share, wrench_add_mms_receiver
local adb_get_input_window_dump, adb_top_window
local adb_start_weixin_share, adb_is_window
local check_phone
local weixin_find_friend, qq_find_friend, qq_find_group_friend
local emoji_for_qq, debug, get_a_note, emoji_for_weixin, emoji_rewrite, emoji_for_weibo
local adb_get_last_pic, debugging
local wrench_find_weixin_contact, wrench_find_qq_contact, wrench_find_dingding_contact
local find_weibo_friend
local adb_start_service_and_wait_file_gone
local adb_start_service_and_wait_file, adb_am
local wait_input_target, wait_top_activity, wait_top_activity_match
local wait_input_target_n
local wrench_eval, log, share_pics_to_app, share_text_to_app
local picture_to_weibo_comment
local check_scroll_lock, prompt_user, yes_or_no_p

-- variables
local where_is_dial_key
local rows_mail_att_finder
local UNAME_CMD = "uname || busybox uname || { echo -n Lin && echo -n ux; }"

local using_adb_root
local adb_unquoter
local is_windows = false
local debug_set_x = ""
M.default_width, M.default_height = 1080, 1920
M.init_width, M.init_height = 1080, 1920
M.real_width, M.real_height = 1080, 1920
M.app_width, M.app_height = 1080,1920

M.update_screen_ratios = function()
   M.real_width_ratio, M.real_height_ratio = M.real_width / M.default_width, M.real_height / M.default_height
   M.init_width_ratio, M.init_height_ratio = M.init_width / M.default_width, M.init_height / M.default_height
   M.app_width_ratio, M.app_height_ratio = M.app_width / M.default_width, M.app_height / M.default_height
end

M.update_screen_ratios()

local using_oppo_os = false
local brand = "smartisan"
local model = "Wrench"
local qq_emojis_remap, weixin_emojis_remap, weibo_emojis_remap
local sdk_version = 19
local emojis, img_to_emoji_map, emoji_to_img_map
local the_true_adb = "./the-true-adb"
local wrench_send_action

W.sms = "com.android.mms/com.android.mms.ui.ComposeMessageActivity"
W.weixinPackage = "com.tencent.mm/"
W.weibo_home_activity = "com.sina.weibo/com.sina.weibo.MainTabActivity"
W.weibo_search_activity = "com.sina.weibo/com.sina.weibo.page.SearchResultActivity"
W.smartisan_mail_compose = "com.android.email/com.android.mail.compose.ComposeActivity"
W.weixinAlbumPreviewActivity = "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.AlbumPreviewUI"
W.weixinChatActivity = "com.tencent.mm/com.tencent.mm.ui.chatting.ChattingUI"
W.weixinLauncherActivity = "com.tencent.mm/com.tencent.mm.ui.LauncherUI"
W.weixinSearchActivity = "com.tencent.mm/com.tencent.mm.plugin.search.ui.FTSMainUI"
W.weixinSnsUploadActivity = "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI"
W.weixinImagePreviewActivity = "com.tencent.mm/com.tencent.mm.plugin.gallery.ui.ImagePreviewUI"
W.weiboShareActivity = "com.sina.weibo/com.sina.weibo.composerinde.OriginalComposerActivity"
W.qqShareActivity = "com.qzone/com.qzonex.module.operation.ui.QZonePublishMoodActivity"
W.emailSmartisanActivity = "com.android.email/com.android.mail.compose.ComposeActivity"
W.oiFileChooseActivity = "org.openintents.filemanager/org.openintents.filemanager.IntentFilterActivity"
W.weiboCommentActivity = "com.sina.weibo/com.sina.weibo.composerinde.CommentComposerActivity"
W.weiboForwardActivity = "com.sina.weibo/com.sina.weibo.composerinde.ForwardComposerActivity"
W.qqChatActivity = "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity"
W.qqChatActivity2 = "com.tencent.mobileqq/com.tencent.mobileqq.activity.SplashActivity"
W.qqSplashActivity = W.qqChatActivity2
W.qqAlbumList = "com.tencent.mobileqq/com.tencent.mobileqq.activity.photo.AlbumListActivity"
W.qqCameraFlow = "com.tencent.mobileqq/com.tencent.mobileqq.activity.richmedia.FlowCameraPtvActivity2"
W.qqNewCameraFlow = "com.tencent.mobileqq/com.tencent.mobileqq.activity.richmedia.NewFlowCameraActivity"
-- W.qqGroupSearch = "com.tencent.mobileqq/com.tencent.mobileqq.search.activity.GroupSearchActivity"
W.qqGroupSearch = "com.tencent.mobileqq/com.tencent.mobileqq.search.activity.UniteSearchActivity"
W.qqPhotoFlow = "com.tencent.mobileqq/com.tencent.mobileqq.activity.photo.PhotoListFlowActivity"
W.qqPhotoPreview = "com.tencent.mobileqq/com.tencent.mobileqq.activity.photo.PhotoPreviewActivity"
W.qqPhoteList = "com.tencent.mobileqq/com.tencent.mobileqq.activity.photo.PhotoListActivity"
W.weiboAlbumActivity = "com.sina.weibo/com.sina.weibo.photoalbum.PhotoAlbumActivity"
W.weiboImagePreviewActivity = "com.sina.weibo/com.sina.weibo.photoalbum.ImagePagerActivity"
W.weiboPicFilterActivity = "com.sina.weibo/com.sina.weibo.photoalbum.PicFilterActivity"
W.weiboChatActivity = "com.sina.weibo/com.sina.weibo.weiyou.DMSingleChatActivity"
W.notePicPreview = "com.smartisanos.notes/com.smartisanos.notes.Convert2PicturePreviewActivity"

emojis = require"emojis"
img_to_emoji_map = {}
emoji_to_img_map = {}
for k, v in ipairs(emojis) do
   img_to_emoji_map[v[3]] = v[1]
   emoji_to_img_map[v[1]] = v[3]
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

reset_input_method = function()
   if phone_info_map['user_input_method'] then
      input_method_id = phone_info_map['user_input_method']
      local command = (
         [[
cd /data/data/com.android.shell
touch not-started.$$
setsid nohup sh -c 'set -x
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

if package.config:sub(1, 1) == '/' then
   if M.is_debugging then
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
   return emoji_rewrite(text, qq_emojis_remap)
end

emoji_rewrite = function(text, which_emojis)
   local s = 1
   local replace = ""
   repeat
      local fs, fe = text:find("%[.-%]", s)
      if fs then
         local emoji = text:sub(fs, fe)
         log("emoji is %s", emoji)
         if emoji_to_img_map[emoji] then
            replace = replace .. text:sub(s, fs - 1)
            if which_emojis[emoji] then
               replace = replace .. which_emojis[emoji]
            else
               replace = replace .. emoji
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
   return emoji_rewrite(text, weixin_emojis_remap)
end

emoji_for_weibo = function(text)
   return emoji_rewrite(text, weibo_emojis_remap)
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
   if M.is_debugging then
      log(fmt, ...)
   end
end

M.debugging = debugging

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
   for img in html:gmatch('img src="(.-)"') do
      debugging("img is %s", img)
      local emoji = img_to_emoji_map[img] or "[unknown emoji]"
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
adb_shell = function (cmds)
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
   return w == adb_top_window()
end

adb_start_activity = function(a)
   adb_am("am start -n " .. a)
end

M.adb_start_activity = adb_start_activity

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

M.adb_am = adb_am

M.adjust_x = function(x)
   return x * app_width_ratio
end

M.adjust_y = function(y)
   if y * 2 < default_height then
      return y * real_height_ratio
   else
      return y * app_height_ratio
   end
end

local function adb_event(events)
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
            height_ratio = init_height_ratio
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
         local add = ('input %s %d %d;'):format(action, events[i] * width_ratio, events[i+1] * height_ratio)
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

local function adb_tap_bot_left()
   adb_event{20, 1882}
end

local function adb_tap_mid_bot()
   adb_event{560, 1840}
end

local function sleep(time)
   adb_event(("sleep %s"):format(time))
end

prompt_user = function(fmt, ...)
   if select_args then
      return select_args{string.format(fmt, ...)}
   end
end

M.flatten_table = function(arg1, ...)
   local t1 = {arg1, ...}
   if #t1 == 1 and type(arg1) == 'table' then
      t1 = arg1
   end

   local t2 = {}
   for _, val in pairs(t1) do
      if type(val) == 'table' then
         for _, val2 in pairs(flatten_table(val)) do
            t2[#t2 + 1] = val2;
         end
      else
         t2[#t2 + 1] = val
      end
   end
   return t2
end

M.print_table = function(x)
   for _, val in pairs(x) do
      print(("%d: %s"):format(_, val))
   end
end

M.select_args = function(arg1, ...)
   if type(arg1) == 'table' then
      return select_from_args_table(arg1)
   else
      return select_from_args_table{arg1, ...}
   end
end

M.history_map = {}
M.history_loaded = false

M.member = function(elt, array)
  for _, value in pairs(array) do
    if value == elt then
      return true
    end
  end
  return false
end

M.load_history = function()
   local dofile_res, history_map = pcall(dofile, M.configDirFile("history.lua"))
   if dofile_res then
      M.history_map = history_map
   else
      M.history_map = {}
   end
   M.history_loaded = true
end

M.quote_string = function(str)
   local equals = ""
   while true do
      local try = "]" .. equals .. "]"
      if not (str .. "]"):match(try) then
         return "["..equals.."[" .. str .. try
      end
      equals = equals .. "="
   end
end

M.save_history = function()
   local history_file = io.open(M.configDirFile("history.lua"), "w")
   history_file:write("local map = {}\n")
   for k, v in spairs(M.history_map) do
      if k ~= "" then
         history_file:write(("\nmap[ %s ] = {\n"):format(M.quote_string(k)))
         local saved_vals = {}
         local length = 0
         for _, val in pairs(v) do
            if not saved_vals[val] and length < 100 then
               saved_vals[val] = 1
               history_file:write(("    %s,\n"):format(M.quote_string(val)))
               length = length + 1
            end
         end
         history_file:write(("}\n"))
      end
   end
   history_file:write("return map\n")
   history_file:close()
end

function table.slice(tbl, first, last, step)
  local sliced = {}

  for i = first or 1, last or #tbl, step or 1 do
    sliced[#sliced+1] = tbl[i]
  end

  return sliced
end

M.select_args_with_history = function(history_name, prompt, init_args, ...)
   if not M.history_loaded then
      M.load_history()
   end

   local args= {prompt, init_args, ...}

   if type(history_name) == 'table' and prompt == nil then
      history_name, args = history_name[1], table.slice(history_name, 2)
   end

   if not M.history_map[history_name] then
      M.history_map[history_name] = {}
   end

   for i = 1, #M.history_map[history_name] do
      if not M.member(M.history_map[history_name][i], args) then
         args[#args + 1] = M.history_map[history_name][i]
      end
   end

   local ret = M.select_args(args)

   M.history_map[history_name][#M.history_map[history_name] + 1] = ret
   M.save_history()
   return ret
end

M.prompt_user = prompt_user

yes_or_no_p = function(txt, ...)
   if select_args then
      return select_args{string.format(txt, ...)} ~= ""
   end
   return false
end

check_scroll_lock = function()
   local input_method, ime_height, ime_connected, current_input_method
   local function using_wrench_ime()
      input_method, ime_height, ime_connected, current_input_method = adb_get_input_window_dump()
      return current_input_method == 'com.wrench.inputmethod.pinyin'
   end

   while not using_wrench_ime() do
      local input_methods = adb_pipe"ime list -s"
      local input_method_id
      input_methods = split("\n", input_methods)
      for i = 1, #input_methods do
         log("checking inputh method %s", input_methods[i])
         if input_methods[i]:match(current_input_method .. "/") then
            input_method_id = input_methods[i]
            break
         end
      end

      log("input_method_id is %s", input_method_id )

      if input_method_id and input_method_id ~= phone_info_map['user_input_method'] then
         local old_input_method = "空(没有设置过)"
         if phone_info_map['user_input_method'] then
            old_input_method = phone_info_map['user_input_method']
         end
         if yes_or_no_p((
               "不使用小扳手的时候，你想把你默认的输入法自动改成当前正在使用的 %s 吗？（你原来默认的输入法是 %s）"
                        ):format(
               input_method_id,
               old_input_method
         )) then

            phone_info_map['user_input_method'] = input_method_id
            save_phone_info()
         end
      end
      reset_input_method()

      adb_shell"ime enable com.wrench.inputmethod.pinyin/.PinyinIME; ime set com.wrench.inputmethod.pinyin/.PinyinIME; sleep 1;"
      if using_wrench_ime() then
         break
      end
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
   adb_event{'key', 'scroll_lock'}
   if social_need_confirm and not yes_or_no_p("Confirm to share to weibo?") then
      return
   end
   if yes_or_no_p("Share to Weibo?") then
      adb_event{991, 166}
   end
end

local function wrench_share_to_weibo(text)
   share_text_to_app("com.sina.weibo", ".composerinde.ComposerDispatchActivity", text)
   wait_input_target(W.weiboShareActivity)
   if yes_or_no_p("Share to Weibo?") then
      wrench_send_action()
   end
end

local function wrench_share_to_qq(text)
   share_text_to_app("com.qzone", "com.qzonex.module.operation.ui.QZonePublishMoodActivity",  text)
   wait_input_target(W.qqShareActivity)
   if yes_or_no_p("分享到QQ空间？") then
      wrench_send_action()
   end
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

M.sdcard_putclip_path = "/sdcard/putclip.txt"
M.wait_top_activity_n = wait_top_activity_n

M.wait_top_activity_n_ok = function(n_retry, activity)
   local window = wait_top_activity_n(n_retry, activity)
   if window == activity then
      return true
   end
end

wait_top_activity = function(...)
   return wait_top_activity_n(20, ...)
end

M.wait_top_activity = wait_top_activity

wait_top_activity_match = function(activity)
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
   if not yes_or_no_p("等了两秒钟也没有等到 %s 窗口，请确认是否放弃此操作（重启Lua后台）？", activity) then
      error("用户取消了小扳手的操作")
   end
   return window
end

M.wait_top_activity_match = wait_top_activity_match

wait_input_target = function(...)
   return wait_input_target_n(20, ...)
end

M.wait_input_target = wait_input_target
M.wait_input_target_n_ok = function(n_loop, activity)
   return (wait_input_target_n(n_loop, activity)):match(activity)
end

wait_input_target_n = function(n_loop, ...)
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
               if adb_window_dump[x]:match("mInputMethodTarget.*" .. window) then
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

adb_top_window = function()
   -- dumpsys window|grep mFocusedWindow|perl -npe 's/.*?(\S+)}$/$1/')
   for i = 1, 50 do
      M.m_window_dump = adb_pipe("dumpsys window policy; dumpsys window windows") or ""
      M.m_focused_app = M.m_window_dump:match("mFocusedApp=Token.-(%S+)%s+%S+}") or ""
      M.m_focused_window = M.m_window_dump:match("mFocusedWindow=.-(%S+)}") or ""
      if M.m_focused_window ~= "" then
         -- mStableFullscreen=(0,0)-(1080,2160)
         local mStableFullscreen = M.m_window_dump:match("mStableFullscreen=[-%(%)%d,]+")
         if M.last_screen_size ~= mStableFullscreen then
            M.last_screen_size = mStableFullscreen
            app_width = M.last_screen_size:match('mStableFullscreen=.*%-%((%d+,%d+)%)')
            app_height = app_width:match(',(%d+)')
            app_width = app_width:match('(%d+),')
            app_width_ratio, app_height_ratio = app_width / default_width,  app_height / default_height
         end
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

M.swipe_down = function()
   adb_event"adb-no-virt-key-wrench-swipe 549 483 552 1778 adb-no-virt-key-wrench-swipe 549 483 552 1778 sleep 1"
end

local function search_mail(what)
   putclip_nowait(what)
   for i = 1, 5 do
      M.start_app"com.android.email/com.android.email.activity.Welcome"
      adb_event"adb-no-virt-key-wrench-swipe 549 483 552 778 sleep 1 adb-tap 595 324"
      if M.wait_input_target_n_ok(5, "^com.android.email/") then
         break
      elseif i == 5 then
         prompt_user("Can't get mail search input, check your adb events")
         return
      end
   end
   adb_event"key scroll_lock sleep .5 adb-tap 667 225 sleep .2 adb-tap 841 728"
end

M.adb_tap_1080x2160 = function (x, y, x1080, y1920)
   if real_height ~= 2160 or real_width ~= 1080 then
      x = x1080 or x
      y = y1920 or y
   end

   adb_event("adb-tap " .. x .. " " .. y)
end

local function get_coffee(what)
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

   if social_need_confirm and not yes_or_no_p("Will now find the 咕咕机 Wechat App") then
      return
   end
   adb_event"key scroll_lock sleep .1 key enter sleep .5 "
   if social_need_confirm and not yes_or_no_p("Will now open the 咕咕机 Wechat App") then
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
               tap_top_right() -- clear search text
               adb_event"sleep .1"
               if wait_input_target_n_ok(5, W.weixinSearchActivity) then
                  return
               end
            end

            log("exit from search by key back: %d %s ", i_search, top_window)
            if not wait_input_target_n_ok(5, W.weixinSearchActivity) then
               if i_search == 4 then
                  if yes_or_no_p("本次（第 %d 次）打开微信首页的自动操作没有点出微信搜索框，再试一次？", depth + 1)
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

string_strip = function(s)
   s = s:gsub("^%s+", "")
   s = s:gsub("%s+$", "")
   return s
end

search_sms = function(what)
   putclip_nowait(what)
   sms_activity = "com.android.mms/com.android.mms.ui.ConversationList"
   for i = 1, 5 do
      adb_start_activity(sms_activity)
      adb_event"sleep .5 adb-tap 516 284 sleep .5"
      local input = wait_input_target_n(5, sms_activity)
      if input == sms_activity then
         break
      end
      log("search_sms: wait input: %s", input)
      adb_event"key back"
   end
   adb_event"key scroll_lock key enter"
end

local function dingding_open_homepage()
   local dingding_splash = "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.SplashActivity"
   local dingding_home = "com.alibaba.android.rimet/com.alibaba.android.rimet.biz.home.activity.HomeActivity"
   adb_am("am start -n " .. dingding_splash)
   wait_top_activity_match("com.alibaba.android.rimet/")
   for i = 1, 20 do
      sleep(.1)
      local window = adb_top_window()
      if window then
         log("dd: window is %s at %d", window, i)
      end
      if window and window ~= dingding_splash and window ~= dingding_home then
         if window == "com.alibaba.android.rimet/com.alibaba.android.user.login.SignUpWithPwdActivity" then
            log("You need to sign in dingding")
            break
         end
         log("dd: window is %s at %d", window, i)
         adb_event"key back sleep .1"
         sleep(.1)
         adb_am("am start -n " .. dingding_splash)
         wait_top_activity_match("com.alibaba.android.rimet/")
      elseif window == dingding_splash then
         adb_event"adb-tap 863 222"
      elseif window == dingding_home then
         break
      end

   end
end

M.qq_open_search = function ()
   local max_qq_try = 5

   search_bar_y = 251
   if real_height == 2160 then
      search_bar_y = 204
   end
   
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
         prompt_user("在等 %s 的输入，但最后找到的是 %s，请检查 wrench.lua 脚本", W.qqGroupSearch, top_window)
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
            log("进入了QQ设置页面")
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
   if yes_or_no_p("没法打开QQ主页窗口，放弃？（会重启Lua后台）") then
      error("用户放弃操作")
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

local function wrench_share_to_weixin(text)
   debug("share to weixin: %s", text)
   weixinShareActivity = "com.tencent.mm/com.tencent.mm.plugin.sns.ui"
   if text then
      text = text:gsub("\n", "​\n")
      putclip_nowait(emoji_for_weixin(text))
   end
   adb_start_weixin_share('text')
   wrench_post(nil, 'top-right', "分享到微信朋友圈？")
end

local function weixin_text_share(window, text)
   if text then
      text = text:gsub("\n", "​\n")
      putclip(text)
   end
   adb_event("adb-key scroll_lock sleep .2")
   if yes_or_no_p("Share to wechat?") then
      adb_event(" adb-tap 1016 131")
   end
end

local function wrench_sms(window)
   adb_event"adb-tap 192 1227 sleep .5 adb-key scroll_lock"
   if yes_or_no_p("确认发送短信？") then
      adb_event"adb-tap 857 1008"
   end
end

local function wrench_google_plus(window)
   adb_event{467, 650, 'key', 'scroll_lock', 932, 1818}
end

local function wrench_smartisan_notes(window)
   adb_event{'key', 'scroll_lock', 940, 140, 933, 117, 323, 1272, 919, 123}
end

local function wrench_mail(window)
   if window == 'com.android.email/com.android.email.activity.Welcome' or window == 'com.android.email/com.android.email2.ui.MailActivityEmail' then
      adb_tap_mid_bot()
      wait_input_target(W.smartisan_mail_compose)
      sleep(.5)
   end
   adb_event("key scroll_lock sleep .5")
   if yes_or_no_p("确认发送邮件") then
      if window == 'com.google.android.gm/com.google.android.gm.ComposeActivityGmail' then
         adb_event{806, 178}
      else
         tap_top_right()
      end
   end
end

local function wrench_paste()
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
      ime_height = app_height * 0.105 * 4
   end
   -- log("ime_height = %d; real_height = %d; default_height = %d; napp_height = %d",
   --     ime_height, real_height, default_height, app_height)

   ime_height = ime_height * default_height / app_height
   M.ime_height = ime_height
   M.input_method_active = input_method_active

   local ime_connected = not (
      dump_str:match("mServedInputConnection=null") or
         dump_str:match("mStartedInputConnection=null")
   )
   return input_method_active, ime_height, ime_connected, current_input_method
end

adb_input_method_is_null = function ()
   -- if adb dumpsys input_method | grep mServedInputConnection=null -q; then
   local dump = adb_pipe{'dumpsys', 'input_method'}
   if dump:match("mServedInputConnection=null") or dump_str:match("mStartedInputConnection=null") then
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

M.check_phone = check_phone

adb_wait_file_gone = function(file)
   adb_shell(
      (
      [[
            for x in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 $(seq 1 80); do
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
            for x in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 $(seq 1 80); do
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
      local window = adb_top_window()
      if window:match("com.tencent.mobileqq") then
         text = emoji_for_qq(text)
      end
   end

   text = text:gsub("\n", "\r\n")
   local file, path
   local tmp = os.getenv("TEMP") or "/tmp"
   path = tmp .. package.config:sub(1, 1) .. "lua-smartisan-wrench.txt"
   file = io.open(path, "w")
   if not file then
      error("TEMP env not set")
   end
   file:write(text)
   file:close()
   check_phone()
   adb_push{path, M.sdcard_putclip_path}
end

putclip_nowait = function(text)
   push_text(text)
   adb_start_service('com.bhj.setclip/.PutClipService')
   M.need_wait_putclip = true
end

putclip = function(text)
   push_text(text)
   adb_start_service_and_wait_file_gone('com.bhj.setclip/.PutClipService', M.sdcard_putclip_path)
end

local check_file_push_and_renamed = function(file, md5, rename_to)
   if not rename_to then
      rename_to = file:gsub(".*/", "")
   end

   local md5_on_phone = adb_pipe(("if test -e /data/data/com.android.shell/%s; then cat /sdcard/%s; fi"):format(rename_to, md5))
   md5_on_phone = md5_on_phone:gsub("\n", "")
   local md5file = io.open(md5)
   if not md5file then
      prompt_user("%s 文件打开失败，小扳手可能无法正常运行，请联系作者报Bug（参考 http://baohaojun.github.io/blog/2014/12/01/0-T1Wrench-2.0-Usage-Guide.html#bugs-howto ）", md5)
      return
   end

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

local check_apk_installed = function(apk, md5, reason)
   local md5_on_phone = adb_pipe("cat /sdcard/" .. md5)
   md5_on_phone = md5_on_phone:gsub("\n", "")
   local md5file = io.open(md5)
   local md5_on_PC = md5file:read("*a")
   md5_on_PC = md5_on_PC:gsub("\n", "")
   io.close(md5file)
   debugging("on phone: %s, local: %s", md5_on_phone, md5_on_PC)
   if md5_on_phone ~= md5_on_PC then
      prompt_user(reason .. "\n\n如有必要，请在你的手机上确认允许安装")
      log("Need to install on your phone Wrench helper App %s, please make sure your phone allows it.", apk)
      local install_output = adb_install(apk)
      if install_output:match("\nSuccess") or install_output:match("^Success") then
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

if not wrench_set_variable then
   wrench_set_variable = function(name, val)
   end
end

M.qx = function(command)
   local p = io.popen(command)
   local v = p:read("*a")
   return v
end

M.dataDirFile = function(file)
   return M.dataDir .. file
end

M.configDirFile = function(file)
   return M.configDir .. file
end

local get_xy_from_dump = function(dump, prefix)
   local xy_match = dump:match(prefix .. '=(%d+x%d+)')
   local height = tonumber(xy_match:match('x(%d+)'))
   local width = tonumber(xy_match:match('(%d+)x'))
   return width, height
end

M.update_screen_size = function()
   local dump = adb_pipe{'dumpsys', 'window'}

   real_width, real_height = get_xy_from_dump(dump, "cur")
   init_width, init_height = get_xy_from_dump(dump, "init")

   app_width = dump:match('mStableFullscreen=.*%-%((%d+,%d+)%)')
   app_height = app_width:match(',(%d+)')
   app_width = app_width:match('(%d+),')
   if app_width > app_height and
      (
         WrenchExt.getConfig("force-portrait") == 1 or
            yes_or_no_p("Wrench found your phone screen maybe rotated, correct it? " .. app_width .. "x" .. app_height)
      )
   then
      log("Force portrait mode")
      app_width, app_height = app_height, app_width
   end
   update_screen_ratios()
   log("app_width_ratio is %f, app_height_ratio is %f ", app_width_ratio, app_height_ratio)

   if app_width ~= default_width then
      right_button_x = 1080 - 80 * default_width / app_width
   end
end

M.wrench_config = function(passedConfigDirPath)
   if passedConfigDirPath then
      configDir = passedConfigDirPath
   end
   M.configDir = configDir .. package.config:sub(1, 1)
   M.dataDir = os.getenv("WRENCH_DATA_DIR") .. package.config:sub(1, 1)

   M.ExtMods = wrench_run("ext/.ls-modules.lua")
   if not M.ExtMods then
      log("无法加载 ext/.ls-modules.lua")
   end


   -- install the apk
   if not qt_adb_pipe then
      local p = io.popen("the-true-adb version")
      local v = p:read("*a")
      if v:match("1.0.31") then
         adb_unquoter = '\\"'
      end
   end

   adb_shell"mkdir -p /sdcard/Wrench"
   local uname = adb_pipe(UNAME_CMD)

   if not uname:match("Linux") then
      error("Phone uname is not Linux")
   end

   if not file_exists(M.dataDirFile("apps.info")) then
      M.update_apps()
   end

   check_apk_installed("Setclip.apk", "Setclip.apk.md5", "需要安装小扳手辅助 App")
   check_file_pushed("am.jar", "am.jar.md5")
   check_file_pushed("busybox", "busybox.md5")

   sdk_version = adb_pipe("getprop ro.build.version.sdk")
   brand = adb_pipe("getprop ro.product.brand"):gsub("\n.*", "")
   adb_serial = adb_pipe("getprop ro.serialno")
   model = adb_pipe("getprop ro.product.model"):gsub("\n.*", "")
   arm_arch = adb_pipe("/data/data/com.android.shell/busybox uname -m 2>/dev/null || uname -m")
   androidvncserver = ("androidvncserver-%s.sdk%s"):format(arm_arch, sdk_version)

   codename = adb_pipe("getprop ro.product.codename"):gsub(" ", ".")
   codenamed_vnc = ("%s-%s"):format(androidvncserver, codename)
   if file_exists(codenamed_vnc) then
      androidvncserver = codenamed_vnc
   end

   log("androidvncserver is %s", androidvncserver)

   if file_exists(androidvncserver) then
      check_file_push_and_renamed(androidvncserver, androidvncserver ..  ".md5", "androidvncserver")
      wrench_set_variable("using-vnc", "true")
   else
      if wrench_get_proc_var{"vnc-warning"} ~= "done" then
         prompt_user("没有找到跟你的手机系统 sdk 版本匹配的 %s，无法使用流畅手机屏幕同步。\n\n只能通过对屏幕截屏来同步显示，效率较低。\n\n请查看小扳手说明书了解详情。", androidvncserver)
         wrench_set_proc_var("vnc-warning", "done")
      end
      log("没有找到跟手机系统版本匹配的 %s，无法使用流畅手机屏幕同步（请找作者报个 Bug，或自己编译 androidvncserver。",
          androidvncserver)
      wrench_set_variable("using-vnc", "false")
   end

   debugging("sdk is %s\nbrand is %s\nmodel is %s\n", sdk_version, brand, model)
   sdk_version = tonumber(sdk_version)
   if tonumber(sdk_version) < 16 then
       error("Error, you phone's sdk version is " .. sdk_version .. ",  must be at least 16")
   end

   update_screen_size()

   local id = adb_pipe("id")
   if id:match("uid=0") then
      using_adb_root = true
   else
      using_adb_root = false
   end

   check_apk_installed("WrenchIME.apk", "WrenchIME.apk.md5", "需要安装小扳手输入法")

   local dofile_res
   dofile_res, mail_group_map = pcall(dofile, M.configDirFile("mail_groups.lua"))
   if not dofile_res then
      mail_group_map = {}
   end

   dofile_res, window_post_button_map = pcall(dofile, M.configDirFile("window_post_botton.lua"))
   if not dofile_res then
      dofile_res, window_post_button_map = pcall(dofile, "window_post_botton.lua")
      if not dofile_res then
         window_post_button_map = {}
         save_window_types()
      end
   end

   dofile_res, phone_info_map = pcall(dofile, M.configDirFile("phone_info.lua"))
   if not dofile_res then
      log("phone info failed")
      phone_info_map = {}
      save_phone_info()
   end

   phone_serial = adb_pipe("getprop ro.serialno"):gsub("\n", "")
   reset_input_method()
   return ("brand is %s, adb serial is %s"):format(brand, adb_serial)
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
   if wait_top_activity(W.notePicPreview) ~= W.notePicPreview then
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
   adb_start_service_and_wait_file("com.bhj.setclip/.PutClipService --ei get-last-note-pic 1", M.sdcard_putclip_path)
   local pic = adb_pipe("cat " .. M.sdcard_putclip_path)
   pic = pic:gsub('^/storage/emulated/0', '/sdcard')
   adb_pull{pic, ("%slast-pic-%s.png"):format(M.writableDir, which)}
   if remove then
      adb_shell{rm, pic}
      adb_am(("am startservice --user 0 -n com.bhj.setclip/.PutClipService --es picture %s"):format(pic))
   end
end

wrench_post2 = function(texwrench, text2)
   putclip(texwrench)
   adb_event("key scroll_lock key dpad_down")
   wrench_post(text2)
end

weixin_find_friend = function(friend_name, depth)
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

wrench_find_dingding_contact = function(friend_name)
   dingding_open_homepage()
   adb_event"adb-tap 770 105"
   putclip(friend_name)
   wait_input_target("com.alibaba.android.rimet/com.alibaba.android.search.activity.GlobalSearchInputActivity")
   adb_event"sleep .2 key scroll_lock key enter sleep .8"

   adb_event"adb-tap 276 354 sleep .8 adb-tap 154 663"
end

find_weibo_friend = function(friend_name)
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

qq_find_friend = function(friend_name)
   putclip_nowait(friend_name)
   local clear_button = "adb-tap 804 167"
   if real_height == 2160 then
      clear_button = "adb-tap 833 90"
   end

   local first_result_xy = "adb-tap 544 558"
   if real_height == 2160 then
      first_result_xy = "adb-tap 415 331"
   end
   
   log("qq find friend: %s", friend_name)
   for i = 1, 5 do
      qq_open_search()
      local top_window = wait_input_target_n(5, W.qqChatActivity2, W.qqGroupSearch)
      adb_event("adb-tap 296 1594 adb-tap 522 1842 sleep .1 " .. clear_button .. " sleep .1 key scroll_lock sleep .5") -- clear the search first by input "x " and click ⓧ.
      if top_window and top_window:match(W.qqGroupSearch) then
         for click_search_res = 1, 3 do
            log"Found W.qqGroupSearch"
            adb_event(first_result_xy .. " sleep .2")
            if adb_top_window() ~= W.qqGroupSearch then
               log("Found the qq friend %s", friend_name)
               return
            elseif click_search_res == 3 then
               yes_or_no_p("小扳手好像没有找到你想找的 QQ 好友（%s），请自己手动点一下...", friend_name)
               return
            end
         end
      else
         if i > 1 and not yes_or_no_p("没有找到 QQ 好友 %s，再试一遍？", friend_name) then
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

qq_find_group_friend = function(friend_name)
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
   local group_list_button = "adb-tap 426 1540"
   if real_height == 2160 then
      group_list_button = "adb-tap 404 1167"
   end
   adb_event("sleep 1 " .. group_list_button) -- 点击进入群成员列表
   local troopList = "com.tencent.mobileqq/com.tencent.mobileqq.activity.TroopMemberListActivity"
   window = wait_top_activity(troopList)
   if window ~= troopList then
      prompt_user("没有点到QQ群成员列表页面，可能是小扳手的座标出了点问题，请用录屏功能调整一下座标")
      return
   end

   local troop_list_search = "adb-tap 663 252"
   if real_height == 2160 then
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
   if real_height == 2160 then
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
   if real_height == 2160 then
      send_msg_button = "adb-tap 894 1872"
   end
   adb_event("sleep " .. .5 * i .. send_msg_button)
end

save_window_types = function()
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

M.save_phone_info = function()
   local infofile = io.open(M.configDirFile("phone_info.lua"), "w")
   infofile:write("local map = {}\n")

   local tkeys = {}
   for k in pairs(phone_info_map) do table.insert(tkeys, k) end
   table.sort(tkeys)
   for _, k in ipairs(tkeys) do 
      infofile:write(("map['%s'] = '%s'\n"):format(k, phone_info_map[k]))
   end
   infofile:write("return map\n")
   infofile:close()
end

M.kill_android_vnc = function()
   adb_shell"killall -INT androidvncserver || busybox killall -INT androidvncserver"
end

M.file_exists = function(name)
   local f=io.open(name,"r")
   if f ~= nil then
      io.close(f)
      return true
   else
      return false
   end
end

file_exists = M.file_exists

M.get_app_table = function()
   apps_file = io.open(M.dataDirFile("apps.info"))
   local apps_txt = apps_file:read("*a")
   local apps = split("\n", apps_txt)
   local app_table = {}
   for i = 1, #apps do
      line = apps[i]
      local s = split("=", line)
      local class_ = s[1]
      local package_ = s[2]
      app_table[class_] = package_ .. "/" .. class_
      app_table[package_] = package_ .. "/" .. class_
      local label_ = s[3]
      if not file_exists(M.dataDirFile(class_ .. ".png")) then
         adb_pull{"/sdcard/Wrench/" .. class_ .. ".png", M.dataDirFile(class_ .. ".png")}
      end
   end
   apps_file.close()
   return app_table
end

M.update_apps = function()
   if adb_start_service_and_wait_file("com.bhj.setclip/.PutClipService --ei listapps 1", "/sdcard/Wrench/apps.info") then
      adb_pull{"/sdcard/Wrench/apps.info", M.dataDirFile("apps.info")}
   else
      log("Can't get apps.info")
   end
   M.get_app_table()
end

launch_apps = function()
   if not file_exists(M.dataDirFile("apps.info")) then
      M.update_apps()
   end
   select_apps()
end

on_app_selected = function(app)
   local app_table = M.get_app_table()
   if app ~= "" then
      log("starting: %s", app_table[app])
      adb_start_activity(app_table[app])
   end
end

local dofile_res = nil
dofile_res, WrenchExt = pcall(dofile, "wrench-ext.lua")
if not dofile_res then
   WrenchExt = {}
end

local post_weibo_answer = function(text)
   texts = split("\n\n", text)
   for _, p in ipairs(texts) do
      putclip(p)
      adb_event("sleep 1 key scroll_lock sleep .2 key enter sleep .5")
   end
end

local shouldNotPostActivitys = {
   "com.tencent.mobileqq/com.tencent.mobileqq.activity.aio.photo.AIOGalleryActivity",
   "com.tencent.mm/com.tencent.mm.ui.chatting.gallery.ImageGalleryUI",
   "StatusBar",
}

local function postAfterBackKey(window)
   for _, w in ipairs(shouldNotPostActivitys) do
      if window == w then
         adb_event"key back sleep .2"
         wrench_post()
         return true
      end
   end
   return false
end

start_or_stop_recording = function()
   if not m_is_recording then
      m_is_recording = M.select_args{"请输入你想录制的文件名（例：SearchKindle）?", "", ""}
      if (m_is_recording ~= "") then
         m_is_recording = m_is_recording:gsub("[^a-z0-9_A-Z]", "_")
         m_is_recording = M.configDirFile("ext" .. package.config:sub(1, 1) .. m_is_recording .. ".lua")
         local headString = select_args{"请输入你对本次录制功能的描述（例：在Kindle里搜书）", "", " "}
         if headString == "" then
            prompt_user("必须对录制的功能进行描述，后续使用时才能正确显示")
            m_is_recording = nil
            return
         end
         local start_app = adb_top_window()
         local record_file = io.open(m_is_recording, "a")
         record_file:write("#!/usr/bin/env Wrench.sh\n")
         record_file:write(("-- %s\n\n"):format(headString))
         record_file:write(('M.start_app"%s"\n\n'):format(start_app))
         record_file:close()

      else
         prompt_user("必须输入一个文件名字才可以录制")
         m_is_recording = nil
      end
   else
      prompt_user(("你的屏幕操作已经录制在 %s 文件中，请对其进行一些编辑，然后点一下小扳手设置按钮，以便可以使用此功能"):format(m_is_recording))
      m_is_recording = nil
   end
end

M.call_ext = function(ext, ...)
   M.ext_args = {...}
   wrench_run("ext" .. package.config:sub(1, 1) .. ext .. ".lua")
   M.ext_args = {}
end

M.set_ext_args = function(...)
   M.ext_args = {...}
end

M.start_app = function(to_start, to_find)
   
   if not to_start:match("/") then
      pkg = to_start
      local app_table = M.get_app_table()
      if app_table[pkg] then
         to_start = app_table[pkg]
      end
   end

   pkg = to_start:gsub("/.*", "")
   adb_am{'am', 'force-stop', pkg}
   adb_start_activity(to_start)
   wait_top_activity_match("^" .. pkg)
   sleep(.5)
end

M.M = M

M.ask_for_window_type = function(window)
   window_type = select_args{'Where is the send button for ' .. window,
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
   return window_type
end

wrench_post = function(text, how_to_post, confirm_before_post) -- use weixin
   local window = adb_top_window()
   debug("sharing text: %s for window: %s", text, window)
   if text and text:match("^@%?") then
      wrench_post("@", 'manual-post')
      prompt_user("请选择你要@谁，然后继续")
      text = text:gsub("^@%?", "")
      how_to_post = 'manual-post'
   end

   if text and text:match("@%?$") then
      text = text:gsub("@%?$", "")
      wrench_post(text, 'manual-post')
      wrench_post("@", 'manual-post')
      return
   end

   if text then
      if text:match("@$") and not how_to_post then
         how_to_post = 'manual-post'
      end

      if text:match("^​") and text ~= "​" then
         text = text:sub(string.len("​") + 1)
         local func = loadstring(text)
         wrench_eval(func)
         return "executed string"
      end
      if text:match("^#!lua") and text ~= "#!lua" then
         text = text:sub(string.len("#!lua") + 1)
         local func = loadstring(text)
         wrench_eval(func)
         return "executed string"
      end

      if window:match("com.tencent.mobileqq") then
         putclip(emoji_for_qq(text))
      elseif window:match("com.tencent.mm/") then
         text = text:gsub("\n", "​\n")
         putclip(emoji_for_weixin(text))
      elseif window:match("com.sina.weibo/") then
         putclip(emoji_for_weibo(text))
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
   elseif window == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsUploadUI" or window == "com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsCommentUI" then
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
      local add, post_button = '', right_button_x .. ' 1850'
      local input_method, ime_height, ime_connected = adb_get_input_window_dump() -- $(adb dumpsys window | perl -ne 'print if m/^\s*Window #\d+ Window\{[a-f0-9]* u0 InputMethod\}/i .. m/^\s*mHasSurface/')
      log("input_method is %s, ime_xy is %s, ime_connected is %s", input_method, ime_height, ime_connected)
      -- debugging("ime_xy is %s", ime_xy)

      if input_method then
         add = "key BACK"
      else
         add = "" -- # add="560 1840 key DEL key BACK"
      end
      if input_method then
         if ime_height ~= 0 then
            add = ''
            post_button = ('%d %d'):format(right_button_x, 1920 - ime_height - 80)
         end
      else
         if not ime_connected then
            if window == W.qqChatActivity2 or window == W.qqChatActivity then
               adb_event("540 1740")
            else
               adb_event("540 1840")
            end
            wait_input_target(window)
            if not adb_top_window():match("^PopupWindow") then
               adb_event("key back")
               for n = 1, 5 do
                  local input_method, ime_height, ime_connected = adb_get_input_window_dump()
                  -- log("ime_height is %d: %d", ime_height, n)
                  if ime_height == 0 then
                     adb_top_window() -- make sure we know that the nav bar is gone.
                     break
                  else
                     sleep(.2 * n)
                  end
               end
            end
            add = ''
         end
      end

      if window == "com.github.mobile/com.github.mobile.ui.issue.CreateCommentActivity" then
         post_button = right_button_x .. ' 166'
      end

      local window_type = (how_to_post or window_post_button_map[window])
      if not window_type and (
         window:match("^com.tencent.mm/com.tencent.mm.ui.chatting.") or -- weixin chat with a friend open from group members
            window:match("^com.tencent.mm/com.tencent.mm.plugin.sns.ui.") -- weixin moments comment
      ) then
         window_type = 'weixin-chat'
      end

      if not window_type then
         window_type = ask_for_window_type(window)
      end
      if window_type == 'weixin-chat' then
         post_button = post_button -- empty
      elseif window_type == 'qq-chat' then
         local qq_button_sub = 200
         if real_height == 2160 then
            qq_button_sub = 150
         end
         post_button = ('%d %d'):format(right_button_x, 1920 - ime_height - qq_button_sub)
         -- log("ime_height is %d, post_button is adb_event'adb-long-press %s'", ime_height, post_button)
      elseif window_type == 'weixin-confirm' then
         post_button = post_button
      elseif window_type == 'weibo-share' or window_type == 'top-right' then
         post_button = right_button_x .. ' 150'
      elseif window_type == 'weibo-confirm' then
         post_button = right_button_x .. ' 150'
      elseif window_type == 'manual-post' then
         post_button = ''
      end

      debugging("add is %s", add)
      adb_event(add .. " key scroll_lock")
      if confirm_before_post and not yes_or_no_p(confirm_before_post) then
         return ""
      end

      if window_type:match("confirm") and not yes_or_no_p(("确认发送？（发送按钮位置与 %s 类似）"):format(window_type:gsub("-confirm", ""))) then
         return ""
      end

      if post_button ~= "" then
         adb_event(post_button)
      end
   end
   return "text sent"
end

wrench_send_action = function()
   wrench_post('​')
end

local function upload_pics(...)
   local pics = {...}
   if #pics == 0 then
      return last_uploaded_pics
   end

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

   sleep(.5)
   for try = 1, 5 do
      if adb_top_window() == "smartisanos/smartisanos.app.DoppelgangerChooseActivity" then
         adb_event"sleep .1 adb-tap 370 1727 sleep .1"
      else
         return
      end
   end
end

picture_to_weixin_share = function(pics, ...)
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

picture_to_qq_share = function(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
   log("share to qq")
   share_pics_to_app("com.qzone", "com.qzonex.module.operation.ui.QZonePublishMoodActivity", pics)
   wait_top_activity(W.qqShareActivity)
   adb_event("adb-tap 228 401")
   wait_input_target(W.qqShareActivity)
end

picture_to_weibo_share = function(pics, ...)
   if pics == nil then
      pics = last_uploaded_pics
   end
   share_pics_to_app("com.sina.weibo", ".composerinde.ComposerDispatchActivity", pics)
   wait_top_activity(W.weiboShareActivity)
   adb_event("adb-tap 162 286")
   wait_input_target(W.weiboShareActivity)
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

      if real_height == 2160 then
         pic_share_buttons = {
            "adb-tap 308 211", "adb-tap 668 223", "adb-tap 1013 217",
            "adb-tap 306 544", "adb-tap 656 556", "adb-tap 1012 550",
            "adb-tap 299 882", "adb-tap 663 882", "adb-tap 1036 894",
         }
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
            prompt_user("没有等到 weixinImagePreviewActivity，无法完成此操作")
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

M.tap_top_right = function()
   if real_height == 2160 then
      adb_event"adb-tap 1024 99"
   else
      adb_event"adb-tap 1012 151"
   end
end

M.tap_bottom_center = function()
   adb_event"adb-tap 592 1836"
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

M.exit_ime = function()
   local input_active, ime_height = adb_get_input_window_dump()
   log("exit ime: %s %s", ime_active, ime_height)
   if ime_height ~= 0 then
      adb_event("key back sleep .1 key back sleep .1")
      adb_top_window() -- for mix2, update the app_width (the nav bar comes with ime).
      return
   end

   if ime_active then
      adb_event("key back sleep .1")
   end
   adb_top_window()
end

M.close_ime = function()
   local input_method, ime_height = adb_get_input_window_dump()
   if (ime_height ~= 0) then
      ime_height = 0
      log("Send the back key to hide IME.")
      adb_event("key back")
   end
   return input_method, ime_height
end

local function click_to_album_wx_chat_style(evenwrench, activity1, ...)
   local input_method, ime_height = close_ime()
   local post_button = ('%d %d'):format(right_button_x, 1920 - 50)
   local old_top_window = adb_top_window()

   adb_event(post_button .. " sleep .2 " .. evenwrench)
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
   if wait_top_activity_n(2, old_top_window) ~= old_top_window then
      log"Can't get old dd chat window"
   end
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

   prompt_user("试了很多次，还是在 %s 里，小扳手的自动化脚本可能有问题，请检查一下", adb_top_window())
   error("Failed to get out of %s", join(", ", windows))
end

M.need_confirm = function(fmt, ...)
   if yes_or_no_p(fmt, ...) then
      return true
   else
      return false
   end
end

local function picture_to_qq_chat(pics, ...)
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
                  prompt_user("小扳手自动化脚本没有点到QQ像册界面，无法继续执行发图片功能")
                  return
               else -- qqAlbumList
                  adb_event("sleep .5 adb-tap 329 336") -- 点到第一个像册里去
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

      if real_height == 2160 then
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
   if real_height == 2160 then
      original_pic_button = 'adb-tap 522 1859'
   end

   adb_event("sleep .1 " .. original_pic_button)
   if need_confirm("请确认是不是要发送这些图片？") then
      adb_event"adb-tap 898 1840"
      wait_top_activity(chatWindow)
      adb_event("key back")
   end
end

local function picture_to_weibo_chat(pics, ...)
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

local function wrench_picture(...)
   local pics = upload_pics(...)
   local window = adb_top_window()
   if window == W.weixinLauncherActivity and yes_or_no_p("发送给当前微信聊天窗口") then
      return picture_to_weixin_chat(pics)
   elseif window == "com.tencent.mm/com.tencent.mm.plugin.scanner.ui.BaseScanUI" then
      return M.picture_to_weixin_scan(pics)
   elseif window:match("^com.tencent.mm/com.tencent.mm.ui.chatting") then
      if yes_or_no_p("发送给当前微信聊天窗口") then
         return picture_to_weixin_chat(pics)
      end
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity" then
      if yes_or_no_p("发送给当前QQ聊天窗口") then
         return picture_to_qq_chat(pics)
      end
   elseif window == "com.alibaba.android.rimet/com.alibaba.android.dingtalkim.activities.ChatMsgActivity" then
      if yes_or_no_p("发送给当前钉钉聊天窗口") then
         return picture_to_dingding_chat(pics)
      end
   elseif window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.SplashActivity" then
      if yes_or_no_p("发送给当前QQ聊天窗口") then
         return picture_to_qq_chat(pics)
      end
   elseif window == "com.sina.weibo/com.sina.weibo.weiyou.DMSingleChatActivity" then
      if yes_or_no_p("发送给当前微博私信聊天窗口") then
         return picture_to_weibo_chat(pics)
      end
   elseif window == W.weiboCommentActivity or window == W.weiboForwardActivity then
      if yes_or_no_p("发送给当前微博回复窗口") then
         return picture_to_weibo_comment(pics)
      end
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

wrench_save_mail_heads = function(file, subject, to, cc, bcc, attachments)
   local f = io.open(file, "w")
   f:write(('wrench_load_mail_heads([[%s]], [[%s]], [[%s]], [[%s]], [[%s]])'):format(subject, to, cc, bcc, attachments))
   f:close()
   debugging("hello saving to %s wrench_save_mail_heads", file)
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

wrench_adb_mail = function(subject, to, cc, bcc, attachments)
   if subject ~= "" and file_exists(os.getenv("HOME") .. "/src/github/private-config/bin/wrench-thunderbird") then
      if yes_or_no_p("Do you want to use thunderbird?") then
         system{"wrench-thunderbird", subject, to, cc, bcc, attachments}
         return
      end
   end

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

      adb_am("am start -n " .. W.emailSmartisanActivity .. " mailto:")
      adb_shell"mkdir -p /sdcard/adb-mail"
      wait_input_target(W.emailSmartisanActivity)

      adb_tap_1080x2160(364, 246)
      adb_event("sleep 0.5") -- 展开
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
            adb_event"adb-tap 1050 810 sleep .5"
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

         wait_input_target(W.oiFileChooseActivity)
         local window = adb_top_window()
         if window ~= W.oiFileChooseActivity then
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
      adb_event"key enter sleep .5"
   end

   adb_event"key enter sleep 1.5"
   insert_text(subject)

   adb_tap_1080x2160(415, 357)
   adb_tap_1080x2160(370, 252)
   insert_text(to)
   insert_text(cc)
   insert_text(bcc)

   adb_event"key DPAD_DOWN key DPAD_DOWN"
end

wrench_find_weixin_contact = function(number)
   if not number:match("^[0-9]+$") then
      return weixin_find_friend(number)
   end
   adb_am("am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getcontact 1 --es contact " .. number)
end

wrench_find_qq_contact = function(number)
   local contact_type
   if number == "" then
      number = string_strip(M.select_args_with_history("qq-contact", "请输入 QQ_FRIEND_NAME 或 QQ_USER@QQ_GROUP", "", " "))
      if number == "" then
         prompt_user("没有输入你想查找的QQ联系人，无法查找")
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
      log("Error: unknown where_is_dial_key: %s, must be one of Middle, First from left, Second from left.\n\nPlease update %s", where_is_dial_key, M.configDirFile("phone_info.lua"))
      where_is_dial_key = nil
   end
end

wrench_call = function(number)
   if number:match("@@") then
      number = string_strip(number)
      local names = split("@@", number, true)
      local who, where = names[1] or "", names[2] or ""
      if where == "qq" then
         wrench_find_qq_contact(who)
      elseif where == "wx" then
         wrench_find_weixin_contact(who)
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
         wrench_eval(func)
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

wrench_add_mms_receiver = function(number)
   while adb_is_window(W.sms) do
      adb_event("key back sleep .1")
   end
   adb_am("am start -n " .. W.sms)

   putclip(number .. ',')

   adb_event("sleep 1 key scroll_lock")
   return "Please input your SMS in the Wrench Composer and click Send."
end

M.send_sms = function(sms, someone)
   adb_am("am start -n " .. W.sms)
   putclip(someone)
   adb_event("sleep .2 adb-tap 203 291 sleep .2 key scroll_lock")

   putclip(sms)

   adb_event("adb-tap 568 1088 sleep .2 key scroll_lock")
end

log = function(fmt, ...)
   if log_to_ui then
      log_to_ui(string.format(fmt, ...))
   end
end

if log_to_ui then
   M.log_to_ui = log_to_ui
end

wrench_eval = function(f)
   return f()
end

wrench_run = function (file)
   local ext = file:gsub(".*%.", "")
   if ext ~= "twa" and ext ~= "小扳手" and ext ~= "lua" then
      return "Can not run this script, must be a .twa file"
   end
   local f = loadfile(file)
   return wrench_eval(f)
end

M.wrenchThumbUp = function()
   if not M.ExtMods.descriptions then
      prompt_user("扩展程序加载失败，无法执行此操作")
      return
   end
   local x = select_args(M.ExtMods.descriptions)
   if x == "" then
      return
   end
   if not M.ExtMods.description_to_filename[x] then
      prompt_user("你选择的操作（" .. x .. "）没有对应的扩展脚本，是否忘了更新ext/.modules.lua？")
      return
   end

   if not M.ExtMods.description_to_loaded_func[x] then
      M.ExtMods.description_to_loaded_func[x] = loadfile(M.ExtMods.description_to_filename[x])
   end

   wrench_eval(M.ExtMods.description_to_loaded_func[x])
end

M.shift_click_notification = function(key, pkg, title, text)
   log("click %s, %s, %s, %s", key, pkg, title, text)
   if pkg == "com.tencent.mm" then
      title = title:gsub("%.", " ")
      wrench_call(title .. "@@wx")
   elseif pkg == "com.tencent.mobileqq" then
      local sender = ""
      if title:lower() == "qq" then
         sender = text:gsub(":.*", "")
         sender = sender:gsub(".-%((.-)%).*", "%1")
      else
         title = title:gsub(" %(%d+条新消息%)$", "")
         title = title:gsub(" %(%d+条以上新消息%)$", "")
         sender = title or ""
      end

      sender = sender:gsub("%.", " ")
      wrench_call(sender .. "@@qq")
   end
end

M.log = log
M.open_weixin_scan = open_weixin_scan
M.adb_get_input_window_dump = adb_get_input_window_dump
M.putclip = putclip
M.start_weibo_share = start_weibo_share
M.wrench_post = wrench_post
M.launch_apps = launch_apps
M.on_app_selected = on_app_selected
M.wrench_find_weixin_contact = wrench_find_weixin_contact
M.adb_shell = adb_shell
M.adb_pipe = adb_pipe
M.wrench_picture = wrench_picture
M.wrench_follow_me = wrench_follow_me
M.wrench_share_to_weibo = wrench_share_to_weibo
M.wrench_share_to_weixin = wrench_share_to_weixin
M.picture_to_weibo_share = picture_to_weibo_share
M.picture_to_weixin_share = picture_to_weixin_share
M.picture_to_momo_share = picture_to_momo_share
M.picture_to_qq_share = picture_to_qq_share
M.wrench_spread_it = wrench_spread_it
M.upload_pics = upload_pics
M.adb_start_weixin_share = adb_start_weixin_share
M.emoji_for_qq = emoji_for_qq
M.split = split
M.replace_img_with_emoji = replace_img_with_emoji
M.system = system
M.sleep = sleep
M.debugg = debug
M.get_a_note = get_a_note
M.wrench_call = wrench_call
M.wrench_run = wrench_run
M.wrench_add_mms_receiver = wrench_add_mms_receiver
M.wrench_adb_mail = wrench_adb_mail
M.wrench_save_mail_heads = wrench_save_mail_heads
M.adb_event = adb_event
M.wrench_send_action = wrench_send_action
M.wrench_post2 = wrench_post2
M.wrench_find_qq_contact = wrench_find_qq_contact
M.wrench_share_to_qq = wrench_share_to_qq
M.weixin_find_friend = weixin_find_friend
M.get_coffee = get_coffee

local function be_quiet()
   social_need_confirm = false
end

local function be_verbose()
   social_need_confirm = true
end

local function isWeixinLuckyMoneyReceiver(window)
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

local function clickForWeixinMoney()

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

local should_use_internal_pop = true
if WrenchExt.should_use_internal_pop and WrenchExt.should_use_internal_pop() ~= 1 then
   should_use_internal_pop = false
end

M.wrench_click_notification = function(key, pkg, title, text)
   clickNotification{key}
   if pkg == "com.tencent.mobileqq" and title:lower() == "qq" then
      M.shift_click_notification(key, pkg, title, text)
   end
end

M.notification_arrived = function(key, pkg, title, text)
   ignored_pkgs = {"com.github.shadowsocks", "com.android.systemui"}
   for p = 1, #ignored_pkgs do
      if pkg == ignored_pkgs[p] then
         return
      end
   end
   -- log("got %s(%s): %s(%s)", key, pkg, title, text)
   
   if pkg == "com.tencent.mm" and text:match('%[微信红包%]') then
      clickNotification{key}
      if false and WrenchExt.should_not_pick_money(key, pkg, title, text) == 1 then
         sayThankYouForLuckyMoney(true)
         adb_event"sleep .5 adb-key home"
      else
         clickForWeixinMoney()
      end
   elseif pkg == "com.tencent.mobileqq" and text:match("%[QQ红包%]") then
      clickNotification{key}
      if title:lower() == "qq" then
         adb_event"adb-tap 633 481 sleep .2"
         -- M.shift_click_notification(key, pkg, title, text)
      end

      if WrenchExt.should_not_pick_money(key, pkg, title, text) == 1 then
         sayThankYouForLuckyMoney(true)
         adb_event"sleep .5 adb-key home"
      else
         M.clickForQqMoney(title, text)
      end
   end

   WrenchExt.notification_arrived(key, pkg, title, text)

   if not should_use_internal_pop then
      system{"bhj-notify-from-wrench", "-h", title, "-c", text, "--pkg", pkg}
   end
end

my_show_notifications = function()
   show_notifications()
end

M.be_verbose = be_verbose
M.be_quiet = be_quiet
M.my_show_notifications = my_show_notifications
M.yes_or_no_p = yes_or_no_p
M.start_or_stop_recording = start_or_stop_recording
M.adb_top_window = adb_top_window

local function do_it()
   if arg and type(arg) == 'table' and string.find(arg[0], "wrench.lua") then
      -- wrench_post(join(' ', arg))
      local file = io.open("setclip.apk.md5")
      if file then
         wrench_config()
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
      wrench_picture(arg[1]) -- , arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9])
      os.exit(0)
      print(5)
      debug_set_x = arg[#arg]
      arg[#arg] = nil
      adb_shell(arg)
   else
      return M
   end
end

weixin_emojis_remap = {
   ["[可爱]"] = '[Joyful]', ["[大兵]"] = "[Commando]", ["[折磨]"] = "[Tormented]", ["[示爱]"] = "[Lips]", ["[挥手]"] = "[Surrender]",
   ["[街舞]"] = "[Meditate]", ['[笑哭]'] = '😂', ['[斜眼笑]'] = '[奸笑]', ['[doge]'] = '[🙃]',
}

weibo_emojis_remap = {
   ["[加油]"] = '💪', ['[勾引]'] = '[来]', ['[OK]'] = '[ok]', ['[强]'] = '[good]', ['[爱你]'] = '[haha]',
   ['[飞吻]'] = '[爱你]', ['[抱拳]'] = '[作揖]', ['[心碎]'] = '[伤心]', ['[爱心]'] = '[心]', ['[发呆]'] = '[傻眼]',
   ['[玫瑰]'] = '[鲜花]', ['[拥抱]'] = '[抱抱]', ['[呲牙]'] = '[嘻嘻]', ['[憨笑]'] = '[哈哈]', ['[笑哭]'] = '[笑cry]',
   ['[调皮]'] = '[挤眼]', ['[流泪]'] = '[泪]', ['[快哭了]'] = '[悲伤]', ['[抠鼻]'] = '[挖鼻]', ['[发怒]'] = '[怒]',
   ['[咒骂]'] = '[怒骂]', ['[流汗]'] = '[汗]', ['[惊恐]'] = '[吃惊]', ['[睡觉]'] = '[睡]', ['[糗大了]'] = '[打脸]',
   ['[难过]'] = '[失望]', ['[再见]'] = '[拜拜]', ['[胜利]'] = '[耶]', ['[无奈]'] = '[摊手]',
}

dofile_res, qq_emojis_remap = pcall(dofile, "qq-emojis-remap.lua")
if not dofile_res then
   qq_emojis_remap = {}
end

unicode_remap = {
   ["[微笑]"] = [[🙂]], ["[撇嘴]"] = [[😒]], ["[色]"] = [[😍]], ["[发呆]"] = [[😳]], ["[得意]"] = [[😎]],
   ["[流泪]"] = [[😭]], ["[害羞]"] = [[☺]], ["[闭嘴]"] = [[🤐]], ["[睡]"] = [[😴]], ["[大哭]"] = [[😭]],
   ["[尴尬]"] = [[😓]], ["[发怒]"] = [[😠]], ["[调皮]"] = [[😜]], ["[呲牙]"] = [[😬]], ["[惊讶]"] = [[😮]],
   ["[难过]"] = [[☹]], ["[酷]"] = [[🕶]], ["[冷汗]"] = [[😳]], ["[抓狂]"] = [[😆]], ["[吐]"] = [[😖]],
   ["[偷笑]"] = [[😇]], ["[可爱]"] = [[😊]], ["[白眼]"] = [[🙄]], ["[傲慢]"] = [[😤]], ["[饥饿]"] = [[😋]],
   ["[困]"] = [[😪]], ["[惊恐]"] = [[😱]], ["[流汗]"] = [[😰]], ["[憨笑]"] = [[😄]], ["[大兵]"] = [[👮]],
   ["[奋斗]"] = [[💪]], ["[咒骂]"] = [[😾]], ["[疑问]"] = [[🤖]], ["[嘘]"] = [[🤐]], ["[晕]"] = [[🙃]],
   ["[折磨]"] = [[😝]], ["[衰]"] = [[💀]], ["[骷髅]"] = [[☠]], ["[敲打]"] = [[🔨]], ["[再见]"] = [[🤗]],
   ["[擦汗]"] = [[😓]], ["[抠鼻]"] = [[😐]], ["[鼓掌]"] = [[👏]], ["[糗大了]"] = [[👺]], ["[坏笑]"] = [[😏]],
   ["[左哼哼]"] = [[⚒]], ["[右哼哼]"] = [[🛠]], ["[哈欠]"] = [[😫]], ["[鄙视]"] = [[😑]], ["[委屈]"] = [[🙄]],
   ['[笑哭]'] = '😂', ['[流泪]'] = '😭', ['[快哭了]'] = '😞', ['[爱心]'] = '♥', ['[心碎]'] = '💔',
   ['[礼物]'] = '🎁', ['[微笑]'] = '🙂', ['[撇嘴]'] = '😒', ['[发呆]'] = '', ['[得意]'] = '😎',
   ['[害羞]'] = '☺', ['[色]'] = '😍', ['[调皮]'] = '😜', ['[闭嘴]'] = '🙊', ['[污]'] = '🙈',
   ['[睡]'] = '😴', ['[大哭]'] = '😭', ['[尴尬]'] = '😰', ['[发怒]'] = '😠', ['[呲牙]'] = '😁',
   ['[惊讶]'] = '😮', ['[难过]'] = '😞', ['[酷]'] = '😎', ['[冷汗]'] = '😳', ['[抓狂]'] = '😆',
   ['[吐]'] = '😖',
}

return do_it()

-- Local variables:
-- coding: utf-8
-- End:
