#!/usr/bin/lua

-- module
local M = _ENV

local W = {}

M.ext_args = {}
M.ExtMods = {}
M.is_debugging = false
M.W = W

-- functions
M.WrenchExt = {}
M.window_post_button_map = {}
M.mail_group_map = {}
M.phone_info_map = {}
M.android_serial = ""
M.configDir = "."
M.last_uploaded_pics = {}
M.social_need_confirm = false
M.right_button_x = 984
M.m_is_recording = nil
M.current_recording_file = nil

M.debugging = nil

-- variables
M.where_is_dial_key = nil
M.rows_mail_att_finder = nil
M.UNAME_CMD = "uname || busybox uname || { echo -n Lin && echo -n ux; }"

M.using_adb_root = nil
M.adb_unquoter = nil
M.is_windows = false
M.debug_set_x = ""
M.default_width, M.default_height = 1080, 1920
M.ref_width, M.ref_height = 1080, 1920
M.real_width, M.real_height = 1080, 1920
M.app_width, M.app_height = 1080, 1920
M.ime_app_width, M.ime_app_height = 1080, 1920
M.mCurrentRotation = 0
M.app_table = nil

M.adb_top_window = function() -- 确保小扳手第一次启动不会出错
end

M.update_screen_ratios = function()
   M.default_width, M.default_height = M.ref_width, M.ref_height
   if M.mCurrentRotation % 2 == 1 then
      M.default_width, M.default_height = M.ref_height, M.ref_width
   end

   M.real_width_ratio, M.real_height_ratio = M.real_width / M.default_width, M.real_height / M.default_height
   M.app_width_ratio, M.app_height_ratio = M.app_width / M.default_width, M.app_height / M.default_height
end

M.update_screen_ratios()

M.using_oppo_os = false
M.brand = "smartisan"
M.model = "Wrench"
M.sdk_version = 19
M.emojis = nil
M.img_to_emoji_map = nil
M.emoji_to_img_map = nil
M.the_true_adb = "./the-true-adb"

W.sms = "com.android.mms/com.android.mms.ui.ComposeMessageActivity"
W.weibo_home_activity = "com.sina.weibo/com.sina.weibo.MainTabActivity"
W.weibo_search_activity = "com.sina.weibo/com.sina.weibo.page.SearchResultActivity"
W.smartisan_mail_compose = "com.android.email/com.android.mail.compose.ComposeActivity"
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

M.shell_quote = function (str)
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


M.emoji_rewrite = function(text, which_emojis)
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

M.cmd_and_args_join = function (cmd_and_args)
   if type (cmd_and_args) == 'string' then
      return cmd_and_args
   elseif type (cmd_and_args) == 'table' then
      command_str = ''
      for i = 1, #cmd_and_args do
         if i == 1 and is_windows then
            command_str = command_str .. cmd_and_args[i] .. ' '
         else
            command_str = command_str .. shell_quote(cmd_and_args[i]) .. ' '
         end
      end
      return command_str
   end
end

M.system = function (cmds)
   cmds = cmd_and_args_join(cmds)
   return os.execute(cmds)
end

debugging = function(fmt, ...)
   if M.is_debugging then
      log(fmt, ...)
   end
end

M.debugging = debugging

M.debug = function(fmt, ...)
   print(string.format(fmt, ...))
end

M.split = function(pat, str, allow_null)
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

M.replace_img_with_emoji = function (text, html)
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

M.join = function(mid, args)
   text = ''
   for i = 1, #args do
      if i ~= 1 then
         text = text .. mid
      end
      text = text .. args[i]
   end
   return text
end

M.adb_do = function(func, cmds)
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

M.adb_shell = function (cmds)
   if qt_adb_pipe then
      return adb_pipe(cmds)
   end
   return adb_do(os.execute, cmds)
end

M.adb_pipe = function(cmds)
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

M.adb_start_activity = function(a)
   adb_am("am start -n " .. a)
end

M.adb_am = function(cmd)
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

M.sleep = function(time)
   adb_event(("sleep %s"):format(time))
end

M.prompt_user = function(fmt, ...)
   if select_args then
      return select_args{string.format(fmt, ...)}
   end
end

M.quoteChatMessage = function(...)
   local t1 = {...}
   local ki
   local map = {}
   local text = ''
   for ki = 1, #t1, 2 do
      map[t1[ki]] = t1[ki + 1]
      if t1[ki] == 'text' then
         text = t1[ki + 1]
      end
   end
   text = text:gsub('^%[[0-9]+条%]', "")
   text = ([[
「 %s 」
- - - - - - - - - - - - - - -

]]):format(text)
   wrench_insert_text(text)
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

function table.slice(tbl, first, last, step)
  local sliced = {}

  for i = first or 1, last or #tbl, step or 1 do
    sliced[#sliced+1] = tbl[i]
  end

  return sliced
end

M.yes_or_no_p = function(txt, ...)
   if select_args then
      return select_args{string.format(txt, ...)} ~= ""
   end
   return false
end

M.check_scroll_lock = function()
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
               "不使用小扳手的时候，你想把你默认的输入法自动改成当前正在使用的 %s 吗？（你原来默认的输入法是 %s）"
                        ):format(
               input_method_id,
               old_input_method
         )) then

            phone_info_map['user_input_method'] = input_method_id
            save_phone_info()
         end
      end
      reset_input_method()

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

M.sdcard_putclip_path = "/sdcard/putclip.txt"

M.swipe_down = function()
   adb_event"adb-no-virt-key-wrench-swipe 549 483 552 1778 adb-no-virt-key-wrench-swipe 549 483 552 1778 sleep 1"
end

M.search_mail = function(what)
   putclip_nowait(what)
   for i = 1, 5 do
      M.start_app("com.android.email/com.android.email.activity.Welcome", {restart = 1})
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

M.string_strip = function(s)
   s = s:gsub("^%s+", "")
   s = s:gsub("%s+$", "")
   return s
end

M.search_sms = function(what)
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

M.wrench_sms = function (window)
   adb_event"adb-tap 192 1227 sleep .5 adb-key scroll_lock"
   if yes_or_no_p("确认发送短信？") then
      adb_event"adb-tap 857 1008"
   end
end

M.wrench_google_plus = function (window)
   adb_event{467, 650, 'key', 'scroll_lock', 932, 1818}
end

M.wrench_smartisan_notes = function (window)
   adb_event{'key', 'scroll_lock', 940, 140, 933, 117, 323, 1272, 919, 123}
end

M.wrench_mail = function (window)
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

M.wrench_paste = function ()
   adb_event{'key', 'scroll_lock'}
end

M.last = function(func)
   local x, y
   y = func()
   while y do
      x = y
      y = func()
   end
   return x
end


M.check_phone = function()
   if is_exiting and is_exiting() then
      error("exiting")
   end
end

M.adb_wait_file_gone = function(file)
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

M.adb_start_service = function (service_cmd)
   adb_am("am startservice --user 0 -n " .. service_cmd)
end

M.adb_start_service_and_wait_file_gone = function(service_cmd, file)
   adb_start_service(service_cmd)
   adb_wait_file_gone(file)
end

M.adb_start_service_and_wait_file = function(service_cmd, file)
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

M.adb_push = function(lpath, rpath)
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

M.adb_pull = function(rpath, lpath)
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

M.adb_install = function(apk)
   if qt_adb_install then
      return qt_adb_install{apk}
   else
      return io.popen(the_true_adb .. " install -r " .. apk):read("*a")
   end
end

M.push_text = function(text)
   if text then
      text = M.space_cjk_en(text)
   end
   if not text and os.getenv("PUTCLIP_ANDROID_FILE") then
      local file = io.open(os.getenv("PUTCLIP_ANDROID_FILE"))
      text = file:read("*a")
      file:close()
      local window = adb_top_window()
      if window:match("com.tencent.mobileqq") then
         text = emoji_for_qq(text)
      end
   end

   -- text = text:gsub("\n", "\r\n")
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

M.putclip = function(text)
   push_text(text)
   adb_start_service_and_wait_file_gone('com.bhj.setclip/.PutClipService', M.sdcard_putclip_path)
end

M.read_phone_file = function(file)
   return adb_pipe(("cat %s"):format(file))
end


M.check_file_push_and_renamed = function(file, md5, rename_to)
   if not rename_to then
      rename_to = file:gsub(".*/", "")
   end

   local md5_on_phone = adb_pipe(("if test -e /data/data/com.android.shell/%s; then cat /sdcard/%s; fi"):format(rename_to, md5))
   md5_on_phone = md5_on_phone:gsub("\n", "")
   local md5file = io.open(md5)
   if not md5file then
      prompt_user("%s 文件打开失败，小扳手可能无法正常运行，请联系作者报 Bug（参考 http://baohaojun.github.io/blog/2014/12/01/0-T1Wrench-2.0-Usage-Guide.html#bugs-howto ）", md5)
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

M.check_file_pushed = function(file, md5)
   return check_file_push_and_renamed(file, md5, nil)
end

M.check_apk_installed = function(apk, md5, reason)
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

M.space_cjk_en = function(text)
   if WrenchExt.getConfig("should-space-cjk-en") ~= "true" then
      return text
   end

   local textFile = io.open(M.dataDirFile("space-cjk-en.txt"), "w")
   textFile:write(text)
   textFile:close()

   local output = io.popen("space-cjk-en " .. M.dataDirFile("space-cjk-en.txt"))
   text = output:read("*a")
   output:close()
   return text;
end

M.qx = function(command)
   local p = io.popen(cmd_and_args_join(command))
   local v = p:read("*a")
   if v then
      v = v:gsub("\r", "")
      if v:sub(v:len()) == "\n" then
         v = v:sub(1, v:len() - 1)
      end
   end
   p:close()
   return v
end

M.dataDirFile = function(file)
   return M.dataDir .. file
end

M.configDirFile = function(file)
   return M.configDir .. file
end

M.get_xy_from_dump = function(dump, prefix)
   local xy_match = dump:match(prefix .. '=(%d+x%d+)')
   local height = tonumber(xy_match:match('x(%d+)'))
   local width = tonumber(xy_match:match('(%d+)x'))
   return width, height
end

M.update_screen_size = function()
   M.m_window_dump = adb_pipe("dumpsys window policy; dumpsys window windows") or ""
   M.m_focused_app = M.m_window_dump:match("mFocusedApp=Token.-(%S+)%s+%S+}") or ""
   M.m_focused_window = M.m_window_dump:match("mFocusedWindow=.-(%S+)}") or ""

   M.m_window_displays = adb_pipe"dumpsys window displays" or ""

   if M.m_window_displays:match("Display: mDisplayId=[1-9]") then
      local lines = split("\n", M.m_window_displays, true)
      local i, begin_0 = nil, 1
      for i = 1, #lines do
         if lines[i]:match("Display: mDisplayId=0") then
            begin_0 = i
            break
         end
      end
      M.m_window_displays = M.join("\n", { table.unpack(lines, begin_0) })
   end
   local screen_size_re = "cur=%d+x%d+"
   local curScreenSize = M.m_window_displays:match(screen_size_re) or
      M.m_window_dump:match(screen_size_re)
   log("curScreenSize is %s", curScreenSize)
   if M.last_screen_size ~= curScreenSize then
      M.last_screen_size = curScreenSize

      app_width, app_height = get_xy_from_dump(M.m_window_displays, "app")
      M.mCurrentRotation = M.m_window_dump:match("mCurrentRotation=.-(%d+)")

      M.real_width, M.real_height = get_xy_from_dump(M.m_window_displays, "cur")
      log("M.real_height is %d, app_width is %d, app_height is %d", M.real_height, app_width, app_height)
      ime_app_width, ime_app_height = get_xy_from_dump(M.m_window_displays, "app")

      update_screen_ratios()
   end


   if app_width ~= default_width then
      right_button_x = default_width - 80 * default_width / app_width
   end
end

M.fill_file_path = function(...)
   local dirs = {...}
   return join(package.config:sub(1, 1), dirs)
end

M.wrench_config = function(passedConfigDirPath, passedAppDirPath)
   if passedConfigDirPath then
      configDir = passedConfigDirPath
   end

   M.appDir = passedAppDirPath
   M.resDir = fill_file_path(M.appDir, "res")
   M.configDir = configDir .. package.config:sub(1, 1)
   M.dataDir = os.getenv("WRENCH_DATA_DIR") .. package.config:sub(1, 1)

   wrench_run("lib/.init.lua")

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
      log("Error: Phone uname is not Linux")
      return
   end

   if not file_exists(M.dataDirFile("apps.info")) then
      M.update_apps()
   end

   check_apk_installed("Setclip.apk", "Setclip.apk.md5", "需要安装小扳手辅助 App")
   check_file_pushed("am.jar", "am.jar.md5")
   check_file_pushed("busybox", "busybox.md5")

   sdk_version = adb_pipe("getprop ro.build.version.sdk")
   brand = adb_pipe("getprop ro.product.brand"):gsub("\n.*", "")
   model = adb_pipe("getprop ro.product.model"):gsub("\n.*", "")
   arm_arch = adb_pipe("/data/data/com.android.shell/busybox uname -m 2>/dev/null || uname -m")
   androidvncserver = ("androidvncserver-%s.sdk%s"):format(arm_arch, sdk_version)

   codename = adb_pipe("getprop ro.product.codename"):gsub(" ", ".")
   codenamed_vnc = ("%s-%s"):format(androidvncserver, codename)
   if file_exists(codenamed_vnc) then
      androidvncserver = codenamed_vnc
   end

   log("androidvncserver is %s", androidvncserver)

   screencap = ("screencap-%s.sdk%s"):format(arm_arch, sdk_version)
   if file_exists(screencap) then
      check_file_push_and_renamed(screencap, screencap .. ".md5", "screencap")
   end

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

   android_serial = adb_pipe("getprop ro.serialno"):gsub("\n", "")
   reset_input_method()
   system("bash ./check-notification.sh " .. android_serial)
   adb_quick_input{"input keyevent UNKNOWN"}
   return ("brand is %s, adb serial is %s"):format(brand, android_serial)
end

M.get_a_note = function(text)
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

M.adb_get_last_pic = function(which, remove)
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
   M.app_table = app_table
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

M.launch_apps = function()
   if not file_exists(M.dataDirFile("apps.info")) then
      M.update_apps()
   end
   select_apps()
end

M.on_app_selected = function(app)
   local app_table = M.get_app_table()
   if app ~= "" then
      log("starting: %s", app_table[app])
      adb_start_activity(app_table[app])
   end
end

M.dofile_res = nil
dofile_res, WrenchExt = pcall(dofile, "wrench-ext.lua")
if not dofile_res then
   WrenchExt = {}
end

M.shouldNotPostActivitys = {
   "com.tencent.mobileqq/com.tencent.mobileqq.activity.aio.photo.AIOGalleryActivity",
   "com.tencent.mm/com.tencent.mm.ui.chatting.gallery.ImageGalleryUI",
   "StatusBar",
}

M.postAfterBackKey = function (window)
   for _, w in ipairs(shouldNotPostActivitys) do
      if window == w then
         adb_event"key back sleep .2"
         wrench_post()
         return true
      end
   end
   return false
end

M.start_or_stop_recording = function()
   if not m_is_recording then
      m_is_recording = M.select_args{"请输入你想录制的文件名（例：SearchKindle）?", "", ""}
      if (m_is_recording ~= "") then
         m_is_recording = m_is_recording:gsub("[^a-z0-9_A-Z]", "_")
         m_is_recording = M.configDirFile("ext" .. package.config:sub(1, 1) .. m_is_recording .. ".lua")
         local headString = select_args{"请输入你对本次录制功能的描述（例：在 Kindle 里搜书）", "", " "}
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
      prompt_user(("你的屏幕操作已经录制在 %s 文件中，请对其进行一些编辑，然后点一下小扳手设置按钮，以便可以使用此功能"):format(m_is_recording))
      m_is_recording = nil
   end
end

M.call_ext = function(ext, ...)
   M.ext_args = {...}
   for _, dir in pairs{configDirFile("ext"), "ext"} do
      local file = configDirFile("ext") .. package.config:sub(1, 1) .. ext .. ".lua"
      if file_exists(file) then
         wrench_run(file)
         break
      end
   end

   M.ext_args = {}
end

M.set_ext_args = function(...)
   M.ext_args = {...}
end

M.start_app = function(to_start, keyed_args)
   if not to_start:match("/") then
      pkg = to_start
      local app_table = M.app_table or M.get_app_table()
      if app_table[pkg] then
         to_start = app_table[pkg]
      end
   end

   pkg = to_start:gsub("/.*", "")
   if keyed_args and keyed_args.restart then
      adb_am{'am', 'force-stop', pkg}
   end
   adb_start_activity(to_start)
   wait_top_activity_match("^" .. pkg)
end

M.M = M

M.upload_pics = function (...)
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

M.share_text_to_app = function(pkg, cls, text)
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

M.share_pics_to_app = function(pkg, cls, pics, ...)
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

M.calc_buttons = function(buttons_def)
   -- buttons_def is an assoc array, where keys are: cols, rows, first, last, wanted, start

   local first = split(" ", buttons_def['first'])
   local last = split(" ", buttons_def['last'])
   local wanted = buttons_def['wanted']
   local start = buttons_def['start']
   local first_x = first[2]
   local first_y = first[3]
   local last_x = last[2]
   local last_y = last[3]
   local cols = buttons_def.cols
   local rows = buttons_def.rows

   local return_buttons = {}
   for i = start, (start + wanted) do
      local row = math.floor((i - 1) / cols + 1)
      local col = (i - 1) % cols + 1

      local x = first_x + (last_x - first_x) / (cols - 1) * (col - 1)
      local y = first_y + (last_y - first_y) / (rows - 1) * (row - 1)

      return_buttons[i - start + 1] = ("adb-tap %d %d"):format(x, y)
      -- log("picture button: %d %s", i - start + 1, return_buttons[i - start + 1])
   end
   return return_buttons
end

M.tap_top_right = function()
   if real_height >= 2160 then
      adb_event"adb-tap 1024 99"
   else
      adb_event"adb-tap 1012 151"
   end
end

M.tap_bottom_center = function()
   adb_event"adb-tap 592 1836"
end

M.ime_is_active = function()
   local input_active, ime_height = adb_get_input_window_dump()
   return ime_height > 0
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

M.click_to_album_wx_chat_style = function(evenwrench, activity1, ...)
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

M.wrench_add_mms_receiver = function(number)
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

M.log = function(fmt, ...)
   if log_to_ui then
      log_to_ui(string.format(fmt, ...))
   end
end

if log_to_ui then
   M.log_to_ui = log_to_ui
end

M.wrench_eval = function(f, text)
   if not f then
      prompt_user("Empty f: %s", text)
   else
      return f()
   end
end

M.wrench_run = function (file, argType)
   if argType == 'string' then
      local f = loadstring(file)
      return wrench_eval(f, file)
   end
   local ext = file:gsub(".*%.", "")
   if ext ~= "twa" and ext ~= "小扳手" and ext ~= "lua" then
      return "Can not run this script, must be a .twa file"
   end
   local f = loadfile(file)
   if file:match("/.cache/system%-config/wrench%-.*.twa") then
      system{"rm", file}
   end
   return wrench_eval(f, file)
end

M.wrenchThumbUp = function()
   if not M.ExtMods.descriptions then
      M.ExtMods = wrench_run("ext/.ls-modules.lua")
      if not M.ExtMods then
         log("无法加载 ext/.ls-modules.lua")
      end
   else
      log("ExtMods already on")
   end

   if not M.ExtMods.descriptions then
      prompt_user("扩展程序加载失败，无法执行此操作")
      return
   end
   local x = select_args(M.ExtMods.descriptions)
   if x == "" then
      return
   end
   if not M.ExtMods.description_to_filename[x] then
      prompt_user("你选择的操作（" .. x .. "）没有对应的扩展脚本，是否忘了更新 ext/.modules.lua？")
      return
   end

   if not M.ExtMods.description_to_loaded_func[x] then
      M.ExtMods.description_to_loaded_func[x] = loadfile(M.ExtMods.description_to_filename[x])
   end

   wrench_eval(M.ExtMods.description_to_loaded_func[x], x)
end

M.shift_click_notification = function(key, pkg, title, text)
   log("click %s, %s, %s, %s", key, pkg, title, text)
   if pkg == "com.tencent.mm" then
      title = title:gsub("%.", " ")
      title = title:gsub("、", " ")
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
   elseif pkg == "com.alibaba.android.rimet" then
      sender = title or ""
      wrench_call(sender .. '@@dd')
   end
end

M.be_quiet = function()
   social_need_confirm = false
end

M.be_verbose = function()
   social_need_confirm = true
end

M.should_use_internal_pop = true
if WrenchExt.should_use_internal_pop and WrenchExt.should_use_internal_pop() ~= 1 then
   should_use_internal_pop = false
end

M.wrench_click_notification = function(key, pkg, title, text)
   local top_window = adb_top_window()
   if top_window and top_window:match("com.netease.onmyoji/") then
      system{os.getenv("HOME") .. "/src/github/private-config/bin/wrench-show-notifications.sh"}
      return
   end
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
      if WrenchExt.should_not_pick_money(key, pkg, title, text) == 1 then
         sayThankYouForLuckyMoney(true)
         adb_event"sleep .5 adb-key home"
      else
         clickForWeixinMoney()
      end
   elseif pkg == "com.tencent.mobileqq" and text:match("%[QQ*红包%]") then
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

M.do_it = function()
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


return do_it()

-- Local variables:
-- coding: utf-8
-- End:
