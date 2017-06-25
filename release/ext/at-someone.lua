-- 在QQ、微信等聊天时想@一下某人

wrench_post("@", 'manual-post')
search = select_args_with_history("at-someone", "你想@谁？", " ", "")
top_window = adb_top_window()

local wait_for_input = false
if top_window == "com.tencent.mm/com.tencent.mm.ui.chatting.AtSomeoneUI" then
   tap_top_right()
   wait_for_input = 'weixin'
elseif top_window == "com.tencent.mobileqq/com.tencent.mobileqq.activity.TroopMemberListActivity" then
   adb_event"adb-tap 489 288"
   wait_for_input = 'qq'
end

if wait_for_input then
   if not wait_input_target_n_ok(5, top_window) then
      prompt_user("无法等到搜索@某人的搜索框输入")
      return
   end
else
   prompt_user("请确认已经在你可以输入 %s 的输入窗口", search)
end

wrench_post(search, 'manual-post')

if wait_for_input and yes_or_no_p("请确认你用 %s 搜出来的第一个联系人，是否就是你想@的那个\n\n如是，确认后自动为你点击；如不是，请取消并手动点击想@的联系人", search) then
   if wait_for_input == 'qq' then
      adb_event("adb-tap 502 272")
   elseif wait_for_input == 'weixin' then
      adb_event("adb-tap 382 298")
   end
end

