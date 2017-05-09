-- 给作者打钱以表支持

call_ext("weixin-scan")
wrench_picture("bhj-wechat-pay.png")
wait_top_activity_n_ok(20, 'com.tencent.mm/com.tencent.mm.plugin.remittance.ui.RemittanceUI')
for i = 1, 5 do
   adb_event"sleep .5 adb-tap 287 759 sleep .2"
   local ime_active, height, ime_connected = adb_get_input_window_dump()
   log("ime_active is %s", ime_active)
   if ime_active and height ~= 0 then
      break
   end
end

what = select_args{"请输入想对小扳手作者说的话", "谢谢你开发小扳手！", "小扳手真是太帅了！"}
putclip(what)
adb_event"key scroll_lock sleep .1 adb-tap 852 841 sleep .2 adb-tap 429 832"
prompt_user("真的要通过微信给作者打钱吗？请输入金额并确认支付。谢谢你对小扳手的支持^_*！")
