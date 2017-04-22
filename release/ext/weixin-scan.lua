-- 打开微信扫描

local open_weixin_scan = function()
   weixin_open_homepage()
   adb_event"sleep .1 adb-tap 56 154 sleep .5 adb-tap 1002 115 sleep .5 adb-tap 717 580"
   if true then
      return
   end
   wx_login_page = "com.tencent.mm/com.tencent.mm.plugin.webview.ui.tools.WebViewUI"
   for i = 1, 60 do
      w = adb_top_window()
      if w == "com.tencent.mm/com.tencent.mm.plugin.scanner.ui.BaseScanUI" then
         sleep(1)
      elseif w == wx_login_page then
         for y = 1, 10 do
            adb_event"sleep .1 adb-tap 516 1020"
         end
         break
      end
   end
end

open_weixin_scan()
