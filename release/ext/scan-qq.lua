#!/usr/bin/env Wrench
-- 打开QQ App并扫描

M.qq_open_search()

M.exit_ime()

local qq_scan_button = 'adb-tap 814 580'
if real_height == 2160 then
   qq_scan_button = 'adb-tap 839 453'
end

adb_event("sleep .1 adb-tap 1018 145 sleep .2 " .. qq_scan_button .. " sleep .3")

if not wait_top_activity_n_ok(10, "com.tencent.mobileqq/com.tencent.biz.qrcode.activity.ScannerActivity") then
   if yes_or_no_p("打开 QQ 扫描好像失败了，重试？") then
      call_ext("scan-qq")
   end
end
