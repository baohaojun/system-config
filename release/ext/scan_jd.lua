-- 打开京东 App，扫描登录 PC 网页
start_app("com.jingdong.app.mall/com.jingdong.app.mall.main.MainActivity")

wait_top_activity_n_ok(25, [[com.jingdong.app.mall/com.jingdong.app.mall.MainFrameActivity]])
adb_event"adb-tap 64 120" -- click scan

prompt_user("Please scan the bar code with your jingdong app, and click ok")

if not wait_top_activity_n_ok(5, [[com.jingdong.app.mall/com.jingdong.app.mall.login.ScanCodeLoginActivity]]) then
   prompt_user("当前页面不是京东App扫描PC登录确认页：" .. adb_top_window())
   return
end
adb_event"adb-tap 437 1200" -- click confirm
