-- 打开京东 App 扫描二维码
start_app("com.jingdong.app.mall/com.jingdong.app.mall.main.MainActivity")

wait_top_activity_n_ok(25, [[com.jingdong.app.mall/com.jingdong.app.mall.MainFrameActivity]])
adb_event"adb-tap 64 120" -- click scan

