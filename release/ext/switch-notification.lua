#!/usr/bin/env Wrench
-- 打开通知授权设置页，再手动（可能要多次尝试）打开、关闭小扳手接收通知权限


adb_am"am start -a android.settings.ACTION_NOTIFICATION_LISTENER_SETTINGS"

prompt_user("请确保已经打开小扳手的系统通知读取权限开关。如果不能点击系统通知的话，请重新关闭、打开一次。")
