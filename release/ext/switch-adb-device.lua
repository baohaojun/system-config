#!/usr/bin/env Wrench.sh
-- 换个 adb 设备重新连接（如果有多个设备的话）

local devices = adb_devices()
if devices and #devices > 1 then
   local device = select_args(flatten_table("请选择你要连接到哪个 adb 设备", devices))
   for i = 1, #devices do
      if devices[i] == device then
         wrench_set_proc_var("ANDROID_SERIAL", device)
         return
      end
   end
   prompt_user("你选择的 %s 设备不是合法的参数", device)
else
   log("devices is %s", devices)
end
