M.scene_map_file = nil
M.save_scenes = function()
   local mapfile = io.open(M.scene_map_file, "w")
   mapfile:write("local map = {}\n")
   for k, v in spairs(scenes_map) do
      if k ~= "" then
         mapfile:write(("map['%s'] = '%s'\n"):format(k, v))
      end
   end
   mapfile:write("return map\n")
   mapfile:close()
end

M.load_scene_map = function()
   if not M.scene_map_file then
      local tmp_scene_map_file = M.configDirFile(("scenes-map-%dx%d@%s.lua"):format(M.real_width, M.real_height, android_serial))
      log("tmp scene_map_file is %s", tmp_scene_map_file)
      dofile_res, M.scenes_map = pcall(dofile, tmp_scene_map_file)
      if not dofile_res then
         M.scenes_map = {}
      end
      M.scene_map_file = tmp_scene_map_file
   end
end

M.forget_scene = function(scene)
   log("Forget scene: %s", scene)
   M.scenes_map[scene] = nil
end

M.refind_scene = function(scene, retry)
   if not retry or retry < 1 then
      retry = 1
   else
      retry = retry + 1
   end

   for i = 1, retry do
      if find_scene(scene) then
         return true
      end
      log("Scene %s not found", scene)
      sleep(.1)
   end

   local ret = false

   if retry ~= 1 then
      forget_scene(scene)
      ret = find_scene(scene)
   end

   if not ret then
      if not  M.g_find_scene_debug then
         M.g_find_scene_debug = true
         M.refind_scene(scene, retry)
      else
         error("Can't jump from " .. tostring(from_scene) .. " to " .. tostring(to_scene))
      end
   end
   return ret
end

M.click_post_button = function(button_name)
   load_scene_map()
   if not M.scene_exists(button_name) then
      system(("find-scene.sh new-scene %s"):format(button_name))
      find_scene(button_name)
   end

   click_scene(button_name, {skip_refind = 1})
end

M.show_scene_for_dbg = function(scene)
   system(
      ("cd '%s' && convert %s.png.failed.png %s.png -append x.png && display x.png&"):format(
         M.resDir, scene, scene
   ))
end

M.update_scene = function(scene)
   system(("cd '%s'; mv %s.png.failed.png %s.png"):format(
         M.resDir, scene, scene
   ))
end

M.debug_scene_actions = {
   "XXX", -- will be replaced as the prompt
   "确认此现象是正常的（啥也不做）", -- 2
   "用 failed XXX.png 进行替换", -- 3
   "我手动操作一下手机，然后你在原来的位置重新比较一遍", -- 4
   "我手动操作一下手机，然后你忘记原来的位置，重新查找一遍", -- 5
   "关闭场景查找功能的调试开关", -- 6
   "☠☠☠☠☠☠☠☠☠", -- 7
   "将 failed XXX.png 添加为 alias", -- 8
}

M.get_debug_action = function(desc)
   M.debug_scene_actions[1] = desc
   local ans = select_args(M.debug_scene_actions)
   log("your choice: %s", ans)
   return ans
end

M.debug_find_scene = function(desc, scene)
   local should_ret = nil
   local ret_val = nil
   if M.g_find_scene_debug then
      show_scene_for_dbg(scene)
      local action = M.get_debug_action(desc)
      if action == M.debug_scene_actions[3] then
         update_scene(scene);
      elseif action == M.debug_scene_actions[4] then
         ret_val = find_scene(scene)
         should_ret = 1
      elseif action == M.debug_scene_actions[5] then
         forget_scene(scene)
         ret_val = find_scene(scene)
         should_ret = 1
      elseif action == M.debug_scene_actions[6] then
         M.g_find_scene_debug = nil
      elseif action == M.debug_scene_actions[7] then
         error("You have choosed to start over")
      elseif action == M.debug_scene_actions[8] then
         system(("find-scene.sh add-alias %s"):format(scene))
      end
   end
   return ret_val, should_ret
end

M.scene_exists = function(scene)
   load_scene_map()
   return M.scenes_map[scene]
end

M.scene_x = function(scene)
   load_scene_map()
   local saved_scene_xy = M.scenes_map[scene]
   while not saved_scene_xy do
      log("Can't get xy for %s, err!", scene)
      M.g_find_scene_debug = true
      find_scene(scene)
      saved_scene_xy = M.scenes_map[scene]
   end
   return saved_scene_xy:gsub(" .*", "")
end

M.scene_y = function(scene)
   load_scene_map()
   local saved_scene_xy = M.scenes_map[scene]
   return saved_scene_xy:gsub(".* ", "")
end

M.find_scene = function(scene, times)
   if type(scene) == 'function' then
      return scene()
   elseif scene == true then
      return true
   elseif scene:match("^window:") then
      scene = scene:gsub("window:", "")
      return adb_top_window() == scene
   end
   load_scene_map()
   times = times or 1
   if M.g_find_scene_debug then
      system(("rm %s/%s.png.failed.png -f; killall display"):format(M.resDir, scene))
   end
   saved_scene_xy = M.scenes_map[scene]
   if not M.scenes_map[scene] then
      for i = 1, times do
         sleep(1)
         scene_xy = qx("find-scene.sh find-scene -s " .. scene .. " --scene-dir " .. M.resDir)
         if scene_xy ~= "" then
            break
         elseif i == times then
            debug_desc = ("无法找到新的场景：%s，要不要将 failed %s.png 替换为 %s.png（如果选否，会重新查找，请确认）？"):format(scene, scene, scene)
            ret_val, should_ret = debug_find_scene(debug_desc, scene)
            if should_ret then
               return ret_val
            end
            return nil
         end
      end
      M.scenes_map[scene] = scene_xy
      save_scenes()
   end
   scene_xy = M.scenes_map[scene]

   s_x = scene_xy:gsub(" .*", "")
   s_y = scene_xy:gsub(".* ", "")

   if system(("find-scene.sh is-scene -x %s -y %s -s %s"):format(s_x, s_y, scene)) then
      log("found scene: %s at %s %s", scene, s_x, s_y)
      debug_desc = ("已找到场景：%s，要不要将 failed %s.png 替换为 %s.png？"):format(scene, scene, scene)
      ret_val, should_ret = debug_find_scene(debug_desc, scene)
      if should_ret then
         return ret_val
      end
      return true
   else
      log("! found scene: %s at %s %s", scene, s_x, s_y)
      debug_desc = ("无法找到旧的场景：%s，要不要将 failed %s.png 替换为 %s.png（如果选否，会忘掉它，当成新场景处理，请手动切换到此场景）？"):format(scene, scene, scene)
      ret_val, should_ret = debug_find_scene(debug_desc, scene)
      if should_ret then
         return ret_val
      end
      return false
   end
end

M.scene_size_map = {}

M.get_scene_size = function(scene)
   if not M.scene_size_map[scene] then
      command = ("(set -x; identify %s/%s.png | perl -pe 's,.*?\\s(\\d+x\\d+)\\s.*,$1,') 2>~/tmp/x.txt"):format(M.resDir, scene)
      size = qx(command)
      M.scene_size_map[scene] = size
   end

   if not M.scene_size_map[scene] then
      log("Can't get size for scene: %s", scene)
      size = "20x20"
   else
      size = M.scene_size_map[scene]
   end
   log ("size for %s is %s", scene, size)
   return size
end

M.get_scene_w = function(scene)
   size = M.get_scene_size(scene)
   return size:gsub("x.*", "")
end

M.get_scene_h = function(scene)
   size = M.get_scene_size(scene)
   return size:gsub(".*x", "")
end

M.swipe_scene = function (scene, settings)
   log("Click scene: %s", scene)
   settings = settings or {}

   swipe_millisecs = settings.swipe_millisecs or 100

   x_plus = settings.x or M.get_scene_w(scene) / 2
   y_plus = settings.y or M.get_scene_h(scene) / 2

   x_delta = settings.dx
   y_delta = settings.dy

   M.load_scene_map()

   local xy = scenes_map[scene]
   xy = split(" ", xy)
   from_x = xy[1] + x_plus
   from_y = xy[2] + y_plus

   to_x = from_x + x_delta
   to_y = from_y + y_delta

   adb_quick_input{("input touchscreen swipe %d %d %d %d %d"):format(from_x, from_y, to_x, to_y, swipe_millisecs)}
end

M.click_scene = function (scene, settings)
   log("Click scene: %s", scene)
   settings = settings or {}

   x_plus = settings.x or M.get_scene_w(scene)
   y_plus = settings.y or M.get_scene_h(scene)
   click_times = settings.click_times or 1
   click_wait = settings.click_wait or .1

   if settings.skip_refind and not M.scene_exists(scene) then
      settings.skip_refind = false
   end

   if not settings.skip_refind and not refind_scene(scene, settings.retry) then
      log("Can't find scene: %s for click (retry: %s)", scene, settings.retry)
      return
   end
   local xy = scenes_map[scene]
   xy = split(" ", xy)
   for n = 1, click_times do
      log("click %s +(%d %d) @%d", scene, x_plus, y_plus, n)
      if settings.long_press then
         adb_long_press_XY(xy[1] + x_plus, xy[2] + y_plus, settings.long_press)
      else
         adb_tap_XY(xy[1] + x_plus, xy[2] + y_plus)
      end
      if n < click_times then
         sleep(click_wait)
      end
   end
end

M.jump_from_to = function(from_scene, to_scene, settings)
   settings = settings or {}
   times = settings.times or 3
   sleep_time = settings.sleep_time or .2

   action = settings.action

   for t = 1, times do
      if find_scene(to_scene) then
         log("jumped from %s to %s", from_scene, to_scene)
         return
      end

      if find_scene(from_scene) then
         settings.skip_refind = 1
         if action then
            action()
         else
            click_scene(from_scene, settings)
         end
         sleep(sleep_time)
         if t == times and find_scene(to_scene) then
            log("jumped from %s to %s", from_scene, to_scene)
            return
         end
      end
   end
   log("Can't jump from %s to %s", tostring(from_scene), tostring(to_scene))
   if not  M.g_find_scene_debug then
      M.g_find_scene_debug = true
      M.jump_from_to(from_scene, to_scene, settings)
   else
      error("Can't jump from " .. tostring(from_scene) .. " to " .. tostring(to_scene))
   end
end

M.wait_for_scene = function(scene, settings)
   settings = settings or {}
   times = settings.times or 10
   sleep_time = settings.sleep_time or .1

   for t = 1, times do
      found = false
      if type(scene) == 'string' and find_scene(scene) then
         found = true
      elseif type(scene) == 'function' and scene() then
         found = true
      end

      if found == true then
         return true
      end
      sleep(sleep_time)
   end
   error(("Can't wait for scene: %s"):format(scene))
end

M.jump_out_of = function(scene, settings)
   settings = settings or {}
   times = settings.times or 1
   sleep_time = settings.sleep_time or .2

   for t = 1, times do
      if not find_scene(scene) then
         log("jumped out of %s", scene)
         return
      end

      settings.skip_refind = 1
      click_scene(scene, settings)
      sleep(sleep_time)
      if t == times and not find_scene(scene) then
         log("jumped out of %s", scene)
         return
      end
   end
   log("Can't jump out of %s", scene)
   error(("Can't jump out of %s"):format(scene))
end
