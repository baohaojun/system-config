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
      sleep(.1)
   end

   if retry == 1 then
      return false
   end

   forget_scene(scene)
   return find_scene(scene)
end

M.show_scene_for_dbg = function(scene)
   system(
      ("cd '%s' && convert failed-%s.png %s.png -append x.png && display x.png&"):format(
         M.resDir, scene, scene
   ))
end

M.update_scene = function(scene)
   system(("cd '%s'; mv failed-%s.png %s.png"):format(
         M.resDir, scene, scene
   ))
end

M.debug_scene_actions = {
   "XXX", -- will be replaced as the prompt
   "确认此现象是正常的（啥也不做）", -- 2
   "用 failed-XXX.png 进行替换", -- 3
   "我手动操作一下手机，然后你在原来的位置重新比较一遍", -- 4
   "我手动操作一下手机，然后你忘记原来的位置，重新查找一遍", -- 5
   "关闭场景查找功能的调试开关", -- 6
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
      end
   end
   return ret_val, should_ret
end

M.find_scene = function(scene, times)
   load_scene_map()
   times = times or 1
   if M.g_find_scene_debug then
      system(("rm %s/failed-%s.png -f; killall display"):format(M.resDir, scene))
   end
   saved_scene_xy = M.scenes_map[scene]
   if not M.scenes_map[scene] then
      for i = 1, times do
         sleep(1)
         scene_xy = qx("find-scene.sh find-scene -s " .. scene .. " --scene-dir " .. M.resDir)
         if scene_xy ~= "" then
            break
         elseif i == times then
            debug_desc = ("无法找到新的场景：%s，要不要将 failed-%s.png 替换为 %s.png（如果选否，会重新查找，请确认）？"):format(scene, scene, scene)
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
      debug_desc = ("已找到场景：%s，要不要将 failed-%s.png 替换为 %s.png？"):format(scene, scene, scene)
      ret_val, should_ret = debug_find_scene(debug_desc, scene)
      if should_ret then
         return ret_val
      end
      return true
   else
      log("! found scene: %s at %s %s", scene, s_x, s_y)
      debug_desc = ("无法找到旧的场景：%s，要不要将 failed-%s.png 替换为 %s.png（如果选否，会忘掉它，当成新场景处理，请手动切换到此场景）？"):format(scene, scene, scene)
      ret_val, should_ret = debug_find_scene(debug_desc, scene)
      if should_ret then
         return ret_val
      end
      return false
   end
end

M.click_scene = function (scene, settings)
   log("Click scene: %s", scene)
   settings = settings or {}

   x_plus = settings.x or 0
   y_plus = settings.y or 0
   click_times = settings.click_times or 1

   if not settings.skip_refind and not refind_scene(scene, settings.retry) then
      log("Can't find scene: %s for click (retry: %s)", scene, settings.retry)
      return
   end
   local xy = scenes_map[scene]
   xy = split(" ", xy)
   for n = 1, click_times do
      log("click %s @%d", scene, n)
      if settings.long_press then
         adb_long_press_XY(xy[1] + x_plus, xy[2] + y_plus, settings.long_press)
      else
         adb_tap_XY(xy[1] + x_plus, xy[2] + y_plus)
      end
      if n < click_times then
         sleep(.1)
      end
   end
end

M.jump_from_to = function(from_scene, to_scene, settings)
   settings = settings or {}
   times = settings.times or 1
   sleep_time = settings.sleep_time or .2

   for t = 1, times do
      if find_scene(to_scene) then
         log("jumped from %s to %s", from_scene, to_scene)
         return
      end

      if find_scene(from_scene) then
         settings.skip_refind = 1
         click_scene(from_scene, settings)
         sleep(sleep_time)
         if t == times and find_scene(to_scene) then
            log("jumped from %s to %s", from_scene, to_scene)
            return
         end
      end
   end
   error("Can't jump from " .. from_scene .. " to " .. to_scene)
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
   error("Can't jump out of %s", scene)
end
