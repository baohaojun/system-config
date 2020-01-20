-- 在聊天的时候，@某人
-- 主要是为钉钉而开发，在钉钉里要 @某个人的话，非常麻烦

M.wrench_cue = function(who)
   current_win = adb_top_window()
   current_pkg = current_win:gsub("/.*", "")

   wrench_post("@", "No-Post")

   search_button = current_pkg .. ".cue-search"
   cue_all_scene = current_pkg .. ".cue-all"

   wait_for_scene(search_button)

   wait_ime = function()
      log("waiting for ime: %s", current_pkg)
      return wait_input_target_n_ok(1, current_pkg)
   end

   find_scene(cue_all_scene)
   jump_from_to(search_button, wait_ime)

   local cue_all = true
   if who:lower() ~= "all" and who ~= "所有人" then
      cue_all = false
   end


   is_back_to_chat = function()
      return adb_top_window() == current_win
   end

   click_1 = function()
      if cue_all then
         log("cue_all is %s", cue_all)
         click_scene(cue_all_scene, {skip_refind = true})
      else
         log("cue_all is %s", cue_all)
         jump_out_of("dd/cue-multi")
         wrench_post(who, "No-Post")
         click_scene(cue_all_scene, {skip_refind = true})
         jump_out_of("dd/cue-queding")
      end
   end

   jump_from_to(true, is_back_to_chat, {action = click_1})
end
