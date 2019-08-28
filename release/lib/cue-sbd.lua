-- 在聊天的时候，@某人
-- 主要是为钉钉而开发，在钉钉里要 @某个人的话，非常麻烦

M.wrench_cue = function(who)
   current_win = adb_top_window()
   current_pkg = current_win:gsub("/.*", "")

   wrench_post("@", "no-post")

   search_button = current_pkg .. ".cue-search"
   cue_all = current_pkg .. ".cue-all"

   wait_for_scene(search_button)

   wait_ime = function()
      log("waiting for ime: %s", current_pkg)
      return wait_input_target_n_ok(1, current_pkg)
   end

   find_scene(cue_all)
   jump_from_to(search_button, wait_ime)
   wrench_post(who, "no-post")


   is_back_to_chat = function()
      return adb_top_window() == current_win
   end

   click_1 = function()
      click_scene(cue_all, {skip_refind = true})
   end

   jump_from_to(true, is_back_to_chat, {action = click_1})
end
