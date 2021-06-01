-- 在聊天的时候，@某人
-- 主要是为钉钉而开发，在钉钉里要 @某个人的话，非常麻烦

local ce = {}

M.cue_funcs = {
   ['com.alibaba.android.rimet'] = function ()
      find_scene(ce.cue_all_scene)
      local cue_all = true
      if ce.who:lower() ~= "all" and ce.who ~= "所有人" then
         cue_all = false
      end

      ce.click_1 = function()
         if cue_all then
            log("cue_all is %s", cue_all)
            click_scene(ce.cue_all_scene, {skip_refind = true})
         else
            log("cue_all is %s", cue_all)
            jump_out_of("dd/cue-multi")
            for n = 1, #ce.who_list do
               local who = ce.who_list[n]
               wrench_post(who, "No-Post")
               sleep(.5)
               click_scene("dd/cue-1st-choice", {skip_refind = true})
            end
            click_scene("dd/cue-queding", {skip_refind = true})
         end
      end
   end,

   ['com.ss.android.lark'] = function ()
      find_scene(ce.cue_all_scene)
      local cue_all = true
      if ce.who:lower() ~= "all" and ce.who ~= "所有人" then
         cue_all = false
      end

      ce.click_1 = function()
         if cue_all then
            log("cue_all is %s", cue_all)
            click_scene(ce.cue_all_scene, {skip_refind = true})
         else
            log("cue_all is %s", cue_all)
            jump_out_of("feishu/cue-multi")
            for n = 1, #ce.who_list do
               local who = ce.who_list[n]
               wrench_post(who, "No-Post")
               sleep(.5)
               if n == 1 then
                  click_scene("feishu/cue-1st-choice", {skip_refind = true})
               else
                  click_scene("feishu/cue-2nd-choice", {skip_refind = true})
               end
               for i = 1, #who do
                  adb_event("key DEL")
               end
               sleep(.5)
            end
            click_scene("feishu/cue-queding", {skip_refind = true})
         end
      end
   end,

   ['com.tencent.mm'] = function ()
      wrench_post(ce.who, "No-Post")
      sleep(.1)
      ce.click_1 = function()
         click_scene(ce.search_button, {skip_refind = true, x = 30, y = 240})
      end
   end
}

M.wrench_cue = function(who, ...)
   ce.who = who
   ce.who_list = {who, ...}
   ce.current_win = adb_top_window()
   ce.current_pkg = ce.current_win:gsub("/.*", "")

   wrench_post("@", "No-Post")

   ce.search_button = ce.current_pkg .. ".cue-search"
   ce.cue_all_scene = ce.current_pkg .. ".cue-all"

   wait_for_scene(ce.search_button)

   wait_ime = function()
      log("waiting for ime: %s", ce.current_pkg)
      return wait_input_target_n_ok(1, ce.current_pkg)
   end

   jump_from_to(ce.search_button, wait_ime)
   ce.is_back_to_chat = function()
      if (ce.current_pkg == 'com.alibaba.android.rimet') then
         return not find_scene "dd/at-sbd-title"
      elseif (ce.current_pkg == 'com.tencent.mm') then
         return not find_scene "wx/close-cue-search"
      else
         return adb_top_window() == ce.current_win
      end
   end

   M.cue_funcs[ce.current_pkg](ce.cue_all_scene, ce.who)
   jump_from_to(true, ce.is_back_to_chat, {action = ce.click_1})
end
