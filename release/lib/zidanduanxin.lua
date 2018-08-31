M.open_zidanduanxin = function()
   local top_window, i
   for i = 1, 5 do
      if not top_activity_match("com.bullet.messenger/") then
         adb_am("am start -n com.bullet.messenger/com.smartisan.flashim.main.activity.MainActivity")
      end
      sleep(.3)

      local found_grey = find_scene("zidanduanxin-duihua-grey")
      local found_duihua = find_scene("zidanduanxin-duihua")
      if not found_grey and not found_duihua then
         adb_event("key back")
         sleep(.2)
      elseif found_grey then
         click_scene("zidanduanxin-duihua-grey")
         sleep(.2)
         if find_scene("zidanduanxin-duihua") then
            return
         end
      else
         if not find_scene("zidanduanxin-search-button") then
            adb_event("key back")
            sleep(.3)
            if find_scene("zidanduanxin-search-button") then
               return
            end
         else
            return
         end
      end
   end
   error("Can't open zidanduanxin")
end

M.open_zidanduanxin_search = function()
   open_zidanduanxin()

   click_scene("zidanduanxin-search-button", nil, {x = 500, y = 20})

   if not wait_input_target_n_ok(3, "com.bullet.messenger/com.smartisan.flashim.main.activity.MainActivity") then
      error("Can't open zidanduanxin search")
   end
end

M.get_who_and_confirm = function (who)
   return who:gsub("?$", ""), who:match("?$")
end

M.find_zidanduanxin_friend = function(who)
   who, confirm = get_who_and_confirm(who)
   putclip_nowait(who)
   open_zidanduanxin_search()
   adb_event("adb-key scroll_lock sleep .2")
   if confirm then
      prompt_user("请确认你要查找的是哪个用户")
   else
      click_scene("zidanduanxin-search-qunliao", nil, {x = 500, y = 200 })
      sleep(.5)
      if find_scene("zd-fasongxiaoxi") then
         click_scene("zd-fasongxiaoxi")
      end
   end
end
