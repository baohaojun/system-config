-- 在Kindle书店里搜索你想要找的书
function start_kindle()
   for i = 1, 20 do
      adb_start_activity"com.amazon.kindle/com.amazon.kindle.UpgradePage"
      wait_top_activity_match("^com.amazon.kindle/")
      adb_event"key back sleep .2"

      top_window = adb_top_window()
      if top_window ~= "" and not top_window:match("^com.amazon.kindle/") then
         log("We got top window: %s at %d", top_window, i)
         break
      end
   end

   adb_start_activity"com.amazon.kindle/com.amazon.kindle.UpgradePage"
end

for i = 1, 20 do
:: restart ::
   start_kindle()
   if not wait_top_activity_n_ok(5, [[com.amazon.kindle/com.amazon.kcp.library.StandaloneLibraryActivity]]) then
      log("can't find amazon StandaloneLibraryActivity at %d", i)
   else
      adb_event"adb-tap 1024 152" -- click store
      wait_top_activity_n_ok(5, [[com.amazon.kindle/com.amazon.kcp.store.LegacyStoreActivity]])
      for j = 1, 20 do

         adb_event"sleep .5"
         adb_event"adb-tap 505 311" -- click search
         if wait_input_target_n_ok(5, "com.amazon.kindle/com.amazon.kcp.store.LegacyStoreActivity") then
            log("got to amazon store search at %d", j)
            break
         else
            log("can't get store search ime at %d", j)
         end
      end
      log("we are sending %s", M.ext_args[1])
      t1_post(M.ext_args[1])
      adb_event"sleep .1 key enter"
      break
   end
end
