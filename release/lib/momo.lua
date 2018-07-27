M.picture_to_momo_share = function(pics, ...)
   if pics == nil then
       pics = last_uploaded_pics
   end
   share_pics_to_app("com.immomo.momo", ".android.activity.feed.SharePublishFeedActivity", pics)
   wait_top_activity("com.immomo.momo/com.immomo.momo.android.activity.feed.PublishFeedActivity")
   adb_event("adb-tap 176 329")
   wait_input_target("com.immomo.momo/com.immomo.momo.android.activity.feed.PublishFeedActivity")
end
