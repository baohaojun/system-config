local M = {}
M.description_to_filename = {}
M.description_to_loaded_func = {}
M.descriptions = {}

M.descriptions[1] = "What do you want?"
M.description_to_filename[ [==[在Kindle书店里搜索你想要找的书]==] ] = 'SearchKindle.lua'
M.descriptions[2] = [==[在Kindle书店里搜索你想要找的书]==]
M.description_to_filename[ [==[在微博上关注小扳手作者包昊军（不一定成功）]==] ] = 'follow-me.lua'
M.descriptions[3] = [==[在微博上关注小扳手作者包昊军（不一定成功）]==]
M.description_to_filename[ [==[通过微博、微信朋友圈传播小扳手（不一定成功）]==] ] = 'spread-it.lua'
M.descriptions[4] = [==[通过微博、微信朋友圈传播小扳手（不一定成功）]==]
M.description_to_filename[ [==[打开手机第三方通知应用授权设置页面，再手动打开、关闭接收通知权限（某些最新安卓版本无效😂）]==] ] = 'switch-notification.lua'
M.descriptions[5] = [==[打开手机第三方通知应用授权设置页面，再手动打开、关闭接收通知权限（某些最新安卓版本无效😂）]==]
M.description_to_filename[ [==[打开微博扫码功能]==] ] = 'weibo_scan.lua'
M.descriptions[6] = [==[打开微博扫码功能]==]
return M
