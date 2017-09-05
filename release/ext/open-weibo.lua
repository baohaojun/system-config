#!/usr/bin/env Wrench.sh
-- 用手机微博App打开微博网页

local sina_url
if M.ext_args and M.ext_args[1] then
   sina_url = M.ext_args[1]
else
   sina_url = select_args{"请输入想用微博App打开的微博网址", "", " "}
end

local saved_sina_url = sina_url

function open_sina(url)
   log("url is %s", url)
   adb_shell{"am", "start", "sinaweibo://detail?mblogid=" .. url}
end

function open_sina_user(arg)
   log("will open sina user: %s", arg:gsub("\n", " "))
   arg = split("\n", arg)
   type_ = arg[1]
   param_val = arg[2]
   if type_ == "d" then
      param_name = "nick"
   else
      param_name = "uid"
   end

   adb_shell{"am", "start", "-n", "com.sina.weibo/.page.ProfileInfoActivity", "--es", param_name, param_val}
end

sina_url, ok = sina_url:gsub("http://m?%.?weibo%.co?[mn]?/([dup])/([^?/&%%]+).*", "%1\n%2")
if ok == 1 then
   open_sina_user(sina_url)
   return
end

sina_url, ok = sina_url:gsub("http://[mw]*%.?weibo.co?[mn]?/[^/]*/([^?/]+).*", "%1")
if ok == 1 then
   open_sina(sina_url)
   return
end

sina_url, ok = sina_url:gsub("http://m?%.?weibo%.co?[mn]?/([^?/&%%]+)$", "u\n%1")
if ok == 1 then
   open_sina_user(sina_url)
   return
end

prompt_user("不知道怎么用微博App打开这条网址：" .. saved_sina_url)
