#!/usr/bin/env Wrench
-- 各种搜索功能（搜微信联系人、QQ联系人、QQ群里的用户、微博用户、邮件（按发信人搜）等等）

if M.ext_args then
   what = M.ext_args[1]
   where = M.ext_args[2]
end

if not where then
   where = select_args_with_history('misc-search', "请选择你想在哪个App里搜索（也可直接输入比如 baohaojun@@wx，如果我在你的微信好友里的话）？", "微信（用户名@@wx）", "QQ联系人（用户名@@qq）", "QQ群里的用户（用户名@群名@@qq）", "微博用户（用户名@@wb）", "邮件（用户名@@mail）", "帮助")
end

search_string = nil

if where == "微信（用户名@@wx）" then
   postfix = "@@wx"
elseif where == "QQ联系人（用户名@@qq）" then
   postfix = "@@qq"
elseif where == "QQ群里的用户（用户名@群名@@qq）" then
   local group = select_args{"请输入你想搜索哪个QQ群？", "", " "}
   postfix = "@" .. group .. "@@qq"
elseif where == "微博用户（用户名@@wb）" then
   postfix = "@@wb"
elseif where == "邮件（用户名@@mail）" then
   postfix = "@@mail"
elseif where == "帮助" then
   prompt_user"也可以按小扳手的电话按扭，然后直接输入比如 baohaojun@@wx"
   return
elseif where:match"@@" then
   search_string = where
else
   prompt_user("不知道如何搜索你输入的选项：" .. where)
   return
end

if not search_string and not what then
   what = select_args{"请输入你在 " .. where .. " 里要搜索的内容", "", " "}
end

if not search_string then
   search_string = what .. postfix
end

wrench_call(search_string)


