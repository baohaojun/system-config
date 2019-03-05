#!/usr/bin/env Wrench
-- 各种搜索功能（搜微信联系人、QQ 联系人、QQ 群里的用户、微博用户、邮件（按发信人搜）等等）

local get_where_to_search = function()
   return select_args_with_history(
      'misc-search',
      "请选择你想在哪个 App 里搜索（也可直接输入比如 baohaojun@@wx，如果我在你的微信好友里的话）？",
      "微信（用户名@@wx）",
      "钉钉（用户名@@dd）",
      "QQ 联系人（用户名@@qq）",
      "QQ 群里的用户（用户名@群名@@qq）",
      "子弹短信用户（用户名@@zd）",
      "微博用户（用户名@@wb）",
      "邮件（用户名@@mail）",
      "帮助"
   )
end

local get_postfix_from_where = function(where)
   local postfix=nil
   if where == "微信（用户名@@wx）" then
      postfix = "@@wx"
   elseif where == "钉钉（用户名@@dd）" then
      postfix = "@@dd"
   elseif where == "QQ 联系人（用户名@@qq）" then
      postfix = "@@qq"
   elseif where == "QQ 群里的用户（用户名@群名@@qq）" then
      local group = select_args{"请输入你想搜索哪个 QQ 群？", "", " "}
      postfix = "@" .. group .. "@@qq"
   elseif where == "微博用户（用户名@@wb）" then
      postfix = "@@wb"
   elseif where == "邮件（用户名@@mail）" then
      postfix = "@@mail"
   end
   return postfix
end

local get_search_string

get_search_string = function()
   if M.ext_args then
      what = M.ext_args[1]
      where = M.ext_args[2]
   end

   if not where then
      where = get_where_to_search()
   end

   postfix = get_postfix_from_where(where)
   search_string = nil

   if postfix then
      postfix = postfix
   elseif where == "帮助" then
      prompt_user"也可以按小扳手的电话按扭，然后直接输入比如 baohaojun@@wx"
      return nil
   elseif where:match"@@" then
      search_string = where
   else
      if yes_or_no_p("你没有指定在哪个 app 里搜索（比如用@@wx 指定微信，@@dd 指定钉钉等），重试？") then
         return get_search_string()
      else
         return nil
      end
   end

   if not search_string and not what then
      what = select_args{"请输入你在 " .. where .. " 里要搜索的内容", "", " "}
   end

   if not search_string then
      search_string = what .. postfix
   end
   return search_string
end

search_string = get_search_string()

if search_string then
   wrench_call(search_string)
end


