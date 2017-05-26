-- 把拖进来的文字保存到我的 GTD 列表里面

local gtd_text
if M.ext_args and M.ext_args[1] then
   gtd_text = M.ext_args[1]
else
   gtd_text = select_args{"请输入想插入到 GTD 列表的文字", "", " "}
end

local gtd_file = io.open(dataDirFile("wrench-gtd.org"), "a+")

gtd_file:write(("\n* TODO [#C] %s"):format(gtd_text))
gtd_file:close()

log("Your gtd capture text has been saved at %s", dataDirFile("wrench-gtd.org"))
