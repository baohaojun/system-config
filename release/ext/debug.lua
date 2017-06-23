-- 让小扳手进入、退出调试模式（打更多log）

if M.is_debugging then
   log("退出debug模式")
   M.is_debugging = false
else
   log("打开调试模式")
   M.is_debugging = true
end
