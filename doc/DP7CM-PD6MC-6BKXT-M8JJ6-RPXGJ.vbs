ON ERROR RESUME NEXT

Dim VOL_PROD_KEY
if Wscript.arguments.count<1 then
VOL_PROD_KEY =InputBox("使用说明："&vbCr&vbCr&" 本程序将自动替换你当前 Windows 的序列号,通过微软验证完全正版。"&vbCr&vbCr&"序列号(OEM版无效,默认版本为 XP VLK)："&vbCr&vbCr&"输入序列号(默认为 XP VLK)：","Windows XP/2003 序列号更换工具","DP7CM-PD6MC-6BKXT-M8JJ6-RPXGJ")
if VOL_PROD_KEY="" then
Wscript.quit
end if
else
VOL_PROD_KEY = Wscript.arguments.Item(0)
end if

VOL_PROD_KEY = Replace(VOL_PROD_KEY,"-","") 'remove hyphens if any

for each Obj in GetObject("winmgmts:{impersonationLevel=impersonate}").InstancesOf ("win32_WindowsProductActivation")

result = Obj.SetProductKey (VOL_PROD_KEY)

if err = 0 then
Wscript.echo "您的 Windows CD-KEY 修改成功。请检查系统属性。"
end if

if err <> 0 then
Wscript.echo "修改失败！请检查输入的 CD-KEY 是否与当前 Windows 版本相匹配。"
Err.Clear
end if

Next
