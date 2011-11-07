'Undo Windows Update Restriction - xp_winupd_netpolicy.vbs
'© Doug Knox - modified 10/18/2003
'Downloaded from www.dougknox.com

On Error Resume Next
Set WshShell = WScript.CreateObject("WScript.Shell")

p1 = "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\WindowsUpdate\DisableWindowsUpdateAccess"
p2 = "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoWindowsUpdate"

WshShell.RegWrite p1, 0, "REG_DWORD"
WshShell.RegWrite p2, 0, "REG_DWORD"

Set WshShell = Nothing

x = MsgBox("Finished",4096,"Windows Update Restriction")
