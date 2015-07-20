#Set environment variables for Visual Studio Command Prompt
#http://stackoverflow.com/questions/2124753/how-i-can-use-powershell-with-the-visual-studio-command-prompt
function batCall([string] $path, [string] $arg)
{
Write-Host "Calling $path $arg"
cmd /c "$path $arg & set" |
foreach {
  if ($_ -match "=") {
    #Write-Host "ENV:\$($v[0])=$($v[1])"
    $v = $_.split("="); set-item -force -path "ENV:\$($v[0])"  -value "$($v[1])"
  }
}
}