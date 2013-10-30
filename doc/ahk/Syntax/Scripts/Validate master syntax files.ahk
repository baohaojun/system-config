;File1 has full commands
;File2 has command names

File1 = ..\Commands.txt
File2 = ..\CommandNames.txt

;___________________________________________


MisMatch = 0
Count = 0

Loop, Read, %File2%
{
	Count++
	2Line = %A_LoopReadLine%
	FileReadLine, 1Line, %File1%, %A_Index%
	
	LineNum = %A_Index%
	
	Loop, Parse, 2Line, %A_Space%
		IfNotInString, 1Line, %A_LoopField%
		{
			MsgBox,, MisMatch Found, Line Number = %LineNum%`n%File1%`t%1Line%`n%File2%`t%2Line%
			MisMatch ++
		}
}

MsgBox, Total Mismatches: %MisMatch% of %Count%