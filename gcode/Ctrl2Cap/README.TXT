
Ctrl2cap Keyboard Filter

Example keyboard filter driver for NT4 and Win2K
Copyright (C) 1996-1999 Mark Russinovich
http://www.sysinternals.com

To install:

	- Run "ctrl2cap.exe /install" from install directory
	- Reboot

To uninstall:
	- Run "ctrl2cap.exe /uninstall" from install directory
	- Reboot

To build source:
	- Copy ddk\src\input\inc\ntddkbd.h to \sys directory
	- Run buildnt4.bat under NT 4 DDK to build NT 4 driver
		* symbols placed under symbols.nt4
		* driver produced as release\ctrl2cap.nt4.sys
	- Run buildnt5.bat under Win2K DDK to build Win2K driver
		* symbols placed under symbols.nt5
		* driver produced as release\ctrl2cap.nt5.sys
