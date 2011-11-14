all: kbddvp32.dll kbddvp64.dll kbddvp.exe

clean:
	-for %a in (res cab exe) do @if exist kbddvp.%a del kbddvp.%a
	-for %a in (32 64) do @for %b in (dll obj lib exp) do @if exist kbddvp%a.%b del kbddvp%a.%b
	-for %a in (obj exe) do @if exist launcher.%a del launcher.%a

CL32="$(WINDDK)\bin\x86\cl.exe"
LINK32=link -machine:ix86
CL64="$(WINDDK)\bin\win64\x86\amd64\cl.exe"
LINK64=link -machine:amd64

# http://www.levicki.net/articles/tips/2006/09/29/How_to_build_keyboard_layouts_for_Windows_x64.php
kbddvp64.obj: kbddvp.c
	$(CL64) -nologo -c -I..\inc -Zp8 -Gy -W3 -WX -Gz -Gm- -EHs-c- -GR- -GF -Zl -Oxs \
		 -DBUILD_WOW6432 -D_WIN32_WINNT=0x0501 -Fo$@ $**

kbddvp32.obj: kbddvp.c
	$(CL32) -nologo -c -I..\inc -Zp8 -Gy -W3 -WX -Gz -Gm- -EHs-c- -GR- -GF -Zl -Oxs \
		 -D_WIN32_WINNT=0x0501 -Fo$@ $**

kbddvp.res: kbddvp.rc
	    rc $**

# ignore that sections had different attributes before they were merged 
kbddvp32.dll: kbddvp32.obj kbddvp.res
	$(LINK32) -nologo -dll -base:0x5FFF0000 -subsystem:native -def:kbddvp.def -noentry \
		 -merge:.edata=.data -merge:.rdata=.data -merge:.text=.data -merge:.bss=.data -section:.data,re \
		 -ignore:4078,4070,4254 -opt:nowin98 -stack:0x40000,0x1000 -opt:ref,icf -release -out:$@ $**

# different from the 32-bit version is the base address and the machine flag
kbddvp64.dll: kbddvp64.obj kbddvp.res
	$(LINK64) -nologo -dll -base:0x5FFE0000 -subsystem:native -def:kbddvp.def -noentry \
		 -merge:.edata=.data -merge:.rdata=.data -merge:.text=.data -merge:.bss=.data -section:.data,re \
		 -ignore:4078,4070,4254 -opt:nowin98 -stack:0x40000,0x1000 -opt:ref,icf -release -out:$@ $**

launcher.obj: launcher.c
	$(CL32) -nologo -c -Fo$@ $**

# nowin98 option aligns at smaller page size
# we don't need to link with libc (or libcmt) since we don't use the CRT!
launcher.exe: launcher.obj
	$(LINK32) -nologo -subsystem:windows -opt:nowin98 -release -nodefaultlib -out:$@ \
		 $** kernel32.lib user32.lib shell32.lib

# http://support.microsoft.com/kb/310618
kbddvp.cab: kbddvp32.dll kbddvp64.dll kbddvp.inf launcher.exe
	cabarc -m LZX:21 n $@ $**

# If you want to use makecab instead of cabarc, then use these commands instead;
# 
# kbddvp-bin.ddf: kbddvp32.dll kbddvp64.dll kbddvp.inf launcher.exe
# 	copy /y NUL $@
# 	for %a in ($**) do echo %a >> $@
# 
# # <http://msdn.microsoft.com/en-us/library/bb267310.aspx#microsoftmakecabusersguide>
# kbddvp.cab: kbddvp-bin.ddf
# 	makecab /D CabinetNameTemplate=$@ /D CompressionType=LZX /D CompressionMemory=21 /F $**
	
# http://www.msfn.org/board/SED-INF-DDF-file-format-t49202.html		 
kbddvp.exe: kbddvp.sed kbddvp.cab
	iexpress /N /Q /M kbddvp.sed
