; 1. Start | Programs | Administrative Tools | Active Directory Users and Computers
; 2. Select an organizational unit
; 3. Right-click and choose Properties
; 4. Go to the Group Policy tab in the dialog that opens
; 5. Press New and then Edit, to open the Group Policy editor
; 6. User Configuration\Software Settings\Software installation
; 7. Right-click and choose New | Package...
; 8. Files of type=ZAW Down-level application packages (*.zap)
; 9. File name=kbddvp-1_2_1.zap
;10. Select deployment method: Published
;11. OK

[Application]
FriendlyName=Programmer Dvorak
;<http://www.microsoft.com/technet/prodtechnol/ie/ieak/techinfo/deploy/60/en/iexpswit.mspx?mfr=true>
SetupCommand="kbddvp-1_2_1-i386.exe" -q:a
DisplayVersion=1.2.1
Publisher=Roland Kaufmann
URL=http://www.kaufmann.no/roland/dvorak/
LCID=1033
Architecture=Intel
