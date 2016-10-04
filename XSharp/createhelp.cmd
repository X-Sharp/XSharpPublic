@echo off
SET OUT=Binaries\Help\
del %OUT%xsvulcan.cab
del %OUT%xsharp.cab
del %OUT%xsharp.mshc
del %OUT%xsharp.chm
del %OUT%xsharp.chw
del %OUT%xsharp.pdf
del %OUT%helpcontentsetup.msha
SET HM="C:\Program Files (x86)\EC Software\HelpAndManual7\HELPMAN.EXE" \Xsharp\Dev\XSharp\docs\XSHelp.hmxp /stdout
%HM% "/tsk=Recent Publishes:Publish as CHM" "/tsk=Recent Publishes:Publish as HTM"  "/tsk=Recent Publishes:VS Help"  "/tsk=Recent Publishes:Publish as PDF"
makecab %OUT%xsvulcan.mshc %OUT%xsvulcan.cab
makecab %OUT%xsharp.mshc %OUT%xsharp.cab
SET HM=
SET OUT=
pause