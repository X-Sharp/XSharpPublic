@echo off
SET OUT=Binaries\Help\
if exist %OUT%xsvulcan.cab del %OUT%xsvulcan.cab
if exist %OUT%xsharp.cab del %OUT%xsharp.cab
if exist %OUT%xsharp.mshc del %OUT%xsharp.mshc
if exist %OUT%xsharp.chm del %OUT%xsharp.chm
if exist %OUT%xsharp.chw del %OUT%xsharp.chw
if exist %OUT%xsharp.pdf del %OUT%xsharp.pdf
if exist del %OUT%helpcontentsetup.msha
SET HM="C:\Program Files (x86)\EC Software\HelpAndManual7\HELPMAN.EXE" \Xsharp\Dev\XSharp\docs\XSHelp.hmxp /stdout
Echo =======================
Echo Creating CHM
Echo =======================
%HM% "/tsk=Recent Publishes:Publish as CHM" 
Echo =======================
Echo Creating WebHelp 
Echo =======================
%HM% "/tsk=Recent Publishes:Publish as HTM"  
Echo =======================
Echo Creating VsHelp 
Echo =======================
%HM% "/tsk=Recent Publishes:VS Help"  
Echo =======================
Echo Creating PDF
Echo =======================
%HM% "/tsk=Recent Publishes:Publish as PDF"
makecab %OUT%xsharp.mshc %OUT%xsharp.cab
SET HM=
SET OUT=
