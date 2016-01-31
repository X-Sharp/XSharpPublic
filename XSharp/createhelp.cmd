@echo off
SET HM="C:\Program Files (x86)\EC Software\HelpAndManual7\HELPMAN.EXE" D:\Xsharp\Dev\XSharp\docs\XSHelp.hmxp /stdout
%HM% "/tsk=Recent Publishes:Publish as CHM" 
%HM% "/tsk=Recent Publishes:Publish as HTM" 
%HM% "/tsk=Recent Publishes:VS Help" 
%HM% "/tsk=Recent Publishes:Publish as PDF"
SET HM=