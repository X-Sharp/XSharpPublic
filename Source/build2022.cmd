@echo off
set xsoldpath=%path%
SET VSVERSION=2022
if "%VSEDITION%" == "" SET VSEDITION=Enterprise
set VsBatch="C:\Program Files\Microsoft Visual Studio\%VSVERSION%\%VSEDITION%\Common7\Tools\VsDevCmd.bat"
if not exist %VsBatch% goto VsError
if "%VSSDKINSTALL%" == "" call %VsBatch%
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok
Echo Building VsIntegration 32 bits  %1 Configuration
"%msbuilddir%msbuild" VsIntegration.sln  /fl1 /flp1:Append;Verbosity=diag /p:Configuration=%1 /t:Rebuild  /m /v:q /nologo 
if exist build2019-%1.log del build2019-%1.log
rename msbuild1.log build2019-%1.log
Echo Building VsIntegration 64 bits  %1 Configuration
"%msbuilddir%msbuild" VsIntegration2022.sln  /fl2 /flp1:Append;Verbosity=diag /p:Configuration=%1 /t:Rebuild  /m /v:q /nologo 
if exist build2022-%1.log del build2022-%1.log
rename msbuild2.log build2022-%1.log
Goto End
:Error
echo Syntax: Build2022 Debug or Build Release
goto End
:VsError
echo Cannot locate VS %VSVERSION% Command prompt
goto end
:End
set path=%xsoldpath%
set xsoldpath=

