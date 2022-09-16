@echo off
set xsoldpath=%path%
SET VSVERSION=2019
if "%VSEDITION%" == "" SET VSEDITION=Enterprise
set VsBatch="C:\Program Files (x86)\Microsoft Visual Studio\%VSVERSION%\%VSEDITION%\Common7\Tools\VsDevCmd.bat"
if not exist %VsBatch% goto VsError
if "%VSSDKINSTALL%" == "" call %VsBatch%
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok
Echo Building VsIntegration 2019 %1 Configuration
"%msbuilddir%msbuild" VsIntegration.sln      /fl1 /flp1:Append;Verbosity=diag /p:Configuration=%1   /t:Rebuild  /m /v:q /nologo
if exist build-%1.log del build-%1.log
rename msbuild1.log build-%1.log
Goto End
:Error
echo Syntax: Build2019 Debug or Build Release
goto End
:VsError
echo Cannot locate find VsBatch batchfile
goto end
:End
set path=%xsoldpath%
set xsoldpath=

