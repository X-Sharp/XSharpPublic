@echo off
set xsoldpath=%path%
set xsdotnetpath=%~dp0\Binaries\Tools\dotnet
set path=%xsdotnetpath%;%PATH%
if "%VSVERSION%" == "" SET VSVERSION=2017
if "%VSEDITION%" == "" SET VSEDITION=Enterprise
set VsBatch="C:\Program Files (x86)\Microsoft Visual Studio\%VSVERSION%\%VSEDITION%\Common7\Tools\VsDevCmd.bat"
if not exist %VsBatch% goto VsError
if "%VSSDKINSTALL%" == "" call %VsBatch%
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok

Echo Building VsIntegration and Tools %1 Configuration
"%msbuilddir%msbuild" VsIntegration.sln  /fl1 /flp1:Append;Verbosity=diag /p:Configuration=%1 /p:Platform=x86     /t:Build  /m /v:q /nologo 
"%msbuilddir%msbuild" Tools.sln 		   /fl2 /flp1:Append;Verbosity=diag /p:Configuration=%1 /p:Platform="Any CPU" /t:Build  /v:q /m /nologo 
if exist build-%1.log del build-%1.log
if exist tools-%1.log del tools-%1.log
rename msbuild1.log build-%1.log
rename msbuild2.log tools-%1.log
Goto End
:Error
echo Syntax: Build Debug or Build Release
goto End
:NotFound
echo Cannot locate VS 2017 MsBuild.exe
goto end
:End

