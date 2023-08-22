@echo off
set xsoldpath=%path%
set xsdotnetpath=%~dp0\Artifacts\Tools\dotnet
set path=%xsdotnetpath%;%PATH%
if "%VSVERSION%" == "" SET VSVERSION=2022
if "%VSEDITION%" == "" SET VSEDITION=Enterprise
if "%VsBatch%" == "" set VsBatch=C:\Program Files\Microsoft Visual Studio\%VSVERSION%\%VSEDITION%\Common7\Tools\VsDevCmd.bat
if not exist "%VsBatch%" goto VsError
if "%VSSDKINSTALL%" == "" call "%VsBatch%"
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Public" goto Ok
if /i "%1" == "Release" goto Ok
if /i "%1" == "All" goto All
goto Error
:All
Echo Restore nuget packages once for all builds
dotnet restore Compiler.sln  -p:Configuration=Release
SET XSHARPBUILDNESTED=1
call BuildCompiler Debug
call BuildCompiler Release
call BuildCompiler Public
call BuildMacroCompiler.cmd
SET XSHARPBUILDNESTED=
Goto End
:Ok
set tmpXSharpDev=%XSharpDev%
rem Reset the XSharpDev path so we will not use the compiler we are generating
set XSharpDev=
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe" >nul
taskkill  /f /t /fi "IMAGENAME eq VBCSCompiler.exe" >nul
rem Next line can help to debug where DotNet.exe finds the right SDK
rem SET COREHOST_TRACE=1
if "%XSHARPBUILDNESTED%" == "1" goto Build
Echo Restoring packages for configuration %1 
dotnet restore Compiler.sln  -p:Configuration=%1
:Build
Echo Building output for configuration %1
msbuild Compiler.sln /fl1 /p:Configuration=%1 /t:Build /v:m /nologo /m
if exist build-%1.log del build-%1.log
rename msbuild1.log build-%1.log
set XSharpDev=%tmpXSharpDev%
Goto End
:Error
echo Syntax: BuildCompiler Debug , BuildCompiler Public or BuildCompiler Release 
goto End
:VsError
echo Could not find batch file %VsBatch%
:End
set path=%xsoldpath%
set xsoldpath=
set xsdotnetpath=