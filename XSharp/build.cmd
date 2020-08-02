@echo off
if "%VSSDKINSTALL%" == "" call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\Common7\Tools\VsDevCmd.bat"
set path=%~dp0\..\Roslyn\Binaries\Tools\dotnet;%path%
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Public" goto Ok
if /i "%1" == "Release" goto Ok
if /i "%1" == "All" goto All
goto Error
:All
Echo Restore nuget packages once for all builds
dotnet restore Compiler.sln 
SET XSHARPBUILDNESTED=1
call Build Debug
call Build Release
call Build Public
SET XSHARPBUILDNESTED=
Goto End
:Ok
set tmpXSharpDev=%XSharpDev%
rem Reset the XSharpDev path so we will not use the compiler we are generating
set XSharpDev=
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe" >nul
rem Next line can help to debug where DotNet.exe finds the right SDK
rem SET COREHOST_TRACE=1
if "%XSHARPBUILDNESTED%" == "1" goto Build
Echo Restoring packages for configuration %1 
dotnet restore Compiler.sln 
:Build
Echo Building output for configuration %1
msbuild Compiler.sln /fl1 /p:Configuration=%1 /t:Build /v:m /nologo
if exist build-%1.log del build-%1.log
rename msbuild1.log build-%1.log
set XSharpDev=%tmpXSharpDev%
Goto End
:Error
echo Syntax: Build Debug , Build Public or Build Release 
goto End
:End
