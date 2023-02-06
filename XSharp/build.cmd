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
call Build Debug
call Build Release
call Build Public
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
set AntlrCall=java -jar eng\antlr4-csharp-4.6.1-SNAPSHOT-complete.jar
set AntlrPackage=-package LanguageService.CodeAnalysis.XSharp.SyntaxParser
set AntlrOutputDir=%~dp0src\Compiler\XSharpCodeAnalysis\Generated
set AntlrInputDir=%~dp0src\Compiler\XSharpCodeAnalysis\Parser\
set AntlrParams=-long-messages  -message-format vs2005 -Dlanguage=CSharp_v4_5 %AntlrPackage% -listener -o %AntlrOutputdir%
rem echo %AntlrCall% %AntlrParams% %AntlrInputdir%XSharpLexer.g4
rem echo %AntlrCall% %AntlrParams% %AntlrInputdir%XSharp.g4
if exist "%AntlrOutputdir%\XSharp*.*" del %AntlrOutputdir%\XSharp*.* /q
%AntlrCall% %AntlrParams% %AntlrInputdir%XSharpLexer.g4
%AntlrCall% %AntlrParams% %AntlrInputdir%XSharp.g4
rem There seems to be a problem packing the zips on the CI server, so create the folder "manually"
IF NOT EXIST %~dp0artifacts\Zips\*.* md %~dp0artifacts\Zips
SET CreateZips=
IF /i "%1" == "Release" SET CreateZips=Yes
msbuild Compiler.sln /fl1 /p:Configuration=%1 /t:Build /v:m /nologo /m
if exist build-%1.log del build-%1.log
rename msbuild1.log build-%1.log
set XSharpDev=%tmpXSharpDev%
Goto End
:Error
echo Syntax: Build Debug , Build Public or Build Release 
goto End
:VsError
echo Could not find batch file %VsBatch%
:End
set path=%xsoldpath%
set xsoldpath=
set xsdotnetpath=