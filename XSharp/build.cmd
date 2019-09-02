@echo off
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Public" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok
call findmsbuild.cmd
if "%msbuilddir%" == "" goto NotFound
:found
set tmpXSharpDev=%XSharpDev%
rem Reset the XSharpDev path so we will not use the compiler we are generating
set XSharpDev=
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe" >nul
Echo Building Compiler %1 Configuration  with Compiler.sln
Echo Using MsBuild in %msbuilddir%
"%msbuilddir%msbuild" Compiler.sln /fl1 /p:Configuration=%1		/t:Build /v:m /nologo
if exist build-%1.log del build-%1.log
rename msbuild1.log build-%1.log
set XSharpDev=%tmpXSharpDev%
Goto End
:Error
echo Syntax: Build Debug , Build Public or Build Release 
goto end
:NotFound
echo Cannot locate VS 2017 MsBuild.exe
goto end
:End
