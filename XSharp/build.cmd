@echo off
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Public" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok
rem call findmsbuild.cmd
if "%VSINSTALLDIR%" == "" goto NotFound
:found
set tmpXSharpDev=%XSharpDev%
rem Reset the XSharpDev path so we will not use the compiler we are generating
set XSharpDev=
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe" >nul
Echo Building Compiler %1 Configuration with Compiler.sln
Echo Using VS location in %VSINSTALLDIR%
msbuild Compiler.sln /fl1 /p:Configuration=%1		/t:Build /v:m /nologo
if exist build-%1.log del build-%1.log
rename msbuild1.log build-%1.log
set XSharpDev=%tmpXSharpDev%
Goto End
:Error
echo Syntax: Build Debug , Build Public or Build Release 
goto end
:NotFound
echo Cannot locate VS 
goto end
:End
