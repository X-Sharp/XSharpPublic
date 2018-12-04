@echo on
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok
set msbuilddir=c:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\MSBuild\15.0\Bin\
if exist "%msbuilddir%msbuild.exe" goto found
set msbuilddir=c:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\MSBuild\15.0\Bin\
if exist "%msbuilddir%msbuild.exe" goto found
set msbuilddir=c:\Program Files (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin\
if exist "%msbuilddir%msbuild.exe" goto found
goto notfound

:found
set tmpXSharpDev=%XSharpDev%
rem Reset the XSharpDev path so we will not use the compiler we are generating
set XSharpDev=
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe" >nul
Echo Building Compiler %1 Configuration
"%msbuilddir%msbuild" Compiler.sln /fl1 /p:Configuration=%1		/t:Build /m /v:m /nologo
if exist build-%1.log del build-%1.log
rename msbuild1.log build-%1.log
set XSharpDev=%tmpXSharpDev%
Goto End
:Error
echo Syntax: Build Debug or Build Release 
goto end
:NotFound
echo Cannot locate VS 2017 MsBuild.exe
goto end
:End
