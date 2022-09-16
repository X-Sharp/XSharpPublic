@echo off
if "%VCINSTALLDIR"== "" goto setVsPath
goto rest
:setVsPath
call "C:\Program Files\Microsoft Visual Studio\2022\Enterprise\Common7\Tools\VsDevCmd.bat"
:rest
if "%libpath%" == "" goto NotFound
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
if /i "%1" == "Documentation" goto Ok
if /i "%1" == "All" goto All
goto Error
:All
for %%i in (Debug, Release, Documentation) do call buildRt %%i
goto End
:Ok
Echo Building Runtime %1 Configuration
if not "%XSHARPDEV%" == "" Echo Using X# development compiler in "%XSHARPDEV%"
msbuild Runtime.sln 		/fl1 /flp1:Append /p:Configuration=%1	/p:Platform="Any CPU"     /t:Build  /m /v:m
rem /v:q /nologo 
if exist buildRt%1.log del buildRt%1.log
rename msbuild1.log buildRt%1.log
Goto End
:Error
echo Syntax: BuildRt Debug, BuildRt Release, BuildRt Documentation or BuildRt All
goto end
:NotFound
Echo Make sure you run this cmd file from a Developer prompt
echo Cannot locate MsBuild.exe
goto end
:End


