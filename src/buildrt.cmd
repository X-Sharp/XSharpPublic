@echo off
rem When running in a VS command prompt there should be an environment variable VSSDKINSTALL
if not "%VSSDKINSTALL%" == "" goto Build
rem Try to find the VS build.
if "%VSVERSION%" == "" SET VSVERSION=2022
if "%VSEDITION%" == "" SET VSEDITION=Enterprise
if "%VsBatch%" == "" set VsBatch=C:\Program Files\Microsoft Visual Studio\%VSVERSION%\%VSEDITION%\Common7\Tools\VsDevCmd.bat
if not exist "%VsBatch%" goto VsError
call %VsBatch%
:Build
if "%libpath%" == "" goto NotFound
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
if /i "%1" == "Documentation" goto Ok
if /i "%1" == "DocChinese" goto Ok
if /i "%1" == "All" goto All
goto Error
:All
for %%i in (Debug, Release, Documentation, DocChinese) do call buildRt %%i
goto End
:Ok
Echo Building Runtime %1 Configuration
if not "%XSHARPDEV%" == "" Echo Using X# development compiler in "%XSHARPDEV%"
if /i "%1" == "Documentation" goto Docs
if /i "%1" == "DocChinese"    goto Docs
msbuild Runtime.sln 		/fl1 /flp1:Append /p:Configuration=%1	/p:Platform="Any CPU"     /t:Build  /m /v:m
goto Rest
:Docs
msbuild RuntimeDocs.sln 		/fl1 /flp1:Append /p:Configuration=%1	/p:Platform="Any CPU"     /t:Build  /m /v:m
rem /v:q /nologo 
:Rest
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


