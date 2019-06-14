@echo off
if "%libpath%" == "" goto NotFound
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
if /i "%1" == "Documentation" goto Ok
goto Error
:Ok
rem call findmsbuild.cmd
rem if "%msbuilddir%" == "" goto NotFound
:found
Echo Building Runtime %1 Configuration
rem Echo Using MsBuild in "%msbuilddir%"
msbuild Runtime.sln 		/fl1 /flp1:Append /p:Configuration=%1	/p:Platform="Any CPU"     /t:Build  /m /v:m
rem /v:q /nologo 
if exist buildRt%1.log del buildRt%1.log
rename msbuild1.log buildRt%1.log
Goto End
:Error
echo Syntax: BuildRt Debug, BuildRt Release or BuildRt Documentation
goto end
:NotFound
Echo Make sure you run this cmd file from a Developer prompt
echo Cannot locate MsBuild.exe
goto end
:End


