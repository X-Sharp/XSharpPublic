@echo off
if "%1" == "Debug" goto Ok
if "%1" == "debug" goto Ok
if "%1" == "Release" goto Ok
if "%1" == "release" goto Ok
goto Error
:Ok
Echo Building VsIntegration and Tools
msbuild VsIntegration.sln 	/fl1 /flp1:Append /p:Configuration=%1 /p:Platform=x86     /t:Build  /m /v:m /nologo 
msbuild Tools.sln 		   /fl2 /flp1:Append /p:Configuration=%1 /p:Platform="Any CPU" /t:Build  /m /v:m /nologo 
if exist build-%1.log del build-%1.log
if exist buildt-%1.log del buildt-%1.log
rename msbuild1.log build-%1.log
rename msbuild2.log buildt-%1.log
Goto End
:Error
echo Syntax: Build Debug or Build Release
:End

