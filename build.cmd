@echo off
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok
Echo Building VsIntegration and Tools %1 Configuration
msbuild VsIntegration.sln  /fl1 /flp1:Append /p:Configuration=%1 /p:Platform=x86     /t:Build  /m /v:q /nologo 
msbuild Tools.sln 		   /fl2 /flp1:Append /p:Configuration=%1 /p:Platform="Any CPU" /t:Build  /m /v:q /nologo 
if exist build-%1.log del build-%1.log
if exist tools-%1.log del tools-%1.log
rename msbuild1.log build-%1.log
rename msbuild2.log tools-%1.log
Goto End
:Error
echo Syntax: Build Debug or Build Release
:End

