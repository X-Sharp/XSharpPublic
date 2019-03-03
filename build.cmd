@echo off
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok
set msbuilddir=c:\Program Files (x86)\MSBuild\14.0\Bin\
if "%msbuilddir%" == "" goto NotFound
:found

Echo Building VsIntegration and Tools %1 Configuration
"%msbuilddir%msbuild" VsIntegration.sln  /fl1 /flp1:Append /p:Configuration=%1 /p:Platform=x86     /t:Build  /m /v:q /nologo 
"%msbuilddir%msbuild" Tools.sln 		   /fl2 /flp1:Append /p:Configuration=%1 /p:Platform="Any CPU" /t:Build  /m /v:q /nologo 
if exist build-%1.log del build-%1.log
if exist tools-%1.log del tools-%1.log
rename msbuild1.log build-%1.log
rename msbuild2.log tools-%1.log
Goto End
:Error
echo Syntax: Build Debug or Build Release
goto End
:NotFound
echo Cannot locate VS 2017 MsBuild.exe
goto end
:End

