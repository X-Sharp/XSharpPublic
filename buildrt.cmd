@echo off
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
if /i "%1" == "Documentation" goto Ok
goto Error
:Ok
Echo Building Runtime %1 Configuration
msbuild Runtime.sln 		/fl1 /flp1:Append /p:Configuration=%1	/p:Platform="Any CPU"     /t:Build  /m /v:q /nologo 
if exist buildRt%1.log del buildRt%1.log
rename msbuild1.log buildRt-%1.log
Goto End
:Error
echo Syntax: BuildRt Debug, BuildRt Release or BuildRt Documentation
:End

