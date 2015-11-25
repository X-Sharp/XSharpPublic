@echo off
if "%1" == "Debug" goto Ok
if "%1" == "debug" goto Ok
if "%1" == "Release" goto Ok
if "%1" == "release" goto Ok
goto Error
:Ok
call Setversion
Echo Building Compiler version %Version%
msbuild Compiler.sln /fl1 /p:Configuration=%1	/p:RoslynSemanticVersion=%Version% /t:Build /property:OfficialBuild=true /m /v:m /nologo
if exist build-%1.log del build-%1.log
rename msbuild1.log build-%1.log
Goto End
:Error
echo Syntax: Build Debug or Build Release
:End

