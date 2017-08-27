@echo off
if "%1" == "Debug" goto Ok
if "%1" == "debug" goto Ok
if "%1" == "Release" goto Ok
if "%1" == "release" goto Ok
goto Error
:Ok
Echo Building Compiler 
rem Reset the XSharpDev path so we will not use the compiler we are generating
set tmpXSharpDev=%XSharpDev%
set XSharpDev=
msbuild Compiler.sln 		/fl1 /flp1:Append /p:Configuration=%1	    /t:Build /p:OfficialBuild=true /m /v:m /nologo
if exist build-%1.log del build-%1.log
rename msbuild1.log build-%1.log
set XSharpDev=%tmpXSharpDev%
Goto End
:Error
echo Syntax: Build Debug or Build Release
:End

