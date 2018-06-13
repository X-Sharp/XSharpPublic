@echo off
Echo Cleaning Binaries folder
if exist Binaries\Debug  rd Binaries\Debug /s /q
if exist Binaries\Release rd Binaries\Release /s /q
if exist Binaries\Obj rd Binaries\Obj /s /q
Echo Building VsIntegration and Tools
msbuild VsIntegration.sln /fl1 /p:Configuration=Debug	/p:Platform=x86 /t:ReBuild /m /v:m /nologo
msbuild VsIntegration.sln /fl2 /p:Configuration=Release	/p:Platform=x86 /t:ReBuild /m /v:m /nologo
if exist build-debug.log del build-debug.log
if exist build-release.log del build-release.log
rename msbuild1.log build-debug.log
rename msbuild2.log build-release.log
msbuild Tools.sln /fl1 /p:Configuration=Debug	/p:Platform="Any CPU" /t:ReBuild /m /v:m /nologo
msbuild Tools.sln /fl2 /p:Configuration=Release	/p:Platform="Any CPU" /t:ReBuild /m /v:m /nologo
if exist buildt-debug.log del buildt-debug.log
if exist buildt-release.log del buildt-release.log
rename msbuild1.log buildt-debug.log
rename msbuild2.log buildt-release.log
