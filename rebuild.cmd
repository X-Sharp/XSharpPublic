@echo off
Echo Cleaning Binaries folder
if exist Binaries\Debug  rd Binaries\Debug /s /q
if exist Binaries\Release rd Binaries\Release /s /q
if exist Binaries\Obj rd Binaries\Obj /s /q
Echo Building VsIntegration 
msbuild VsIntegration.sln /fl1 /p:Configuration=Debug	/t:ReBuild /property:OfficialBuild=true /m /v:m /nologo
msbuild VsIntegration.sln /fl2 /p:Configuration=Release	/t:ReBuild /property:OfficialBuild=true /m /v:m /nologo
if exist build-debug.log del build-debug.log
if exist build-release.log del build-release.log
rename msbuild1.log build-debug.log
rename msbuild2.log build-release.log
