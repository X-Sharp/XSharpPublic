@echo off
Echo Cleaning Binaries folder
rem Reset the XSharpDev path so we will not use the compiler we are generating
set tmpXSharpDev=%XSharpDev%
set XSharpDev=
if exist Binaries\Debug_AnyCPU 		rd Binaries\Debug_AnyCPU /s /q
if exist Binaries\Release_AnyCPU 	rd Binaries\Release_AnyCPU /s /q
if exist Binaries\Obj				rd Binaries\Obj /s /q
Echo Building Compiler 
msbuild Compiler.sln /fl1 /p:Configuration=Debug	/t:ReBuild /property:OfficialBuild=true /m /v:m /nologo
msbuild Compiler.sln /fl2 /p:Configuration=Release	/t:ReBuild /property:OfficialBuild=true /m /v:m /nologo
if exist build-debug.log del build-debug.log
if exist build-release.log del build-release.log
rename msbuild1.log build-debug.log
rename msbuild2.log build-release.log
set XSharpDev=%tmpXSharpDev%