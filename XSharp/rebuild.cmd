@echo off
Echo Cleaning Binaries folder
rem Reset the XSharpDev path so we will not use the compiler we are generating
set tmpXSharpDev=%XSharpDev%
set XSharpDev=
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe"
if exist Binaries\Debug_AnyCPU 		rd Binaries\Debug_AnyCPU /s /q
if exist Binaries\Release_AnyCPU 	rd Binaries\Release_AnyCPU /s /q
if exist Binaries\Obj				rd Binaries\Obj /s /q
Echo Building Compiler 
msbuild Master.sln /fl1 /p:Configuration=Debug		/t:Build /property:OfficialBuild=true /m /v:m /nologo
msbuild Master.sln /fl2 /p:Configuration=Release	/t:Build /property:OfficialBuild=true /m /v:m /nologo
if exist rebuild-debug.log del rebuild-debug.log
if exist rebuild-release.log del rebuild-release.log
rename msbuild1.log rebuild-debug.log
rename msbuild2.log rebuild-release.log
set XSharpDev=%tmpXSharpDev%