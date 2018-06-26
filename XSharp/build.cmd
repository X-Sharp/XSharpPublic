@echo off
set tmpXSharpDev=%XSharpDev%
rem Reset the XSharpDev path so we will not use the compiler we are generating
set XSharpDev=
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe"
Echo Building Compiler 
msbuild Master.sln /fl1 /p:Configuration=Debug		/t:Build /m /v:q /nologo
msbuild Master.sln /fl2 /p:Configuration=Release	/t:Build /m /v:q /nologo
if exist build-debug.log del build-debug.log
if exist build-release.log del build-release.log
rename msbuild1.log build-debug.log
rename msbuild2.log build-release.log
set XSharpDev=%tmpXSharpDev%