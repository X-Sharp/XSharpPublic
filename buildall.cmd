@echo off
Color 07
cd \xsharp\dev\Roslyn
Echo Restore Nuget Packages for Roslyn code
call restore.cmd
cd \xsharp\dev\xsharp
Echo kill running XSCompiler processes
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe"
Echo Restore Nuget Packages for Compiler and Tools
nuget restore compiler.sln -V q
nuget restore macrocompiler.sln -V q
nuget restore tools.sln -V q
Echo Clean Compiler output folders
call clean.cmd
Echo Build Compiler and Tools - Release version
call build release
Echo Build Compiler and Tools - Debug version
call build debug
Echo Build Runtime
cd \xsharp\devrt
call rebuildrt.cmd
Echo build XML for documentation
msbuild runtime.sln /p:Configuration=Documentation /t:Build /m /v:m /nologo
Echo build reference documentation
msbuild docs.shfbproj /p:Configuration=Debug /t:Build /m /v:m /nologo
msbuild vodocs.shfbproj /p:Configuration=Debug /t:Build /m /v:m /nologo
Echo Build VSIntegration
cd \xsharp\devpublic
call rebuild.cmd
cd \xsharp\dev\xsharp
if exist msbuild3.log del msbuild3.log
Echo recompile macrocompiler which depends on xsharp.core and xsharp.vo
msbuild macrocompiler.sln /fl3 /p:Configuration=Release	/t:Build /m /v:m /nologo
if exist build-macro.log del build-macro.log
rename msbuild3.log build-macro.log
Echo Creating Help files
call createhelp.cmd
Echo Create installer
"c:\Tools\Inno Setup 5\ISCC.exe" /DCompression=lzma2/ultra64  /Q "c:\XSharp\Dev\XSharp\src\Setup\XSharpSetup.iss" 
