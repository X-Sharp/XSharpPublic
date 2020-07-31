@echo on
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\Common7\Tools\VsDevCmd.bat"
echo Start restoring Roslyn ...
call "%~dp0\Roslyn\Restore.cmd"
echo Start building Roslyn Compiler ...
msbuild "%~dp0\Roslyn\Compilers.sln" /v:m /m
echo Start building XSharp Compilers (all 3 configurations)
cd "%~dp0\XSharp"
call Build.cmd All

