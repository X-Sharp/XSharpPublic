@echo off
Echo Cleaning Binaries folder
if exist Binaries\Debug  rd Binaries\Debug /s /q
if exist Binaries\Release rd Binaries\Release /s /q
if exist Binaries\Obj rd Binaries\Obj /s /q
Echo Building Runtime 
SET XSHARPDEV=\XSharp\Dev\Xsharp\Binaries\Debug_AnyCPU
call buildRt Debug
call buildRt Release
