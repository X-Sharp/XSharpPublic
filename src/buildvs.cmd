@echo off
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok
call buildvs2019 %1
call buildvs2022 %1
:Error
echo Syntax: BuildVS Debug or BuildVS Release
goto End
:End
