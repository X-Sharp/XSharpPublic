@echo off
if /i "%1" == "Debug" goto Ok
if /i "%1" == "Release" goto Ok
goto Error
:Ok
call build2019 %1
call build2022 %1
:Error
echo Syntax: Build Debug or Build Release
goto End
:End
