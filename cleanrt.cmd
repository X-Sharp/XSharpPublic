@echo off
Echo Cleaning Runtime Solution
msbuild Runtime.sln /fl1 /p:Configuration=Debug	/t:Clean /m /v:m 
msbuild Runtime.sln /fl2 /p:Configuration=Release	/t:Clean /m /v:m 
msbuild Runtime.sln /fl3 /p:Configuration=Documentation	/t:Clean /m /v:m 
if exist clean-debug.log del clean-debug.log
if exist clean-release.log del clean-release.log
if exist clean-documentation.log del clean-documentation.log
rename msbuild1.log clean-debug.log
rename msbuild2.log clean-release.log
rename msbuild3.log clean-documentation.log
if exist binaries\debug\*.* rd binaries\debug /s /q
if exist binaries\obj\*.* rd binaries\obj /s /q
if exist binaries\release\*.* rd binaries\release /s /q
if exist binaries\documentation\*.* rd binaries\documentation /s /q