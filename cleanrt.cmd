@echo off
Echo Cleaning Runtime Solution
msbuild Runtime.sln /fl1 /p:Configuration=Debug			/t:Clean /m /v:q  
msbuild Runtime.sln /fl2 /p:Configuration=Release		/t:Clean /m /v:q  
msbuild Runtime.sln /fl3 /p:Configuration=Documentation	/t:Clean /m /v:q 
if exist cleanrt*.log del cleanrt*.log
rename msbuild1.log cleanrt-debug.log
rename msbuild2.log cleanrt-release.log
rename msbuild3.log cleanrt-documentation.log
if exist binaries\debug\*.* rd binaries\debug /s /q
if exist binaries\obj\*.* rd binaries\obj /s /q
if exist binaries\release\*.* rd binaries\release /s /q
if exist binaries\documentation\*.* rd binaries\documentation /s /q
