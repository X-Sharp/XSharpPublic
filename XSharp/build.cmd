@echo off
SET Version=0.1.0
Echo Building Compiler version %Version%
msbuild Compiler.sln /fl1 /p:Configuration=Debug	/p:RoslynSemanticVersion=%Version% /t:Build /property:OfficialBuild=true /m /v:m /nologo
msbuild Compiler.sln /fl2 /p:Configuration=Release	/p:RoslynSemanticVersion=%Version% /t:Build /property:OfficialBuild=true /m /v:m /nologo
if exist build-debug.log del build-debug.log
if exist build-release.log del build-release.log
rename msbuild1.log build-debug.log
rename msbuild2.log build-release.log