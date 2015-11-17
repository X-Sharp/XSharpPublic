@echo off
SET Version=0.1.0
Echo Building Compiler version %Version%
Pause
msbuild Compiler.sln /fl1 /p:Configuration=Debug	/p:RoslynSemanticVersion=%Version% /t:Rebuild /property:OfficialBuild=true /m /v:m 
msbuild Compiler.sln /fl2 /p:Configuration=Release	/p:RoslynSemanticVersion=%Version% /t:Rebuild /property:OfficialBuild=true /m /v:m 
