dotnet restore MacroCompiler.sln
msbuild MacroCompiler.sln /fl1 /p:Configuration=Release /t:Build /v:m /nologo /m /p:ResolveNuGetPackages=false
if exist build-MC-Release.log del build-MC-Release.log
rename msbuild1.log build-MC-Release.log
msbuild MacroCompiler.sln /fl1 /p:Configuration=Debug /t:Build /v:m /nologo /m /p:ResolveNuGetPackages=false
if exist build-MC-Debug.log del build-MC-Debug.log
rename msbuild1.log build-MC-Debug.log


