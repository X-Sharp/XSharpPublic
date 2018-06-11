nuget restore NugetBootstrap\NugetBootstrap.csproj -packagesdir Binaries\Packages
nuget restore compiler.sln
nuget restore macrocompiler.sln
nuget restore tools.sln
rebuild.cmd
