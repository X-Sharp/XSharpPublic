set nugetdir=%userprofile%\.nuget\packages\nuget.commandline\4.6.2\tools\

%nugetdir%nuget restore compiler.sln
%nugetdir%nuget restore macrocompiler.sln
%nugetdir%nuget restore tools.sln
rebuild.cmd
