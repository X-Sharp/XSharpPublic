set mainhelp=\xsharp\dev\xsharp\binaries\help
set rthelp1=\xsharp\devrt\binaries\help
set rthelp2=\xsharp\devrt\binaries\vohelp
set setup=c:\XSharp\Dev\XSharp\src\Setup
copy xsharp.msha %mainhelp%
cd %mainhelp%
makecab xsharp.mshc xsharp.cab /V1
cd %rthelp1%
makecab xsruntime.mshc xsruntime.cab /V1
copy xsruntime.mshc %mainhelp%
copy xsruntime.cab %mainhelp%
cd %rthelp2%
makecab XsVoSdk.mshc XsVoSdk.cab /V1
copy XsVoSdk.mshc %mainhelp%
copy XsVoSdk.cab  %mainhelp%
cd %setup%

