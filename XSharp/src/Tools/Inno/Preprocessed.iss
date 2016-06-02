; BEGIN ISPPBUILTINS.ISS


; END ISPPBUILTINS.ISS

; Please note that the "deregistering" of the XSharp association is done in a script step at the end of this file


;Folders

;#define Compression     "none"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{32EB192A-B120-4055-800E-74B48B80DA06}
DisableWelcomePage=no
DisableStartupPrompt=yes
DisableReadyMemo=yes
DisableFinishedPage=no
InfoBeforeFile=Baggage\ReadmeShort.rtf
AppName=XSharp
AppVersion=0.2.5.2500
AppCopyright=Copyright © 2015-2016 XSharp B.V.
AppVerName=XSharp 0.2.5
AppPublisher=XSharp BV
AppPublisherURL=http://www.xsharp.info
AppSupportURL=http://www.xsharp.info
AppUpdatesURL=http://www.xsharp.info
DefaultDirName={pf}\XSharp
DefaultGroupName=XSharp
LicenseFile=Baggage\License.txt
OutputDir=D:\XSharp\Dev\XSharp\Binaries\Setup 
OutputBaseFilename=XSharpSetup025
OutputManifestFile=Setup-Manifest.txt
SetupIconFile=Baggage\XSharp.ico
Compression=lzma2/ultra64
SolidCompression=yes
SetupLogging=yes

; Version Info for Installer and uninstaller
VersionInfoVersion=0.2.5.2500
VersionInfoDescription=XSharp Beta 5
VersionInfoCompany=XSharp BV
VersionInfoTextVersion=0.2.5.2500 (Beta 5)
VersionInfoCopyRight=Copyright © 2015-2016 XSharp B.V.
VersionInfoProductName=XSharp
VersionInfoProductVersion=0.2.5.2500
Wizardsmallimagefile=Baggage\XSharp_Bmp_Banner.bmp 
WizardImagefile=Baggage\XSharp_Bmp_Dialog.bmp

;Uninstaller
UninstallFilesDir={app}\uninst
UninstallDisplayName=XSharp Beta 5
UninstallDisplayIcon={app}\Images\XSharp.ico;
UninstallLogMode=overwrite


TouchDate=2016-06-04
TouchTime=02:05:00




; Make sure they are admins
PrivilegesRequired=admin
; Make sure they are running on Windows 2000 Or Higher
Minversion=6.0.600


[Components]
Name: "main";   Description: "The XSharp Compiler and Build System";  Types: full compact custom; Flags: fixed; 
Name: "vs2015"; Description: "Visual Studio 2015 Integration";        Types: full custom;                  Check: Vs2015IsInstalled;
Name: "vs2015\vulcanprg"; Description: "Keep Vulcan associated with PRG files"; Types: full custom;                  Check: VulcanPrgAssociated;
Name: "vsnext"; Description: "Visual Studio 15 Preview Integration";  Types: full custom;                  Check: VsNextIsInstalled;
Name: "xide";   Description: "Include the XIDE files";                Types: full custom;                  


[Dirs]
Name: "{app}\Assemblies"
Name: "{app}\Bin"
Name: "{app}\Help"
Name: "{app}\Images"
Name: "{app}\Include"
Name: "{app}\ProjectSystem"
Name: "{app}\Redist"
Name: "{app}\Tools"
Name: "{app}\Uninst"
Name: "{app}\Xide"
Name: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Components: vs2015; 
Name: "{code:GetVsNextIdeDir}\Extensions\XSharp"; Components: vsNext; 


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\xsc.exe";                            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\xsc.rsp";                            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSCompiler.exe";                     DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.dll";            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main

; PDB files
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.pdb";            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\xsc.pdb";                            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSCompiler.pdb";                     DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main

; GAC files in Bin folder
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\System.Collections.Immutable.dll";   DestDir: "{app}\bin"; StrongAssemblyName: "System.Collections.Immutable, Version=1.1.37.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete; components: main
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\System.Reflection.Metadata.dll";     DestDir: "{app}\bin"; StrongAssemblyName: "System.Reflection.Metadata, Version=1.1.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a";    Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete; components: main

; Support files
Source: "Baggage\Readme.rtf";                             DestDir: "{app}"    ; Flags: isreadme ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main
Source: "Baggage\Redist.txt";                             DestDir: "{app}\Redist" ; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main
Source: "Baggage\XSharp.ico";                             DestDir: "{app}\Images"; Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main
;Source: "Baggage\License.rtf";                            DestDir: "{app}";        Flags: touch {#StdFlags}; Components: main
Source: "Baggage\License.txt";                            DestDir: "{app}";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main

; Include Files
Source: "D:\Xsharp\Dev\XSharp\src\Common\*.xh";                            DestDir: "{app}\Include"; Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main

;MsBuild Files
Source: "D:\Xsharp\DevPublic\Binaries\Debug\Xaml\*.*";                          DestDir: "{pf}\MsBuild\XSharp\Rules";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname uninsneveruninstall; Components: main
Source: "D:\Xsharp\DevPublic\Binaries\Debug\Targets\*.*";                       DestDir: "{pf}\MsBuild\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname uninsneveruninstall; Components: main
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharp.Build.dll";                  DestDir: "{pf}\MsBuild\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname uninsneveruninstall; Components: main

;Documentation
Source: "D:\Xsharp\Dev\XSharp\Binaries\Help\\XSharp.pdf";                        DestDir: "{app}\Help";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main
Source: "D:\Xsharp\Dev\XSharp\Binaries\Help\\XSharp.chm";                        DestDir: "{app}\Help";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: main

;XIDE
Source: "D:\Xsharp\Dev\XSharp\Xide\XIDE_Set_up_1.03.exe";                      DestDir: "{app}\Xide";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: Xide

;VsProjectSystem
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpProject2015.vsix";            DestDir: "{app}\ProjectSystem"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015 or vsnext

Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.dll";            DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.pdb";            DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpCodeDomProvider.dll";         DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete; StrongAssemblyName: "XSharp.CodeDom.XSharpCodeDomProvider, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31c59c566fa38f21"; Components: vs2015
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpCodeDomProvider.pdb";         DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpColorizer2015.dll";           DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpColorizer2015.pdb";           DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015

Source: "D:\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\*.*";                 DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates";     Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "D:\Xsharp\DevPublic\Binaries\Debug\ProjectTemplates\*.*";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ProjectTemplates";  Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015

Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpProject2015.dll";             DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpProject2015.dll.config";      DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpProject2015.pdb";             DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpProject2015.pkgdef";          DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015; AfterInstall: AdjustPkgDef;
Source: "D:\Xsharp\DevPublic\Binaries\Debug\extension.vsixmanifest";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015

Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharp.ico ";                              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "d:\Xsharp\Dev\XSharp\src\VisualStudio\XSharp.ProjectType\Images\XSharpImages.imagemanifest";  DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\Images";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.dll";                  DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp";         Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vs2015 

; VsNext
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.dll";            DestDir: "{code:GetVsNextIdeDir}\PrivateAssemblies"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.pdb";            DestDir: "{code:GetVsNextIdeDir}\PrivateAssemblies"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpCodeDomProvider.dll";         DestDir: "{code:GetVsNextIdeDir}\PrivateAssemblies"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete; StrongAssemblyName: "XSharp.CodeDom.XSharpCodeDomProvider, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31c59c566fa38f21"; Components: vsnext
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpCodeDomProvider.pdb";         DestDir: "{code:GetVsNextIdeDir}\PrivateAssemblies"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpColorizer2015.dll";           DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpColorizer2015.pdb";           DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext

Source: "D:\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\*.*";                 DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates";     Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\DevPublic\Binaries\Debug\ProjectTemplates\*.*";              DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ProjectTemplates";  Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext

Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpProject2015.dll";             DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpProject2015.dll.config";      DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpProject2015.pdb";             DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharpProject2015.pkgdef";          DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\DevPublic\Binaries\Debug\extension.vsixmanifest";            DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext

Source: "D:\Xsharp\DevPublic\Binaries\Debug\XSharp.ico ";                             DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";       Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "d:\Xsharp\Dev\XSharp\src\VisualStudio\XSharp.ProjectType\Images\XSharpImages.imagemanifest";  DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\Images"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext
Source: "D:\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.dll";                  DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname; Components: vsnext 

; Examples
Source: "D:\Xsharp\DevPublic\Samples\*.prg";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname;
Source: "D:\Xsharp\DevPublic\Samples\*.txt";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname;
Source: "D:\Xsharp\DevPublic\Samples\*.vh";                              DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname;
Source: "D:\Xsharp\DevPublic\Samples\*.sln";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname;
Source: "D:\Xsharp\DevPublic\Samples\*.xsproj";                          DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname;


; update machine.config
Source:"d:\Xsharp\Dev\XSharp\src\Tools\Various\RegisterProvider.exe";          DestDir: "{app}\Tools";                     Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname

[Icons]
Name: "{group}\{cm:ProgramOnTheWeb,XSharp}"; Filename: "http://www.xsharp.info";IconFilename:{app}\Images\XSharp.ico;
Name: "{group}\{cm:UninstallProgram,XSharp}"; Filename: "{uninstallexe}"; 
Name: "{group}\XSharp Documenation (CHM)"; Filename: "{app}\Help\XSharp.chm"; 
Name: "{group}\XSharp Documenation (PDF)"; Filename: "{app}\Help\XSharp.pdf"; 
Name: "{group}\{cm:UninstallProgram,XSharp}"; Filename: "{uninstallexe}"; 
Name: "{group}\XSharp Examples"; Filename: "{commondocs}\XSharp\Examples";
Name: "{app}\Examples";  Filename: "{commondocs}\XSharp\Examples";


[Registry]
Root: HKLM; Subkey: "Software\XSharpBV"; Flags: uninsdeletekeyifempty 
Root: HKLM; Subkey: "Software\XSharpBV\XSharp"; Flags: uninsdeletekey 
Root: HKLM; Subkey: "Software\XSharpBV\XSharp"; ValueName: "XSharpPath"; ValueType: string; ValueData: "{app}" ;

[Ini]
Filename: "{code:GetVs2015IdeDir}\Extensions\extensions.configurationchanged"; Section:"XSharp"; Key: "Installed"; String: "0.2.5.2500"; Flags: uninsdeletesection; Components: vs2015;
Filename: "{code:GetVsNextIdeDir}\Extensions\extensions.configurationchanged"; Section:"XSharp"; Key: "Installed"; String: "0.2.5.2500"; Flags: uninsdeletesection; Components: vsnext;

[Run]
Filename: "{app}\Tools\RegisterProvider.exe";
Filename:  "{app}\Xide\XIDE_Set_up_1.03.exe"; Description:"Run XIDE Installer"; Flags: postInstall;  Components: XIDE;

[UninstallRun]
; This XSharp program deletes the templates cache folder and the extensionmanager key in the registry
;Filename: "{app}\uninst\XsVsUnInst.exe"; Flags: runhidden;  Components: vs2015 ;

[InstallDelete]
; Template cache, component cache and previous installation of our project system
; vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\vtc"    ; Components: vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\ComponentModelCache"    ; Components: vs2015
Type: filesandordirs; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp";       Components: vs2015
Type: files;          Name: "{code:GetVs2015IdeDir}\XSharp.CodeAnalysis.dll"; Components: vs2015
Type: files;          Name: "{code:GetVs2015IdeDir}\XSharp.CodeAnalysis.pdb"; Components: vs2015
; vsnext
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\vtc"    ; Components: vsnext
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\ComponentModelCache"    ; Components: vsnext
Type: filesandordirs; Name: "{code:GetVsNextIdeDir}\Extensions\XSharp";       Components: vsnext
Type: files;          Name: "{code:GetVsNextIdeDir}\XSharp.CodeAnalysis.dll"; Components: vsnext
Type: files;          Name: "{code:GetVsNextIdeDir}\XSharp.CodeAnalysis.pdb"; Components: vsnext

; remove the old uninstaller because the uninstall file format has changed
Type: filesandordirs; Name: "{app}\Uninst"

[UninstallDelete]
Type: filesandordirs; Name: "{app}\Assemblies"                    ; Components: main
Type: filesandordirs; Name: "{app}\Bin"                           ; Components: main
Type: filesandordirs; Name: "{app}\Help"                          ; Components: main
Type: filesandordirs; Name: "{app}\Images"                        ; Components: main
Type: filesandordirs; Name: "{app}\ProjectSystem"                 ; Components: main
Type: filesandordirs; Name: "{app}\Redist"                        ; Components: main
Type: filesandordirs; Name: "{app}\Uninst"                        ; Components: main
Type: filesandordirs; Name: "{pf}\MsBuild\XSharp"             ; Components: main
Type: filesandordirs; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Components: vs2015;  
Type: filesandordirs; Name: "{code:GetVsNextIdeDir}\Extensions\XSharp"; Components: vsnext;  
Type: filesandordirs; Name: "{commondocs}\XSharp\Examples";
Type: dirifempty;     Name: "{app}"; 
Type: dirifempty;     Name: "{commondocs}\XSharp"; 

; Template cache and component cache
;vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\vtc"; 			Components: vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\ComponentModelCache"; 	Components: vs2015
;vsnext
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\vtc"; 			Components: vsnext
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\ComponentModelCache"; 	Components: vsnext

[Messages]
WelcomeLabel1=Welcome to XSharp (X#)
WelcomeLabel2=This installer will install XSharp Beta 5 on your computer.%n%nIt is recommended that you close all other applications before continuing, especially all running copies of Visual Studio.
WizardInfoBefore=Warning
InfoBeforeLabel=You are about to install Beta software
InfoBeforeClickLabel=Only continue the installation if you are aware of the following:


[Code]
Program setup;
var
  PrintButton: TButton;
  Vs2015Path : String;
  Vs2015Installed: Boolean;
  Vs2015BaseDir: String;
  VsNextPath : String;
  VsNextInstalled: Boolean;
  VsNextBaseDir: String;
  VulcanPrgAssociation: Boolean;
  VulcanGuid : String;

procedure PrintButtonClick(Sender: TObject);
var ResultCode :integer;
begin
ExtractTemporaryFile('license.txt');
if not ShellExec('Print', ExpandConstant('{tmp}\license.txt'),
     '', '', SW_SHOW, ewNoWait, ResultCode) then
end;
procedure DetectVS();
begin
  Vs2015Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\VisualStudio\SxS\VS7','14.0',Vs2015BaseDir) ;
  if Vs2015Installed then Vs2015Path := Vs2015BaseDir+'\Common7\Ide\';
  VsNextInstalled := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\VisualStudio\SxS\VS7','15.0',VsNextBaseDir) ;
  if VsNextInstalled then VsNextPath := VsNextBaseDir+'\Common7\Ide\';
  VulcanPrgAssociation := false;
  if Vs2015Installed then
  begin
  VulcanPrgAssociation := RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\Languages\File Extensions\.prg', '', VulcanGuid) ;
  if VulcanPrgAssociation then 
      begin
      VulcanPrgAssociation := (UpperCase(VulcanGuid) = '{8D3F6D25-C81C-4FD8-9599-2F72B5D4B0C9}');
      end
  end

end;


{function ResetExtensionManager: Boolean;
begin
  RegDeleteKeyIncludingSubKeys(HKEY_CURRENT_USER,'SOFTWARE\Microsoft\VisualStudio\14.0\ExtensionManager');
  result := True;
end;
}

function VulcanPrgAssociated: Boolean;
begin
  result := Vs2015Installed and VulcanPrgAssociation;
end;


function Vs2015IsInstalled: Boolean;
begin
  result := Vs2015Installed;
end;

function VsNextIsInstalled: Boolean;
begin
  result := VsNextInstalled;
end;

function GetVs2015IdeDir(Param: String): String;
begin
  result := Vs2015Path;
end;

function GetVsNextIdeDir(Param: String): String;
begin
  result := VsNextPath;
end;

Procedure CurPageChanged(CurPage: Integer);

begin
  PrintButton.Visible := CurPage = wpLicense;
end;

procedure InitializeWizard();
begin
    PrintButton := TButton.Create(WizardForm);
    PrintButton.Caption := '&Print...';
    PrintButton.Left := WizardForm.InfoAfterPage.Left + 96;
    PrintButton.Top := WizardForm.InfoAfterPage.Height + 88;
    PrintButton.OnClick := @PrintButtonClick;
    PrintButton.Parent := WizardForm.NextButton.Parent;
end;

function InitializeSetup(): Boolean;
var
  ErrorCode: Integer;
begin
  DetectVS();
  result := true;
  if not Vs2015Installed and not VsNextInstalled then
  begin
    if MsgBox('Visual Studio 2015 has not been detected, do you want to download the free Visual Studio Community Edition ?', mbConfirmation, MB_YESNO) = IDYES then
    begin
    ShellExec('open','https://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx','','',SW_SHOW,ewWaitUntilIdle, ErrorCode);
    result := false;
    end
  end;
  
end;

procedure AdjustPkgDef();
var pkgdeffile: String;
var section: String;
begin
  if IsComponentSelected('vs2015\vulcanprg') then
  begin
        pkgdeffile := Vs2015Path+'\Extensions\XSharp\XSharpProject2015.pkgdef'
        section    := '$RootKey$\Languages\File Extensions\';
        DeleteIniSection(section+'.prg', pkgdeffile);
        DeleteIniSection(section+'.ppo', pkgdeffile);
        DeleteIniSection(section+'.vh',  pkgdeffile);
        { There are duplicate sections in the file. We want to delete the 1st 3 sections for the extensions prg, ppo and vh}
        section := '$RootKey$\Editors\{b4829761-2bfa-44b7-8f8f-d2625ebcf218}\Extensions';
        DeleteIniSection(section,pkgdeffile);
        DeleteIniSection(section,pkgdeffile);
        DeleteIniSection(section,pkgdeffile);

  end;
end;

