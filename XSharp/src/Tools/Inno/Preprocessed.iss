; BEGIN ISPPBUILTINS.ISS


; END ISPPBUILTINS.ISS

;#define Compression     "none"





; Code Signing

;Folders



[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{32EB192A-B120-4055-800E-74B48B80DA06}
DisableWelcomePage=no
DisableStartupPrompt=yes
DisableReadyMemo=yes
DisableFinishedPage=no
InfoBeforeFile=Baggage\ReadmeShortFox.rtf
AppName=XSharp
AppVersion=0.2.11.2110
AppCopyright=Copyright © 2015-2017 XSharp B.V.
AppVerName=XSharp 0.2.11.0
AppPublisher=XSharp BV
AppPublisherURL=http://www.xsharp.info
AppSupportURL=http://www.xsharp.info
AppUpdatesURL=http://www.xsharp.info
DefaultDirName={pf}\XSharp
DefaultGroupName=XSharp
LicenseFile=Baggage\License.txt
OutputDir=\XSharp\Dev\XSharp\Binaries\Setup 
OutputBaseFilename=XSharpSetup0211Fox
OutputManifestFile=Setup-Manifest.txt
SetupIconFile=Baggage\XSharp.ico
Compression=lzma2/ultra64
SolidCompression=yes
SetupLogging=yes

; Version Info for Installer and uninstaller
VersionInfoVersion=0.2.11.2110
VersionInfoDescription=XSharp Beta 11
VersionInfoCompany=XSharp BV
VersionInfoTextVersion=0.2.10.2110 (Beta 11)
VersionInfoCopyRight=Copyright © 2015-2017 XSharp B.V.
VersionInfoProductName=XSharp
VersionInfoProductVersion=0.2.11.2110
Wizardsmallimagefile=Baggage\XSharp_Bmp_Banner.bmp 
WizardImagefile=Baggage\XSharp_Bmp_Dialog.bmp

;Uninstaller
UninstallFilesDir={app}\uninst
UninstallDisplayName=XSharp Beta 11
UninstallDisplayIcon={app}\Images\XSharp.ico;
UninstallLogMode=overwrite


TouchDate=2017-04-19
TouchTime=02:11:00


; Make sure they are admins
PrivilegesRequired=admin
; Make sure they are running on Windows 2000 Or Higher
Minversion=6.0.600

; Code Signing
Signtool=mssigntool sign /f c:\XSharp\Dev\XSharp\build\Signatures\XSharpCert.pfx /p J1O39dGG6FPLXWj  /t http://timestamp.globalsign.com/scripts/timstamp.dll  /d "XSharp, xBase compiler for .Net" $f        
SignToolRetryCount=5


[Components]
Name: "main";             Description: "The XSharp Compiler and Build System";        Types: full compact custom; Flags: fixed; 
Name: "vs2015";           Description: "Visual Studio 2015 Integration";              Types: full custom;         Check: Vs2015IsInstalled;
Name: "vs2015\help";      Description: "Install VS documentation";                    Types: full custom;         Check: HelpViewer22Found;
Name: "vs2017";           Description: "Visual Studio 2017 Integration";              Types: full custom;         Check: vs2017IsInstalled;
Name: "vs2017\help";      Description: "Install VS documentation";                    Types: full custom;         Check: vs2017IsInstalled;
Name: "xide";             Description: "Include the XIDE 1.08 installer";  Types: full custom;                  


[Dirs]
Name: "{app}\Assemblies";
Name: "{app}\Bin";
Name: "{app}\Bin\Debug";
Name: "{app}\Bin\Release";
Name: "{app}\Help";
Name: "{app}\Images";
Name: "{app}\Include";
Name: "{app}\ProjectSystem";                                
Name: "{app}\Redist"
Name: "{app}\Snippets"
Name: "{app}\Tools";
Name: "{app}\Uninst";
Name: "{app}\Xide";
Name: "{code:GetVs2015IdeDir}\Extensions\XSharp";                              Components: vs2015; 
Name: "{code:GetVs2015IdeDir}\Extensions\XSharp\\Snippets\1033";             Components: vs2015; 
Name: "{userdocs}\Visual Studio 2015\Code Snippets\XSharp\My Code Snippets";   Components: vs2015; 
Name: "{code:Getvs2017IdeDir}\Extensions\XSharp";                              Components: vs2017; 
Name: "{code:GetVs2017IdeDir}\Extensions\XSharp\\Snippets\1033";             Components: vs2017; 
Name: "{userdocs}\Visual Studio 2017\Code Snippets\XSharp\My Code Snippets";   Components: vs2017; 
; user template folders
Name: "{userdocs}\Visual Studio 2015\Templates\ProjectTemplates\XSharp";   Components: vs2015; 
Name: "{userdocs}\Visual Studio 2015\Templates\ItemTemplates\XSharp";   Components: vs2015; 
Name: "{userdocs}\Visual Studio 2017\Templates\ProjectTemplates\XSharp";   Components: vs2017; 
Name: "{userdocs}\Visual Studio 2017\Templates\ItemTemplates\XSharp";   Components: vs2017; 


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
; Main program
Source: "\Xsharp\Dev\XSharp\Binaries\Release\xsc.exe";                            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce ; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\xsc.rsp";                            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSCompiler.exe";                     DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce ; Components: main; BeforeInstall: TaskKill('xscompiler.exe');
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSharp.CodeAnalysis.dll";            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce ; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSharp.CodeAnalysis.pdb";            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\xsc.pdb";                            DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSCompiler.pdb";                     DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\xsc.exe.config";                     DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly  ; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSCompiler.exe.config ";             DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main


Source: "Baggage\DebugRelease.txt";                        DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
; Release Folder
Source: "\Xsharp\Dev\XSharp\Binaries\Release\xsc.exe";                            DestDir: "{app}\bin\Release"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce ; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSCompiler.exe";                     DestDir: "{app}\bin\Release"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSharp.CodeAnalysis.dll";            DestDir: "{app}\bin\Release"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSharp.CodeAnalysis.pdb";            DestDir: "{app}\bin\Release"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\xsc.pdb";                            DestDir: "{app}\bin\Release"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSCompiler.pdb";                     DestDir: "{app}\bin\Release"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\xsc.exe.config";                     DestDir: "{app}\bin\Release"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly  ; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSCompiler.exe.config ";             DestDir: "{app}\bin\Release"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
; Debug Folder
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\xsc.exe";                            DestDir: "{app}\bin\Debug"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\XSCompiler.exe";                     DestDir: "{app}\bin\Debug"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.dll";            DestDir: "{app}\bin\Debug"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.pdb";            DestDir: "{app}\bin\Debug"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\xsc.pdb";                            DestDir: "{app}\bin\Debug"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\XSCompiler.pdb";                     DestDir: "{app}\bin\Debug"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\xsc.exe.config";                     DestDir: "{app}\bin\Debug"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly  ; Components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\XSCompiler.exe.config ";             DestDir: "{app}\bin\Debug"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main

Source: "\Xsharp\DevPublic\Binaries\Debug\baggage\rc.exe";                    DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\DevPublic\Binaries\Debug\baggage\rcdll.dll";                 DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\DevPublic\Binaries\Debug\xporter.exe";                       DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce; Components: main
Source: "\Xsharp\DevPublic\Binaries\Debug\xporter.pdb";                       DestDir: "{app}\bin"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main


; GAC files in Bin folder
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\System.Collections.Immutable.dll";   DestDir: "{app}\bin"; StrongAssemblyName: "System.Collections.Immutable, Version=1.2.1.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete; components: main
Source: "\Xsharp\Dev\XSharp\Binaries\Debug\System.Reflection.Metadata.dll";     DestDir: "{app}\bin"; StrongAssemblyName: "System.Reflection.Metadata, Version=1.4.1.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete; components: main

; Support files
Source: "Baggage\ReadmeFox.rtf";                          DestDir: "{app}";   DestName:"Readme.rtf" ; Flags: isreadme ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "Baggage\Whatsnew.rtf";                           DestDir: "{app}";   DestName:"Whatsnew.rtf" ; Flags: isreadme ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main

Source: "Baggage\Redist.txt";                             DestDir: "{app}\Redist" ; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "Baggage\XSharp.ico";                             DestDir: "{app}\Images"; Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "Baggage\License.txt";                            DestDir: "{app}";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main

; Include Files
Source: "\Xsharp\Dev\XSharp\src\Common\*.xh";                            DestDir: "{app}\Include"; Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main

;MsBuild Files
Source: "\Xsharp\DevPublic\Binaries\Debug\Xaml\*.*";                          DestDir: "{pf}\MsBuild\XSharp\Rules";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\DevPublic\Binaries\Debug\Targets\*.*";                       DestDir: "{pf}\MsBuild\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharp.Build.dll";                  DestDir: "{pf}\MsBuild\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharp.Build.pdb";                  DestDir: "{pf}\MsBuild\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main

; MsBuild files for VS2017 in a private directory per installation
Source: "\Xsharp\DevPublic\Binaries\Debug\Xaml\*.*";                          DestDir: "{code:Getvs2017BaseDir}\MsBuild\XSharp\Rules";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Targets\*.*";                       DestDir: "{code:Getvs2017BaseDir}\MsBuild\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharp.Build.dll";                  DestDir: "{code:Getvs2017BaseDir}\MsBuild\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharp.Build.pdb";                  DestDir: "{code:Getvs2017BaseDir}\MsBuild\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017

;Documentation
Source: "\Xsharp\Dev\XSharp\Binaries\Help\XSharp.pdf";                        DestDir: "{app}\Help";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main;
Source: "\Xsharp\Dev\XSharp\Binaries\Help\XSharp.chm";                        DestDir: "{app}\Help";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main;
Source: "\Xsharp\Dev\XSharp\Binaries\Help\XSVulcan.chm";                      DestDir: "{app}\Help";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: main;
Source: "\Xsharp\Dev\XSharp\Binaries\Help\XSharp.msha";                       DestDir: "{app}\Help";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015\help or vs2017\help;     
Source: "\Xsharp\Dev\XSharp\Binaries\Help\XSharp.cab";                        DestDir: "{app}\Help";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce; Components: vs2015\help or vs2017\help;
Source: "\Xsharp\Dev\XSharp\Binaries\Help\XSVulcan.cab";                      DestDir: "{app}\Help";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly signonce; Components: vs2015\help or vs2017\help;

;XIDE
Source: "\Xsharp\Dev\XSharp\Xide\XIDE_Set_up_1.08.exe";                      DestDir: "{app}\Xide";        Flags: touch ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: Xide

;VsProjectSystem
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpProject.vsix";            DestDir: "{app}\ProjectSystem"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015 or vs2017       
; CodeAnalysis is used by project system and CodeDom provider
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSharp.CodeAnalysis.dll";           DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015 ;BeforeInstall: DeleteOldFiles(2015);
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSharp.CodeAnalysis.pdb";           DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpCodeDomProvider.dll";         DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete; StrongAssemblyName: "XSharp.CodeDom.XSharpCodeDomProvider, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31c59c566fa38f21"; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpCodeDomProvider.pdb";         DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpColorizer.dll";               DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpColorizer.pdb";               DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpModel.dll";                   DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpModel.pdb";                   DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
; ItemTemplates per folder
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\Wpf*.Zip";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\WPF";        Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
;Source: "{#BinPFolder}Itemtemplates\VO*.Zip";             DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\VO";         Flags: recursesubdirs {#StdFlags}; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\*Internal.Zip";       DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Internal";   Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\Wcf*.Zip";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\WCF";        Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\Form*.Zip";           DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\U*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\C*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\H*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\T*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\R*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Resources";     Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015

Source: "\Xsharp\DevPublic\Binaries\Debug\ProjectTemplates\*.*";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ProjectTemplates";  Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
; Snippets
Source: "\XSharp\DevPublic\VisualStudio\ProjectPackage\Snippets\*.*";                          DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\\Snippets\1033";  Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015


Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpProject.dll";                 DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpProject.dll.config";          DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpProject.pdb";                 DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpProject.pkgdef";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015; 
Source: "\Xsharp\DevPublic\Binaries\Debug\extension.vsixmanifest";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; DestName: "extension.vsixmanifest"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\Designers.pkgdef";                  DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015; 

Source: "\Xsharp\DevPublic\Binaries\Debug\XSharp.ico ";                        DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpVSIXLogo.png ";                DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015

Source: "\Xsharp\DevPublic\Binaries\Debug\SetupCheck2017.exe";                 DestDir: "{tmp}";      Flags: signonce ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly;

; private snippets


; vs2017
; CodeAnalysis is used by project system and CodeDom provider
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSharp.CodeAnalysis.dll";           DestDir: "{code:GetVs2017IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017     ;BeforeInstall: DeleteOldFiles(2017);
Source: "\Xsharp\Dev\XSharp\Binaries\Release\XSharp.CodeAnalysis.pdb";           DestDir: "{code:GetVs2017IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpCodeDomProvider.dll";         DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete; StrongAssemblyName: "XSharp.CodeDom.XSharpCodeDomProvider, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31c59c566fa38f21"; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpCodeDomProvider.pdb";         DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpColorizer.dll";               DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpColorizer.pdb";               DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpModel.dll";                   DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpModel.pdb";                   DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017

; ItemTemplates per folder
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\Wpf*.Zip";            DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\WPF";        Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
;Source: "{#BinPFolder}Itemtemplates\VO*.Zip";             DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\VO";         Flags: recursesubdirs {#StdFlags}; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\*Internal.Zip";       DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Internal";   Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\Wcf*.Zip";            DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\WCF";        Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\Form*.Zip";           DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\U*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\C*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\H*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\T*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Itemtemplates\R*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Resources";     Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017

Source: "\Xsharp\DevPublic\Binaries\Debug\ProjectTemplates\*.*";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ProjectTemplates";  Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017

; Snippets
Source: "\XSharp\DevPublic\VisualStudio\ProjectPackage\Snippets\*.*";                          DestDir: "{code:GetVs2017IdeDir}\Extensions\XSharp\\Snippets\1033";  Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017

Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpProject.dll";             DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpProject.dll.config";      DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpProject.pdb";             DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpProject.pkgdef";          DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\extension.vsixmanifest";        DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  DestName: "extension.vsixmanifest"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\Designers.pkgdef";                  DestDir: "{code:GetVs2017IdeDir}\Extensions\XSharp"; Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2015; 

Source: "\Xsharp\DevPublic\Binaries\Debug\XSharp.ico ";                             DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017
Source: "\Xsharp\DevPublic\Binaries\Debug\XSharpVSIXLogo.png ";                DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";        Flags: ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly; Components: vs2017

; Examples
Source: "\Xsharp\DevPublic\Samples\*.prg";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly;
Source: "\Xsharp\DevPublic\Samples\*.txt";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly;
Source: "\Xsharp\DevPublic\Samples\*.vh";                              DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly;
Source: "\Xsharp\DevPublic\Samples\*.sln";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly;
Source: "\Xsharp\DevPublic\Samples\*.dbf";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly;
Source: "\Xsharp\DevPublic\Samples\*.ntx";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly;
Source: "\Xsharp\DevPublic\Samples\*.xsproj";                          DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly;


; update machine.config
Source:"\Xsharp\Dev\XSharp\src\Tools\Various\RegisterProvider.exe";          DestDir: "{app}\Tools";                     Flags: signonce ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly;

[Icons]
Name: "{group}\{cm:ProgramOnTheWeb,XSharp}"; Filename: "http://www.xsharp.info";IconFilename:{app}\Images\XSharp.ico;
Name: "{group}\{cm:UninstallProgram,XSharp}"; Filename: "{uninstallexe}";  Parameters: "/Log";
Name: "{group}\XSharp XPorter";  Filename: "{app}\bin\xporter.exe";
Name: "{group}\XSharp Readme";  Filename: "{app}\Readme.rtf";
Name: "{group}\XSharp What's New";  Filename: "{app}\Whatsnew.rtf";
Name: "{group}\XSharp Documenation (CHM)"; Filename: "{app}\Help\XSharp.chm"; 
Name: "{group}\XSharp Documenation (PDF)"; Filename: "{app}\Help\XSharp.pdf"; 
Name: "{group}\XSharp Vulcan Runtime Reference (CHM)"; Filename: "{app}\Help\XSVulcan.chm"; 
Name: "{group}\{cm:UninstallProgram,XSharp}"; Filename: "{uninstallexe}"; 
Name: "{group}\XSharp Examples"; Filename: "{commondocs}\XSharp\Examples";
Name: "{app}\Examples";  Filename: "{commondocs}\XSharp\Examples";


[Registry]
Root: HKLM; Subkey: "Software\XSharpBV"; Flags: uninsdeletekeyifempty 
Root: HKLM; Subkey: "Software\XSharpBV\XSharp"; Flags: uninsdeletekey 
Root: HKLM; Subkey: "Software\XSharpBV\XSharp"; ValueName: "XSharpPath"; ValueType: string; ValueData: "{app}" ;
Root: HKLM; Subkey: "Software\XSharpBV\XSharp"; ValueName: "Help22Installed"; ValueType: string; ValueData: "yes" ;  Components: vs2015\help;
Root: HKLM; Subkey: "Software\XSharpBV\XSharp"; ValueName: "Help23Installed"; ValueType: string; ValueData: "yes" ;  Components: vs2017\help;

; set the VSHelp to Offline
Root: HKCU; Subkey: "Software\Microsoft\VisualStudio\14.0\Help"; ValueName:"UseOnlineHelp"; ValueType: dword; ValueData: 0; Components: vs2015\help; Flags: noerror;
Root: HKCU; Subkey: "Software\Microsoft\VisualStudio\15.0\Help"; ValueName:"UseOnlineHelp"; ValueType: dword; ValueData: 0; Components: vs2017\help; Flags: noerror;

; When Vulcan is Installed then update its extension registration so we can handle the priorities in our project system

Root: HKLM; Subkey: "Software\Microsoft\VisualStudio\14.0\Editors\{{e6787d5e-718e-4810-9c26-7cc920baa335}\Extensions"; ValueName: "ppo"; ValueData: 1; Components: vs2015; Check: VulcanPrgAssociated;
Root: HKLM; Subkey: "Software\Microsoft\VisualStudio\14.0\Editors\{{e6787d5e-718e-4810-9c26-7cc920baa335}\Extensions"; ValueName: "prg"; ValueData: 1; Components: vs2015; Check: VulcanPrgAssociated;
Root: HKLM; Subkey: "Software\Microsoft\VisualStudio\14.0\Editors\{{e6787d5e-718e-4810-9c26-7cc920baa335}\Extensions"; ValueName: "vh";  ValueData: 1; Components: vs2015; Check: VulcanPrgAssociated;

Root: HKCU; Subkey: "Software\Microsoft\VisualStudio\14.0_Config\Editors\{{e6787d5e-718e-4810-9c26-7cc920baa335}\Extensions"; ValueName: "ppo"; ValueData: 1; Components: vs2015; Check: VulcanPrgAssociated;
Root: HKCU; Subkey: "Software\Microsoft\VisualStudio\14.0_Config\Editors\{{e6787d5e-718e-4810-9c26-7cc920baa335}\Extensions"; ValueName: "prg"; ValueData: 1; Components: vs2015; Check: VulcanPrgAssociated;
Root: HKCU; Subkey: "Software\Microsoft\VisualStudio\14.0_Config\Editors\{{e6787d5e-718e-4810-9c26-7cc920baa335}\Extensions"; ValueName: "vh";  ValueData: 1; Components: vs2015; Check: VulcanPrgAssociated;

[Ini]
Filename: "{code:GetVs2015IdeDir}\Extensions\extensions.configurationchanged"; Section:"XSharp"; Key: "Installed"; String: "0.2.11.2110"; Flags: uninsdeletesection; Components: vs2015;
Filename: "{code:Getvs2017IdeDir}\Extensions\extensions.configurationchanged"; Section:"XSharp"; Key: "Installed"; String: "0.2.11.2110"; Flags: uninsdeletesection; Components: vs2017;

[Run]
Filename: "{app}\Tools\RegisterProvider.exe"; Flags: runhidden;

; Remove old Help contents
Filename: "{code:GetHelp22Dir}\HlpCtntMgr.exe"; Parameters: "/silent /operation uninstall /catalogname VisualStudio14 /locale en-us /vendor ""XSharp"" /productname ""X#"" /booklist ""X# Documentation"" /wait 0";   Components: vs2015\help; StatusMsg:"UnInstalling VS Help for VS2015"; Flags: waituntilidle;
Filename: "{code:GetHelp23Dir}\HlpCtntMgr.exe"; Parameters: "/silent /operation uninstall /catalogname VisualStudio15 /locale en-us /vendor ""XSharp"" /productname ""X#"" /booklist ""X# Documentation"" /wait 0";   Components: vs2017\help; StatusMsg:"UnInstalling VS Help for VS2017"; Flags: waituntilidle;


Filename: "{code:GetHelp22Dir}\HlpCtntMgr.exe"; Parameters: "/operation install /catalogname  VisualStudio14 /locale en-us /sourceuri ""{app}\help\XSharp.msha"" /wait 0";     Components: vs2015\help; StatusMsg:"Installing VS Help for VS2015"; Flags: waituntilidle;
Filename: "{code:GetHelp23Dir}\HlpCtntMgr.exe"; Parameters: "/operation install /catalogname  VisualStudio15 /locale en-us /sourceuri ""{app}\help\XSharp.msha"" /wait 0";     Components: vs2017\help; StatusMsg:"Installing VS Help for VS2017";  Flags: waituntilidle;
Filename:  "{app}\Xide\XIDE_Set_up_1.08.exe"; Description:"Run XIDE 1.08 Installer"; Flags: postInstall;  Components: XIDE;

; The following will only work if we make sure our assemblies have a strong name
;Filename: {code:GetV4NetDir}ngen.exe; Parameters: "install ""{app}\bin\xsc.exe"""; StatusMsg: Optimizing performance for your system ...; Flags: runhidden; 
;Filename: {code:GetV4NetDir}ngen.exe; Parameters: "install ""{app}\bin\XSCompiler.exe"""; StatusMsg: Optimizing performance for your system ...; Flags: runhidden; 
;Filename: {code:GetV4NetDir}ngen.exe; Parameters: "install ""{app}\bin\XSharp.CodeAnalysis.dll"""; StatusMsg: Optimizing performance for your system ...; Flags: runhidden; 

[UninstallRun]
Filename: "{code:GetHelp22Dir}\HlpCtntMgr.exe"; Parameters: "/silent /operation uninstall /catalogname VisualStudio14 /locale en-us /vendor ""XSharp"" /productname ""X#"" /booklist ""X# Documentation"" /wait 0";   Components: vs2015\help; StatusMsg:"UnInstalling VS Help for VS2015"; Flags: waituntilidle;
Filename: "{code:GetHelp23Dir}\HlpCtntMgr.exe"; Parameters: "/silent /operation uninstall /catalogname VisualStudio15 /locale en-us /vendor ""XSharp"" /productname ""X#"" /booklist ""X# Documentation"" /wait 0";   Components: vs2017\help; StatusMsg:"UnInstalling VS Help for VS2017"; Flags: waituntilidle;


[InstallDelete]
; The old License.rtf file.
; Template cache, component cache and previous installation of our project system
; vs2015
Type: files;          Name: "{app}\License.rtf"; 
Type: filesandordirs; Name: "{app}\Xide"; 
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\vtc";                            Components: vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\ComponentModelCache";            Components: vs2015
Type: filesandordirs; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp";       Components: vs2015

; vs2017
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\vtc";                            Components: vs2017
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\ComponentModelCache";            Components: vs2017
Type: filesandordirs; Name: "{code:Getvs2017IdeDir}\Extensions\XSharp";       Components: vs2017

; remove the old uninstaller because the uninstall file format has changed in one of the previous builds
Type: filesandordirs; Name: "{app}\Uninst"

[UninstallDelete]
Type: filesandordirs; Name: "{app}\Assemblies"                    ; Components: main
Type: filesandordirs; Name: "{app}\Bin\Debug"                     ; Components: main
Type: filesandordirs; Name: "{app}\Bin\Release"                   ; Components: main
Type: filesandordirs; Name: "{app}\Bin"                           ; Components: main
Type: filesandordirs; Name: "{app}\Help"                          ; Components: main
Type: filesandordirs; Name: "{app}\Images"                        ; Components: main
Type: filesandordirs; Name: "{app}\ProjectSystem"                 ; Components: main
Type: filesandordirs; Name: "{app}\Redist"                        ; Components: main
Type: filesandordirs; Name: "{app}\Tools"                         ; Components: main
Type: filesandordirs; Name: "{app}\Uninst"                        ; Components: main
Type: filesandordirs; Name: "{pf}\MsBuild\XSharp"             ; Components: main
Type: filesandordirs; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Components: vs2015;  
Type: filesandordirs; Name: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Components: vs2017;  
Type: filesandordirs; Name: "{commondocs}\XSharp\Examples";
Type: dirifempty;     Name: "{app}\Include"; 
Type: dirifempty;     Name: "{app}"; 
Type: filesandordirs; Name: "{app}\Xide"; 
Type: dirifempty;     Name: "{app}\Snippets"                      ; Components: main
Type: dirifempty;     Name: "{commondocs}\XSharp"; 

; Template cache and component cache
;vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\vtc";                            Components: vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\ComponentModelCache";            Components: vs2015
;vs2017
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\vtc";                            Components: vs2017
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\ComponentModelCache";            Components: vs2017

[Messages]
WelcomeLabel1=Welcome to XSharp (X#)
WelcomeLabel2=This installer will install XSharp Beta 11 on your computer.%n%nIt is recommended that you close all other applications before continuing, especially all running copies of Visual Studio.
WizardInfoBefore=Warning
InfoBeforeLabel=You are about to install Beta software
InfoBeforeClickLabel=Only continue the installation if you are aware of the following:


[Code]
Program setup;
var

  vs2017VersionPage: TInputOptionWizardPage;
  PrintButton: TButton;
  Vs2015Path : String;
  Vs2015Installed: Boolean;
  Vs2015BaseDir: String;
  VulcanInstalled: Boolean;
  VulcanBaseDir: String;
  CancelSetup : Boolean;
  
  vs2017Path : String;
  vs2017Version: String;
  vs2017Installed: Boolean;
  vs2017BaseDir: String;

  vs2017Count: Integer;
  vs2017BaseDir1: String;
  vs2017BaseDir2: String;
  vs2017BaseDir3: String;

  vs2017Version1: String;
  vs2017Version2: String;
  vs2017Version3: String;
  
  VulcanPrgAssociation: Boolean;
  VulcanGuid : String;
  HelpViewer22Installed : Boolean;
  HelpViewer22Dir : String;
  OurHelp22Installed: Boolean;
  HelpViewer23Installed : Boolean;
  HelpViewer23Dir : String;
  OurHelp23Installed: Boolean;
  OriginalTypeChange: TNotifyEvent;

procedure PrintButtonClick(Sender: TObject);
var ResultCode :integer;
begin
ExtractTemporaryFile('license.txt');
if not ShellExec('Print', ExpandConstant('{tmp}\license.txt'),
     '', '', SW_SHOW, ewNoWait, ResultCode) then
end;
{
VS2017.txt will contain:
InstanceId: 8aa9303a (Complete)
InstallationVersion: 15.0.25914.0 (4222126348959744)
InstallationPath: C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional
Product: Microsoft.VisualStudio.Product.Professional
Workloads:
    Microsoft.VisualStudio.Workload.CoreEditor
    Microsoft.VisualStudio.Workload.ManagedDesktop
}



procedure TaskKill(FileName: String);
var
  ResultCode: Integer;
begin
    Log('Kill running task (when necessary):'+FileName);
    Exec(ExpandConstant('taskkill.exe'), '/f /im ' + '"' + FileName + '"', '', SW_HIDE,
     ewWaitUntilTerminated, ResultCode);
end;

procedure FindVS2017;
var FileContents : TArrayOfString;
  ResultCode: Integer;
  i: integer;
  line: String;
  tokenLength: Integer;
  token: String;
begin
  Log('Detect 2017 start');
  ExtractTemporaryFile('SetupCheck2017.exe');
  if Exec(ExpandConstant('{tmp}\SetupCheck2017.exe'),'','',SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
      if ResultCode = 0 then
      begin
        if LoadStringsFromFile(ExpandConstant('{tmp}\vs2017.txt'), FileContents) then
        begin
            Log('========================================');
            Log('Installed versions of Visual Studio 2017');
            Log('========================================');
            for i:= 0 to GetArrayLength(FileContents)-1 do
            begin
               line := Trim( FileContents[i]);
               Log(line);
               token := 'InstanceId:';
               if Pos(token, line) > 0  then 
                  vs2017Count := vs2017Count + 1;
               token := 'InstallationPath:';
               if Pos(token, line) > 0  then 
                begin
                  tokenLength := Length(token);
                  vs2017BaseDir := Trim(Copy(line, tokenLength+1, Length(line) - tokenLength));
                  if vs2017Count = 1 then
                     vs2017BaseDir1 := vs2017BaseDir;
                  if vs2017Count = 2 then
                     vs2017BaseDir2 := vs2017BaseDir;
                  if vs2017Count = 3 then
                     vs2017BaseDir3 := vs2017BaseDir;
                  vs2017Installed := true;
                end
               token := 'Product:';
               if Pos(token, line) > 0 then
                begin
                  tokenLength := Length(token);
                  vs2017Version := ExtractFileExt(line);
                  vs2017Version := Copy(vs2017Version,2,Length(vs2017Version)-1);
                  if vs2017Count = 1 then
                     vs2017Version1 := vs2017Version;
                  if vs2017Count = 2 then
                     vs2017Version2 := vs2017Version;
                  if vs2017Count = 3 then
                     vs2017Version3 := vs2017Version;
                  vs2017Installed := true;
                end
            end
        end
      end

      Log('Vs 2017 # of installations: '+IntToStr(vs2017Count));
      if vs2017Count > 0 then
      begin
        Log('Vs 2017 installation dir 1: '+vs2017BaseDir1);
        Log('Vs 2017 product version 1 : '+vs2017Version1);
      end
      if vs2017Count > 1 then
      begin
        Log('Vs 2017 installation dir 2: '+vs2017BaseDir2);
        Log('Vs 2017 product version 2 : '+vs2017Version2);
      end
      if vs2017Count > 2 then
      begin
        Log('Vs 2017 installation dir 3: '+vs2017BaseDir3);
        Log('Vs 2017 product version  3: '+vs2017Version3);
      end
  end
  Log('Detect 2017 end');
end;


procedure DetectVS();
var temp : String;
begin
  Log('Start VS Detection');
  VulcanInstalled := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Grafx\Vulcan.NET','InstallPath',VulcanBaseDir) ;
  Vs2015Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\VisualStudio\SxS\VS7','14.0',Vs2015BaseDir) ;
  if Vs2015Installed then Vs2015Path := Vs2015BaseDir+'Common7\Ide\';
  FindVS2017;

  HelpViewer22Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\Help\v2.2','AppRoot',HelpViewer22Dir) ;
  HelpViewer23Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\Help\v2.3','AppRoot',HelpViewer23Dir) ;
{
  if Vs2017Installed and Not HelpViewer23Installed then
  begin
    if MsgBox('Visual Studio 2017 has been detected, but not the VS 2017 help viewer. To install the VS 2017 help you need to install the HelpViewer component. Exit installation?', mbConfirmation, MB_YESNO) = IDYES then
    begin
    CancelSetup := true;
    end
  end
 }
  VulcanPrgAssociation := false;
  if Vs2015Installed then
  begin
  VulcanPrgAssociation := RegQueryStringValue(HKEY_LOCAL_MACHINE, 'Software\Microsoft\VisualStudio\14.0\Languages\File Extensions\.prg', '', VulcanGuid) ;
  if VulcanPrgAssociation then 
      begin
      VulcanPrgAssociation := (UpperCase(VulcanGuid) = '{8D3F6D25-C81C-4FD8-9599-2F72B5D4B0C9}');
      end
  end
  OurHelp22Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'Software\XSharpBV\XSharp','Help22Installed',temp) ;
  OurHelp23Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'Software\XSharpBV\XSharp','Help23Installed',temp) ;
  Log('VS2015: '+Vs2015BaseDir); 
  Log('VS2017: '+Vs2017BaseDir);
 

  Log('End VS Detection');
end;


function VulcanIsInstalled: Boolean;
begin
  result := VulcanInstalled;
end;

function GetVulcanDir(Param: String): String;
begin
  result := VulcanBaseDir;
end;

function VulcanPrgAssociated: Boolean;
begin
  result := Vs2015Installed and VulcanPrgAssociation;
end;

function HelpViewer22Found: Boolean;
begin
  result := HelpViewer22Installed;
end;

function HelpViewer23Found: Boolean;
begin
  result := HelpViewer23Installed;

end;


function Vs2015IsInstalled: Boolean;
begin
  result := Vs2015Installed;
end;

function vs2017IsInstalled: Boolean;
begin
  result := vs2017Installed;
end;

function GetVs2015IdeDir(Param: String): String;
begin
  result := Vs2015Path;
end;

function Getvs2017IdeDir(Param: String): String;
begin
  result := vs2017Path;
end;

function Getvs2017Version(Param: String): String;
begin
  case vs2017VersionPage.SelectedValueIndex of
    0: result := Vs2017Version1;
    1: result := Vs2017Version2;
    2: result := Vs2017Version3;
  end;
end;


function Getvs2017BaseDir(Param: String): String;
begin
  case vs2017VersionPage.SelectedValueIndex of
    0: result := Vs2017BaseDir1;
    1: result := Vs2017BaseDir2;
    2: result := Vs2017BaseDir3;
  end;
end;

function GetHelp22Dir(Param: String): String;
begin
  result := HelpViewer22Dir;
end;

function GetHelp23Dir(Param: String): String;
begin
  result := HelpViewer23Dir;
end;

Procedure Checkvs2017Help();
var
  VsHelpItem: Integer;

begin
  if vs2015Installed and Vs2017Installed then
     vsHelpItem := 4
  else
     vsHelpItem := 2;
   
  if Vs2017Installed and Not HelpViewer23Installed then
  begin
     WizardForm.ComponentsList.ItemCaption[VsHelpItem] := 'Cannot install the VS 2017 documentation because the Help Viewer component is not installed';
     WizardForm.ComponentsList.ItemEnabled[VsHelpItem] := false;
     WizardForm.ComponentsList.Checked[VsHelpItem] := false;
  end

end;


procedure DeleteOldFiles(folder: integer);
var
  FindRec: TFindRec;
  FolderName: String;
  Ok : Boolean;
  i: Integer;
begin
  if folder = 2015 then 
  begin
      FolderName := GetVs2015IdeDir('');
      Ok := true;
  end
  else if folder = 2017 then 
  begin
     FolderName := GetVs2017IdeDir('');
     Ok := true;
  end
  if Ok then begin
    for i := 1 to 2 do
    begin
      if i = 2 then FolderName := FolderName+'PrivateAssemblies\';
      Log('Delete old files in folder '+FolderName);
      if FindFirst(FolderName+'XSharp*.dll', FindRec) then begin
        try
          repeat
            Log('Delete old file '+FindRec.Name);
            DeleteFile(FolderName+FindRec.Name)
          until not FindNext(FindRec);
        finally
          FindClose(FindRec);
        end;
      end;
      if FindFirst(FolderName+'XSharp*.pdb', FindRec) then begin
        try
          repeat
            Log('Delete old file '+FindRec.Name);
            DeleteFile(FolderName+FindRec.Name)
          until not FindNext(FindRec);
        finally
          FindClose(FindRec);
        end;
      end;
    end;
  end;
end;



Procedure CurPageChanged(CurPage: Integer);

begin
  PrintButton.Visible := CurPage = wpLicense;
  if vs2017Installed then 
  begin
    Vs2017BaseDir := Getvs2017BaseDir('');
    Vs2017Version := Getvs2017Version('');
    vs2017VersionPage.CheckListBox.ItemCaption[0] := Vs2017Version1+' in ' + Vs2017BaseDir1;
    vs2017VersionPage.CheckListBox.ItemCaption[1] := Vs2017Version2+' in ' + Vs2017BaseDir2;
    vs2017Path    := vs2017BaseDir+'\Common7\Ide\';
  end
  if vs2017Count > 2 then
    begin
    vs2017VersionPage.CheckListBox.ItemCaption[2] := Vs2017Version3+' in ' + Vs2017BaseDir3;
    vs2017VersionPage.CheckListBox.ItemEnabled[2] := true;
    end
  else
    begin
    vs2017VersionPage.CheckListBox.ItemCaption[2] := '';
    vs2017VersionPage.CheckListBox.ItemEnabled[2] := false;
    end
  Checkvs2017Help;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
var
  Version: String;
begin
  { Store the settings so we can restore them next time }
  case vs2017VersionPage.SelectedValueIndex of
    0: Version := '1';
    1: Version:= '2';
    2: Version := '3';
  end;
  SetPreviousData(PreviousDataKey, 'VS2017Version', Version);
end;

procedure TypeChange (Sender: TObject);
begin
OriginalTypeChange(Sender);
Checkvs2017Help;
end;

procedure InitializeWizard();
begin
    
    Log('InitializeWizard start');
    vs2017VersionPage := CreateInputOptionPage(wpSelectComponents,
                'Visual Studio 2017', 
                'Please select in which Visual Studio 2017 instance the program must be installed',
                'Select the Visual Studio 2017 Instance',True, False);
    vs2017VersionPage.Add('Option 1');
    vs2017VersionPage.Add('Option 2');
    vs2017VersionPage.Add('Option 3');
    vs2017VersionPage.Values[0] := true;
    PrintButton := TButton.Create(WizardForm);
    PrintButton.Caption := '&Print...';
    PrintButton.Left := WizardForm.InfoAfterPage.Left + 96;
    PrintButton.Top := WizardForm.InfoAfterPage.Height + 88;
    PrintButton.OnClick := @PrintButtonClick;
    PrintButton.Parent := WizardForm.NextButton.Parent;
  case GetPreviousData('VS2017Version', '') of
    '1': vs2017VersionPage.SelectedValueIndex := 0;
    '2': vs2017VersionPage.SelectedValueIndex := 1;
    '3': vs2017VersionPage.SelectedValueIndex := 2;
  else
    vs2017VersionPage.SelectedValueIndex := 0;
  end;
  OriginalTypeChange := WizardForm.TypesCombo.OnChange ;
  WizardForm.TypesCombo.OnChange := @TypeChange;
  Log('InitializeWizard end');
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  {Log('ShouldSkipPage start');}
  { Skip pages that shouldn't be shown }
  Result := False;
    if (PageId = vs2017VersionPage.ID)   then 
    begin
      if (vs2017Count < 2 ) then
        Result := True
      if not IsComponentSelected('vs2017') then
        Result := True
    end
  {Log('ShouldSkipPage stop');}
end;

function InitializeSetup(): Boolean;
var
  ErrorCode: Integer;
begin
  CancelSetup := false;
  Log('InitializeSetup start');
  DetectVS();
  result := not CancelSetup ;
  if result and not Vs2015Installed and not vs2017Installed then
  begin
    if MsgBox('Visual Studio 2015 has not been detected, do you want to download the free Visual Studio Community Edition ?', mbConfirmation, MB_YESNO) = IDYES then
    begin
    ShellExec('open','https://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx','','',SW_SHOW,ewWaitUntilIdle, ErrorCode);
    result := false;
    end
  end;
 
  { When more than 1 VS 2017 installation, ask for the instance}
  Log('InitializeSetup end');  
end;

function GetV4NetDir(version: string) : string;

var regkey, regval  : string;

begin

    regkey := 'SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full'

    RegQueryStringValue(HKLM, regkey, 'InstallPath', regval);

    result := regval;
end; 

{ Code to run when uninstalling }
function InitializeUninstall(): Boolean;
begin
  TaskKill('xscompiler.exe');
  result := true;
end;


