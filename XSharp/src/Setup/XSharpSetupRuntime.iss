;
; Note that all folder name variables include the trailing backslash !
;

; mssigntool = "c:\Program Files (x86)\Windows Kits\10\bin\10.0.17134.0\x86\signtool.exe"   $p



#ifndef Compression
#define Compression     "lzma/ultra64"
; "lzma/ultra"
;#define Compression     "none" 
#endif

#define FOX
;#undef FOX

; version info and similar stuff.
  
#define AssemblyVersion     "2.0.2.0"
#define FileVersion         "2.0.2.3"
#define FileNameVersion     "2RC2c"
#define VIVersion           "2.0.2.3"
#define TouchDate           "2019-07-07"
#define TouchTime           "02:20:30"

#define DevFolder           "C:\Xsharp\Dev\XSharp"
#define DevPublicFolder     "C:\Xsharp\DevPublic"
#define SetupFolder         DevFolder + "\src\Setup"

#define BinRtFolder         "C:\Xsharp\DevRt\Binaries\Release\"
#define BinRtDFolder        "C:\Xsharp\DevRt\Binaries\Debug\"
#define BinRtHelpFolder     "C:\Xsharp\DevRt\Binaries\Help\"
#define BinRtVoHelpFolder   "C:\Xsharp\DevRt\Binaries\VoHelp\"
#define BinDFolder          DevFolder + "\Binaries\Debug\net46\"
#define BinRFolder          DevFolder + "\Binaries\Release\net46\"
#define BinDllDFolder       DevFolder + "\Binaries\Debug\Dlls\"
#define BinDllRFolder       DevFolder + "\Binaries\Release\Dlls\"

#ifdef FOX
#define Suffix              "Fox"
#define BinFolder           BinRFolder
#define BinDllFolder        BinDllRFolder
#else   
#define Suffix              "Public"
#define BinFolder           BinDFolder
#define BinDllFolder        BinDllDFolder
#endif

#define SetupExeName        "XSharpSetupRt"+FileNameVersion+Suffix

#define Product             "XSharp Runtime"
#define ProdBuild           "XSharp Runtime version "+ FileVersion
#define Company             "XSharp BV"
#define XSharpURL           "http://www.xsharp.info"
#define CopyRight           "Copyright © 2015-2019 XSharp B.V."

; Code Signing
#define KeyFile             DevFolder + "\build\Signatures\XSharpCert.pfx"
#define TimeStampURL        "http://timestamp.globalsign.com/scripts/timstamp.dll"
#define KeyPassword         "J1O39dGG6FPLXWj"
#define Description         "XSharp (X#), xBase compiler for .Net"


;Source Folders and other related stuff

#define CommonFolder        DevFolder + "\src\Common\"
#define OutPutFolder        DevFolder + "\Binaries\Setup"

#define StdFlags            "ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly"
#define GACInstall          "gacinstall uninsnosharedfileprompt uninsrestartdelete"
#define XSharpVersion       ", Version="+AssemblyVersion+", Culture=neutral, PublicKeyToken=ed555a0467764586, processorArchitecture=MSIL" 
#define VOSDKVersion        ", Version="+AssemblyVersion+", Culture=neutral, PublicKeyToken=a967d8055360a7b9, processorArchitecture=x86" 
#define VOSDKVersionAnyCPU  ", Version="+AssemblyVersion+", Culture=neutral, PublicKeyToken=a967d8055360a7b9, processorArchitecture=MSIL" 


[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{477B7845-48AF-4ACC-BAC6-90003B1EE562}
DisableWelcomePage=no
DisableStartupPrompt=yes
DisableReadyMemo=no
DisableFinishedPage=no
DisableDirPage=yes

InfoBeforeFile=Baggage\rtwhatsnew.rtf
AppName={#Product}
AppVersion={#FileVersion}
AppCopyright={# CopyRight}
AppVerName={#Product} {#FileVersion}
AppPublisher={#Company}
AppPublisherURL={#XSharpURL}
AppSupportURL={#XSharpURL}
AppUpdatesURL={#XSharpURL}
DefaultDirName={commonpf}\XSharp
DefaultGroupName={#Product}
AlwaysShowDirOnReadyPage=true
LicenseFile=Baggage\License.txt
OutputDir={#OutPutFolder} 
OutputBaseFilename={#SetupExeName}
OutputManifestFile=Setup-ManifestRt.txt
SetupIconFile=Baggage\XSharp.ico
Compression={#Compression}
SolidCompression=yes
LZMAUseSeparateProcess=yes
SetupLogging=yes
WindowResizable=yes

; Version Info for Installer and uninstaller
VersionInfoVersion={#= VIVersion}
VersionInfoDescription={# ProdBuild}
VersionInfoCompany={# Company}
VersionInfoTextVersion={#= VIVersion}
VersionInfoCopyRight={# CopyRight}
VersionInfoProductName={# Product}
VersionInfoProductVersion={# VIVersion}
Wizardsmallimagefile=Baggage\XSharp_Bmp_Banner.bmp 
;WizardImagefile=Baggage\XSharp_Bmp_DialogXMas.bmp
WizardImagefile=Baggage\XSharp_Bmp_Dialog.bmp
;WizardImagefile=Baggage\XSharp_Snowman.bmp
WizardStyle=modern
WizardResizable=true

;Uninstaller
UninstallFilesDir={app}\uninst
UninstallDisplayName={#=ProdBuild}
UninstallDisplayIcon={app}\Images\XSharp.ico
UninstallLogMode=append


TouchDate={#=TouchDate}
TouchTime={#=TouchTime}                               


; Make sure they are admins
PrivilegesRequired=admin
UsedUserAreasWarning=false
; Make sure they are running on Windows 2000 Or Higher
Minversion=6.0.600

; Code Signing
Signtool=mssigntool sign /f {# KeyFile} /p {# Keypassword}  /t {# TimeStampURL}  /d "{# Description}" $f        
SignToolRetryCount=5
; Tell windows that we associte the prgx extension
ChangesAssociations=yes
ChangesEnvironment=yes


[Components]
Name: "main";             Description: "The XSharp Runtime";                                Types: full compact custom; Flags: fixed checkablealone disablenouninstallwarning; 
Name: "main\gac";         Description: "Register runtime DLLs in the GAC (recommended !)";  Types: full custom;         Flags: disablenouninstallwarning; 



;[Tasks]
;Name: envPath; Description: "Add XSharp to your PATH variable" 
;Name: desktopicon; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; Components: main
;Name: desktopicon\common; Description: "For all users"; GroupDescription: "Additional icons:"; Components: main; Flags: exclusive
;Name: desktopicon\user; Description: "For the current user only"; GroupDescription: "Additional icons:"; Components: main; Flags: exclusive unchecked
;Name: quicklaunchicon; Description: "Create a &Quick Launch icon"; GroupDescription: "Additional icons:"; Components: main; Flags: unchecked
;Name: associate; Description: "&Associate files"; GroupDescription: "Other tasks:"; Flags: unchecked

[Dirs]
Name: "{app}\Assemblies";
Name: "{app}\Redist"
Name: "{app}\Snippets"


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
; Main program
; Text files, independent of Debug/Release and independent of Fox

; Include Files
Source: "{#CommonFolder}*.xh";          DestDir: "{app}\Include"; Flags: touch uninsneveruninstall {#StdFlags}; 
Source: "baggage\rtwhatsnew.rtf";				DestDir: "{app}"; Flags: touch {#StdFlags}; 

; Runtime in the GAC
; must list each file individually because of the strong name
Components: main\gac;  Source: "{#BinRtFolder}XSharp.Core.dll";                    DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "XSharp.Core{#XSharpVersion}" 
Components: main\gac;  Source: "{#BinRtFolder}XSharp.Rdd.dll";                     DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "XSharp.Rdd{#XSharpVersion}" 
Components: main\gac;  Source: "{#BinRtFolder}XSharp.VO.dll";                      DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "XSharp.VO{#XSharpVersion}" 
Components: main\gac;  Source: "{#BinRtFolder}XSharp.RT.dll";                      DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "XSharp.RT{#XSharpVersion}" 
Components: main\gac;  Source: "{#BinRtFolder}XSharp.XPP.dll";                     DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "XSharp.XPP{#XSharpVersion}" 
Components: main\gac;  Source: "{#BinDllFolder}XSharp.MacroCompiler.dll";          DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "XSharp.MacroCompiler{#XSharpVersion}" 
Components: main\gac;  Source: "{#BinDllFolder}XSharp.MacroCompiler.Full.dll";     DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;  
Components: main\gac;  Source: "{#BinFolder}XSharp.CodeAnalysis.dll";              DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;  
Components: main\gac;  Source: "{#BinFolder}XSharp.Scripting.dll";                 DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;

Components: main\gac;  Source: "{#BinRtFolder}VOWin32APILibrary.dll";              DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "VOWin32APILibrary{#VOSDKVersion}" 
Components: main\gac;  Source: "{#BinRtFolder}VOSystemClasses.dll";                DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "VOSystemClasses{#VOSDKVersionAnyCPU}" 
Components: main\gac;  Source: "{#BinRtFolder}VORDDClasses.dll";                   DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "VORDDClasses{#VOSDKVersionAnyCPU}" 
Components: main\gac;  Source: "{#BinRtFolder}VOSQLClasses.dll";                   DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "VOSQLClasses{#VOSDKVersion}" 
Components: main\gac;  Source: "{#BinRtFolder}VOGUIClasses.dll";                   DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "VOGUIClasses{#VOSDKVersion}" 
Components: main\gac;  Source: "{#BinRtFolder}VOReportClasses.dll";                DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "VOReportClasses{#VOSDKVersion}" 
Components: main\gac;  Source: "{#BinRtFolder}VOConsoleClasses.dll";               DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "VOConsoleClasses{#VOSDKVersion}" 
Components: main\gac;  Source: "{#BinRtFolder}VOInternetClasses.dll";              DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce {#GACInstall};  StrongAssemblyName: "VOInternetClasses{#VOSDKVersion}" 


; Runtime
; can use wildcards because no gac
Components: not main\gac; Source: "{#BinRtFolder}XSharp.????.dll";                 DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;  
Components: not main\gac; Source: "{#BinRtFolder}XSharp.???.dll";                  DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;  
Components: not main\gac; Source: "{#BinRtFolder}XSharp.??.dll";                   DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;  
Components: not main\gac; Source: "{#BinFolder}XSharp.CodeA*.dll";                 DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;  
Components: not main\gac; Source: "{#BinFolder}XSharp.Scrip*.dll";                 DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;  
Components: not main\gac; Source: "{#BinDllFolder}XSharp.Macro*.dll";              DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;  
; VO SDK libraries
Components: not main\gac;  Source: "{#BinRtFolder}VOWin32*.dll";                   DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;
Components: not main\gac;  Source: "{#BinRtFolder}VO*Classes.dll";                 DestDir: "{app}\Redist"; Flags: {#StdFlags} signonce ;

; PDB files never in the gac
Source: "{#BinRtFolder}XSharp.????.pdb";                                           DestDir: "{app}\Redist"; Flags: {#StdFlags} ;
Source: "{#BinRtFolder}XSharp.???.pdb";                                            DestDir: "{app}\Redist"; Flags: {#StdFlags} ;
Source: "{#BinRtFolder}XSharp.??.pdb";                                             DestDir: "{app}\Redist"; Flags: {#StdFlags} ;
Source: "{#BinRtFolder}VOWin32*.pdb";                                              DestDir: "{app}\Redist"; Flags: {#StdFlags} 
Source: "{#BinRtFolder}VO*Classes.pdb";                                            DestDir: "{app}\Redist"; Flags: {#StdFlags} 

; Macro compiler
Source: "{#BinFolder}XSharp.CodeA*.pdb";                                           DestDir: "{app}\Redist"; Flags: {#StdFlags} ;
Source: "{#BinFolder}XSharp.Scrip*.pdb";                                           DestDir: "{app}\Redist"; Flags: {#StdFlags} ;
Source: "{#BinDllFolder}XSharp.Macro*.pdb";                                        DestDir: "{app}\Redist"; Flags: {#StdFlags} ;

;Debug versions
#ifdef FOX
Source: "{#BinRtDFolder}XSharp.??.dll";                      DestDir: "{app}\Debug"; Flags: {#StdFlags} signonce ;
Source: "{#BinRtDFolder}XSharp.???.dll";                     DestDir: "{app}\Debug"; Flags: {#StdFlags} signonce ;
Source: "{#BinRtDFolder}XSharp.????.dll";                    DestDir: "{app}\Debug"; Flags: {#StdFlags} signonce ;
Source: "{#BinDllDFolder}XSharp.CodeA*.dll";                 DestDir: "{app}\Debug"; Flags: {#StdFlags} signonce ;
Source: "{#BinDllDFolder}XSharp.Scrip*.dll";                 DestDir: "{app}\Debug"; Flags: {#StdFlags} signonce ;
Source: "{#BinDllDFolder}XSharp.Macro*.dll";                 DestDir: "{app}\Debug"; Flags: {#StdFlags} signonce ;

Source: "{#BinRtDFolder}XSharp.??.pdb";                      DestDir: "{app}\Debug"; Flags: {#StdFlags} ;
Source: "{#BinRtDFolder}XSharp.???.pdb";                     DestDir: "{app}\Debug"; Flags: {#StdFlags} ;
Source: "{#BinRtDFolder}XSharp.????.pdb";                    DestDir: "{app}\Debug"; Flags: {#StdFlags} ;
Source: "{#BinDllDFolder}XSharp.CodeA*.pdb";                 DestDir: "{app}\Debug"; Flags: {#StdFlags} ; 
Source: "{#BinDllDFolder}XSharp.Scrip*.pdb";                 DestDir: "{app}\Debug"; Flags: {#StdFlags} ; 
Source: "{#BinDllDFolder}XSharp.Macro*.pdb";                 DestDir: "{app}\Debug"; Flags: {#StdFlags} ; 

Source: "{#BinRtDFolder}VOWin32*.dll";                       DestDir: "{app}\Debug"; Flags: {#StdFlags} signonce ;
Source: "{#BinRtDFolder}VO*Classes.dll";                     DestDir: "{app}\Debug"; Flags: {#StdFlags} signonce ;
Source: "{#BinRtDFolder}VOWin32*.pdb";                       DestDir: "{app}\Debug"; Flags: {#StdFlags} ;
Source: "{#BinRtDFolder}VO*Classes.pdb";                     DestDir: "{app}\Debug"; Flags: {#StdFlags} ;
#endif


Source: "{#BinRtHelpFolder}XSharp*.xml";                     DestDir: "{app}\Redist"; Flags: {#StdFlags}

; Assemblies for Add References Dialog
Source: "{#BinRtFolder}XSharp.????.dll";                    DestDir: "{app}\Assemblies"; Flags: signonce {#StdFlags} 
Source: "{#BinRtFolder}XSharp.???.dll";                     DestDir: "{app}\Assemblies"; Flags: signonce {#StdFlags} 
Source: "{#BinRtFolder}XSharp.??.dll";                      DestDir: "{app}\Assemblies"; Flags: signonce {#StdFlags} 
; VO SDK 

Source: "{#BinRtFolder}VOWin32*.dll";                       DestDir: "{app}\Assemblies"; Flags: {#StdFlags} signonce ;
Source: "{#BinRtFolder}VO*Classes.dll";                     DestDir: "{app}\Assemblies"; Flags: {#StdFlags} signonce ;
Source: "{#BinRtHelpFolder}XSharp*.xml";                    DestDir: "{app}\Assemblies"; Flags: {#StdFlags}
Source: "{#BinRtVoHelpFolder}VO*.xml";                      DestDir: "{app}\Assemblies"; Flags: {#StdFlags}




[InstallDelete]
; The old License.rtf file.
; Template cache, component cache and previous installation of our project system vs2015
; Also the CodeDom provider from the GAC

#ifdef FOX
Type: filesandordirs; Name: "{app}\Debug"; 
#endif
Type: filesandordirs; Name: "{app}\Assemblies"; 
Type: filesandordirs; Name: "{app}\Redist";

; Old VOSDK files, better safe than sorry
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VoInternetclasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VoConsoleClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VOGUIClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VORDDClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VOSQLClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VOReportClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VOSystemClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VOWin32APILibrary";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VoInternetclasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VoConsoleClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VOGUIClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VORDDClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VOSQLClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VOReportClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VOSystemClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VOWin32APILibrary";


[UninstallDelete]
Type: filesandordirs; Name: "{app}\Assemblies"                    ; 
#ifdef FOX
Type: filesandordirs; Name: "{app}\Debug"; 
#endif
Type: filesandordirs; Name: "{app}\Redist"                        ; 


; VOSDK files, better safe than sorry
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VoInternetclasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VoConsoleClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VOGUIClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VOSQLClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VOReportClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_32\VOWin32APILibrary";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VORDDClasses";
Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\VOSystemClasses";


[Messages]
WelcomeLabel1=Welcome to {# Product} (X#) 
WelcomeLabel2=This installer will install {#ProdBuild} on your computer.%n%nIt is recommended that you close all other applications before continuing, especially all running copies of Visual Studio.



[Code]
Program setup;
var
  
  PrintButton: TButton;
  XSharpInstalled: Boolean;
  XSharpDir: String;
/////////////////////////////////////////////////////////////////////
procedure PrintButtonClick(Sender: TObject);
var ResultCode :integer;
begin
ExtractTemporaryFile('license.txt');
if not ShellExec('Print', ExpandConstant('{tmp}\license.txt'),
     '', '', SW_SHOW, ewNoWait, ResultCode) then
//if not ShellExec('', ExpandConstant('{tmp}\license.txt'),
//     '', '', SW_SHOW, ewNoWait, ResultCode) then
end;
procedure DetectXSharp();
begin
  XSharpInstalled := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\XSharpBV\XSharp','XSharpPath',XSharpDir) ;
  IF XSharpInstalled  then begin
    XSharpInstalled := FileExists(XSharpDir+'\bin\xsc.exe');
  end
 
end;

/////////////////////////////////////////////////////////////////////
Procedure CurPageChanged(CurPage: Integer);
begin
  PrintButton.Visible := CurPage = wpLicense;
end;
procedure InitializeWizard();
begin
    
    Log('InitializeWizard start');
    { Kill running process to help success the installation}
    PrintButton := TButton.Create(WizardForm);
    PrintButton.Caption := '&Print...';
    { PrintButton is placed on the License Page, bottom right of the license Memo. Width and Height are copied from BackButton}
    PrintButton.Parent := WizardForm.LicenseAcceptedRadio.Parent
    PrintButton.Top := WizardForm.LicenseAcceptedRadio.Top;
    PrintButton.Width := WizardForm.BackButton.Width;
    PrintButton.Height := WizardForm.BackButton.Height;
    PrintButton.Left := WizardForm.LicenseMemo.Left+WizardForm.LicenseMemo.Width - PrintButton.Width;
    PrintButton.OnClick := @PrintButtonClick;
    PrintButton.Anchors := [akRight, akBottom]
    WizardForm.LicenseMemo.Font.Name := 'Courier New';
    WizardForm.LicenseMemo.Font.Size := 9;
    Log('InitializeWizard end');
end;


function InitializeSetup(): Boolean;
begin
  Log('InitializeSetup start');
  DetectXSharp();
  result := true;
  if not XSharpInstalled then
  begin
    MsgBox('X# has not been detected, You need to run the normal X# installer first. This installer will now abort.', mbConfirmation, MB_OK) 
    result := false;
  end;
  Log('InitializeSetup end');  
end;

#expr SaveToFile(AddBackslash(SourcePath) + "Preprocessed.iss")


