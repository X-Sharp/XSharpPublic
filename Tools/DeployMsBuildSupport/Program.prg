USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.IO
USING System.Security.Principal

FUNCTION Start(args as string[]) AS VOID STRICT
    local aFolders as List<String>
    local cPf as string
    local XSharpDir as string
    local uninstall := false as logic
    IF args?:Length > 0
        uninstall := args:Count( { sArg => sArg:ToUpper():Contains("UNINSTALL") }) > 0
    ENDIF
    if ! IsAdministrator()
        Console.WriteLine("This program must be run as administrator")
        return
    endif
    var xsfile := "XSharp.BeforeCommon.Props"
    cPf := System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFiles)
    GetSettingString("XSharpPath", out xsharpdir,"")
    if String.IsNullOrEmpty(xSharpDir)
        Console.WriteLine("Could not locate XSharp Installation")
        return
    endif
    xsharpDir := Path.Combine(xsharpdir, "MsBuild")+"\"
    var sourceFile := Path.Combine(xsharpDir, xsfile)
    if ! System.IO.File.Exists(sourcefile)
        Console.WriteLine("Cannot find the file "+sourcefile)
        return
    endif
    if uninstall
        Console.WriteLine("Removing X# MSBuild support from  VS folders and .Net folders")
    else
        Console.WriteLine("Deploying X# MSBuild support")
        Console.WriteLine("Source file: "+sourcefile )
    endif
    aFolders := List<String>{}
    if uninstall
        var dirs := Directory.GetDirectories(cPF+"\Microsoft Visual Studio")

        foreach var dir in dirs
            var subdirs := Directory.GetDirectories(dir)
            foreach var subdir in subdirs
                if subdir:Contains("\20")
                    aFolders:Add(subdir+"\MsBuild\Current")
                endif
            next
        next
        cPf := System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)
        dirs := Directory.GetDirectories(cPF+"\Microsoft Visual Studio")
        foreach var dir in dirs
            var subdirs := Directory.GetDirectories(dir)
            foreach var subdir in subdirs
                if subdir:Contains("\20")
                    aFolders:Add(subdir+"\MsBuild\Current")
                endif
            next
        next
        var dotNetDir := GetDotNetDir(TRUE)
        if ! String.IsNullOrEmpty(dotNetDir)
            dotNetDir := Path.Combine(dotNetDir, "sdk")
            if Directory.Exists(dotNetDir)
                dirs := Directory.GetDirectories(dotnetdir)
                foreach var dir in dirs
                    aFolders:Add(dir+"\Current")
                next
            endif
        endif
        dotNetDir := GetDotNetDir(FALSE)
        if ! String.IsNullOrEmpty(dotNetDir)
            dotNetDir := Path.Combine(dotNetDir, "sdk")
            if Directory.Exists(dotNetDir)
                dirs := Directory.GetDirectories(dotnetdir)
                foreach var dir in dirs
                    aFolders:Add(dir+"\Current")
                next
            endif
        endif
    endif
    local cAppDir as string
    cAppDir := System.Environment.GetFolderPath(System.Environment.SpecialFolder.LocalApplicationData)
    cAppDir := System.IO.Path.Combine(cAppDir, "Microsoft\MSBuild\Current")
    aFolders:Add(cAppDir)

    if uninstall
        foreach var dir in aFolders
            var cPath := dir+"\Imports\Microsoft.Common.Props\ImportBefore"
            if Directory.Exists(cPath)
                IF File.Exists(Path.Combine(cPath, xsfile))
                    Console.WriteLine("Removing from "+ cPath)
                    File.Delete(Path.Combine(cPath, xsfile))
                ENDIF
                RemoveTree(cPath)
            endif
            cPath := cPath:Replace("Imports\Microsoft.Common.Props\ImportBefore\","")
        next
    else
        if !Directory.Exists(cAppDir)
            Directory.CreateDirectory(cAppDir)
        endif
        cAppDir := Path.Combine(cAppDir, "Imports\Microsoft.Common.Props\ImportBefore")
        if !Directory.Exists(cAppDir)
            Directory.CreateDirectory(cAppDir)
        endif
        System.IO.File.Copy(sourceFile, Path.Combine(cAppDir, xsfile), true)
        Console.WriteLine("Copying to "+ cAppDir)
    endif
#ifdef DEBUG
    Console.WriteLine("Press Enter")
    Console.ReadLine()
#endif

function IsAdministrator() as logic
    var identity := WindowsIdentity.GetCurrent()
    var principal := WindowsPrincipal{identity}
    return principal.IsInRole(WindowsBuiltInRole.Administrator)

function GetSettingString<T>(name as string,  result out T, def as T) as logic
    var key := Microsoft.Win32.Registry.LocalMachine
    result := def
    var subkey := key:OpenSubKey(XSharp.Constants.RegistryKey64, true)
    if subkey == null
        subkey := key.CreateSubKey(XSharp.Constants.RegistryKey64, true)
    endif
    var oResult := subkey:GetValue(name)
    if oResult == null
        return false
    endif
    result := (T) oResult
    return true

function RemoveTree(path as string) as void
    if path:EndsWith("\")
        path := path:Substring(0, Path:Length-1)
    endif
    var files := Directory.GetFiles(path)
    var dirs  := Directory.GetDirectories(path)
    do while files:Length == 0 .and. dirs:Length == 0
        Directory.Delete(path)
        path := Path:Substring(0, path:LastIndexOf("\"))
        files := Directory.GetFiles(path,"*.*")
        dirs  := Directory.GetDirectories(path)
    enddo
    return


Function GetDotNetDir(lx64 as logic) as string
    var key := Microsoft.Win32.Registry.LocalMachine
    try
        if IntPtr.Size == 8
            if lx64
                key := key:OpenSubKey("SOFTWARE\WOW6432Node\dotnet\Setup\InstalledVersions\x64")
            else
                key := key:OpenSubKey("SOFTWARE\WOW6432Node\dotnet\Setup\InstalledVersions\x86")
            endif
        else
            if lx64
                key := key:OpenSubKey("SOFTWARE\dotnet\Setup\InstalledVersions\x64")
            else
                key := key:OpenSubKey("SOFTWARE\dotnet\Setup\InstalledVersions\x86")
            endif
        endif
        return (string) key:GetValue("InstallLocation")
    catch e as Exception
        Console.WriteLine(e:Message)
    end try
    return ""

